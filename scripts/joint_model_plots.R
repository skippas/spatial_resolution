# Figures for the joint model, after the two logistic-regression figures at the
# end of analysis_stimuli_at_pos2.R. Needs fit_logit / fit_2afc and model_data
# from joint_model.R.

p_thresh <- 0.7   # performance that defines the cutoff frequency
C <- 0.9           # predictions are at 90 % contrast

# trials from both experiments at 90 % contrast: all SR trials and the CS
# trials that match the contrast the curves are drawn at
plot_data <- model_data %>%
  filter(contrast_ret == 90) 
experiment_shapes <- c("Spatial resolution" = 21, "Contrast sensitivity" = 22)
freq_breaks <- seq(0.02, 0.16, 0.02)  # tested frequencies too close to label each

# light levels: 1 log unit steps across the plotted data plus the mean, as before
light_lvls <- c(seq(min(plot_data$light_pred), max(plot_data$light_pred), by = 1),
                mean(plot_data$light_pred))

# frequency at which predicted performance equals p, given light and contrast
inv_prediction <- function(betas, eta, light_grid, contrast) {
  unname((eta - betas["(Intercept)"] - betas["light_pred"] * light_grid -
            betas["log10(contrast)"] * log10(contrast)) / betas["patt_freq_ret"])
}

# cutoff frequency across light, with a 95 % interval from simulated coefficients
cutoff_curve <- function(fit, p = p_thresh, contrast = C, n_sim = 5000) {
  eta <- fit$family$linkfun(p)
  light_grid <- seq(min(light_lvls), max(light_lvls), length.out = 100)
  sims <- MASS::mvrnorm(n_sim, coef(fit), vcov(fit)) %>%
    apply(1, function(b) inv_prediction(b, eta, light_grid, contrast))
  tibble(light_pred = light_grid,
         cutoff = inv_prediction(coef(fit), eta, light_grid, contrast),
         lwr = apply(sims, 1, quantile, 0.025),
         upr = apply(sims, 1, quantile, 0.975))
}

# frequency grid spanning the tested frequencies
freq_grid <- seq(floor(min(plot_data$patt_freq_ret) * 1000) / 1000,
                 ceiling(max(plot_data$patt_freq_ret) * 1000) / 1000,
                 by = 0.0005)

# fig 1: max detectable frequency as a function of light level ####
plot_cutoff <- function(fit, p = p_thresh) {
  curve <- cutoff_curve(fit, p)
  pts <- tibble(light_pred = sort(light_lvls[-length(light_lvls)]),
                cutoff = inv_prediction(coef(fit), fit$family$linkfun(p),
                                        light_pred, C))
  ggplot(curve, aes(light_pred, cutoff, colour = light_pred)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), colour = NA, fill = "white",
                alpha = 0.2) +
    geom_line(linewidth = 1.2) +
    geom_point(data = pts, aes(fill = light_pred), shape = 21, size = 2,
               colour = "black") +
    scale_colour_viridis_c(option = "D") +
    scale_fill_viridis_c(option = "D") +
    labs(x = expression("Returning light level" ~ "(" ~ log[10] ~ "lux" ~ ")"),
         y = paste0("Max detectable freq. at P = ", p, "\n(cycles per degree)")) +
    theme_dark() +
    theme(legend.position = "none")
}

# fig 2: choice probability against frequency, cutoffs drawn in ####
plot_curves <- function(fit, p = p_thresh) {
  lvl_labels <- sort(round(light_lvls, 1), decreasing = TRUE)
  as_lvl <- function(x) factor(round(x, 1), levels = lvl_labels)

  curves <- expand_grid(light_pred = light_lvls, patt_freq_ret = freq_grid) %>%
    mutate(contrast = C,
           probability = predict(fit, newdata = pick(everything()),
                                 type = "response"),
           light_lvl = as_lvl(light_pred)) %>%
    filter(probability > p)
  vlines <- tibble(light_pred = light_lvls,
                   patt_freq_ret = inv_prediction(coef(fit),
                                                  fit$family$linkfun(p),
                                                  light_lvls, C),
                   light_lvl = as_lvl(light_pred))
  colours <- setNames(viridisLite::viridis(length(lvl_labels), direction = -1),
                      lvl_labels)

  set.seed(8)
  ggplot(plot_data, aes(patt_freq_ret, first_decision)) +
    geom_jitter(aes(shape = experiment), height = 0.04, width = 0.003,
                alpha = 0.5, colour = "black") +
    geom_line(data = curves, aes(y = probability, colour = light_lvl),
              linewidth = 0.8) +
    geom_hline(yintercept = p, lty = 2) +
    geom_segment(data = vlines, aes(x = patt_freq_ret, xend = patt_freq_ret,
                                    y = 0, yend = p, colour = light_lvl),
                 linetype = "dashed", linewidth = 0.8) +
    scale_x_continuous(breaks = freq_breaks) +
    scale_colour_manual(values = colours) +
    scale_shape_manual(values = experiment_shapes) +
    labs(x = "Pattern frequency (cycles per degree)",
         y = "Proportion of choices for home",
         shape = "Experiment",
         colour = expression(atop("Returning light level",
                                  ~ "(" ~ log[10] ~ "lux" ~ ")"))) +
    theme_bw() +
    theme(
          legend.justification = c("right", "top"))
}

# fig 3: choice probability against frequency at the mean light level ####
# one point per tested frequency and experiment (observed proportion, sized by
# n choices) under the model curve. The points pool trials across light levels;
# the curve is at the mean.
plot_mean_light <- function(fit, p = p_thresh) {
  mean_light <- mean(plot_data$light_pred)

  # 95 % band on the link scale, back-transformed so it respects the 0.5 floor
  newdata <- tibble(light_pred = mean_light, patt_freq_ret = freq_grid,
                    contrast = C)
  pred <- predict(fit, newdata = newdata, type = "link", se.fit = TRUE)
  curve <- newdata %>%
    mutate(probability = fit$family$linkinv(pred$fit),
           lower = fit$family$linkinv(pred$fit - 1.96 * pred$se.fit),
           upper = fit$family$linkinv(pred$fit + 1.96 * pred$se.fit))

  observed <- plot_data %>%
    group_by(experiment, patt_freq_ret) %>%
    summarise(prop_correct = mean(first_decision), n = n(), .groups = "drop")

  cutoff <- inv_prediction(coef(fit), fit$family$linkfun(p), mean_light, C)

  # same colour the mean level gets in fig 2
  lvl_labels <- sort(round(light_lvls, 1), decreasing = TRUE)
  colour <- viridisLite::viridis(length(lvl_labels), direction = -1)[
    match(round(mean_light, 1), lvl_labels)]

  ggplot(curve, aes(patt_freq_ret, probability)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = colour, alpha = 0.2) +
    geom_line(colour = colour, linewidth = 0.8) +
    geom_hline(yintercept = p, lty = 2) +
    annotate("segment", x = cutoff, xend = cutoff, y = 0, yend = p,
             colour = colour, linetype = "dashed", linewidth = 0.8) +
    # dodge so the two experiments don't overlap at shared frequencies
    geom_point(data = observed,
               aes(y = prop_correct, size = n, shape = experiment),
               position = position_dodge(width = 0.004),
               fill = "white", colour = "black") +
    scale_size_area(max_size = 8) +
    scale_shape_manual(values = experiment_shapes) +
    scale_x_continuous(breaks = freq_breaks) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = "Pattern frequency (cycles per degree)",
         y = "Proportion of choices for home",
         size = "Choices",
         shape = "Experiment",
         title = bquote("Mean returning light level" ~
                          "(" * .(round(mean_light, 1)) ~ log[10] ~ "lux)")) +
    guides(shape = guide_legend(override.aes = list(size = 3))) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.box = "vertical",
          plot.title = element_text(hjust = 0.5))
}

plot_cutoff(fit_2afc)
plot_curves(fit_2afc)
plot_mean_light(fit_2afc)
