# Figures for the joint model, after the two logistic-regression figures at the
# end of analysis_stimuli_at_pos2.R. Needs fit_2afc, model_data, pred_curves,
# light_sd, p_thresh and C from joint_model.R, and the helpers in
# functions/joint_model_functions.R.
library(scico)

# trials from both experiments at 90 % contrast: all SR trials and the CS
# trials that match the contrast the curves are drawn at
plot_data <- model_data %>% filter(near(contrast_ret, C))

experiment_shapes <- c("sr" = 21, "cs" = 22)
freq_breaks <- seq(0.02, 0.16, 0.02)  # tested frequencies too close to label each

# light levels: 1 log unit steps across the plotted data plus the mean
light_lvls <- light_levels(plot_data, "log_steps")

# frequency grid spanning the tested frequencies
freq_grid <- seq(floor(min(plot_data$patt_freq_ret) * 1000) / 1000,
                 ceiling(max(plot_data$patt_freq_ret) * 1000) / 1000,
                 by = 0.0005)

# fig 0: curves at the mean and +- 1, 2 sd light levels, observed proportions ####
# model plot - this needs to be adapted - taken from cs code
plot_sd_curves <- function(curves, data) {
  ggplot() +
    geom_line(data = curves,
              aes(x = patt_freq_ret, y = p, colour = light_pred,
                  group = light_pred)) +
    geom_point(data = observed_props(data),
               aes(x = patt_freq_ret, y = prop_correct, size = n,
                   shape = experiment)) +
    scale_x_continuous(breaks = freq_breaks) +
    scale_shape_manual(values = experiment_shapes) +
    labs(x = "Pattern frequency (cycles per degree)",
         y = "Proportion of choices for nest",
         shape = "Experiment") +
    scale_colour_scico(
      palette = "bamako",
      name   =  expression(atop("Illuminance", "(" * log[10] ~ "lx)")),
      breaks = unique(curves$light_pred),
      labels = scales::label_number(accuracy = 0.1),
      guide  = guide_legend(reverse = TRUE,
                            theme = theme(legend.key.spacing.y = unit(-8, "pt")))) +
    scale_size(name = expression(italic(n)[choices])) +   # keeps default spacing
    theme_bw(base_size = 8)
}

# fig 1: max detectable frequency as a function of light level ####
plot_cutoff <- function(fit, light, p, contrast) {
  curve <- cutoff_table(fit, seq(min(light), max(light), length.out = 100),
                        p, contrast, n_sim = 5000)
  pts <- tibble(light_pred = sort(light[-length(light)]),
                cutoff = cutoff_freq(fit, light_pred, p, contrast))
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
plot_curves <- function(fit, data, light, p, contrast) {
  curves <- prediction_grid(light, freq_grid, contrast) %>%
    predict_band(fit, .) %>%
    mutate(light_lvl = light_factor(light_pred, light)) %>%
    filter(p > .env$p)
  vlines <- tibble(light_pred = light,
                   cutoff = cutoff_freq(fit, light, p, contrast),
                   light_lvl = light_factor(light_pred, light))

  set.seed(8)
  ggplot(data, aes(patt_freq_ret, first_decision)) +
    geom_jitter(aes(shape = experiment), height = 0.04, width = 0.003,
                alpha = 0.5, colour = "black") +
    geom_line(data = curves, aes(y = p, colour = light_lvl),
              linewidth = 0.8) +
    geom_hline(yintercept = p, lty = 2) +
    geom_segment(data = vlines, aes(x = cutoff, xend = cutoff,
                                    y = 0, yend = p, colour = light_lvl),
                 linetype = "dashed", linewidth = 0.8) +
    scale_x_continuous(breaks = freq_breaks) +
    scale_colour_manual(values = light_colours(light)) +
    scale_shape_manual(values = experiment_shapes) +
    labs(x = "Pattern frequency (cycles per degree)",
         y = "Proportion of choices for home",
         shape = "Experiment",
         colour = expression(atop("Returning light level",
                                  ~ "(" ~ log[10] ~ "lux" ~ ")"))) +
    theme_bw() +
    theme(legend.justification = c("right", "top"))
}

# fig 3: choice probability against frequency at the mean light level ####
# one point per tested frequency and experiment (observed proportion, sized by
# n choices) under the model curve. The points pool trials across light levels;
# the curve is at the mean.
plot_mean_light <- function(fit, data, light, p, contrast) {
  mean_light <- mean(data$light_pred)
  curve <- prediction_grid(mean_light, freq_grid, contrast) %>%
    predict_band(fit, .)
  cutoff <- cutoff_freq(fit, mean_light, p, contrast)

  # same colour the mean level gets in fig 2
  colour <- light_colours(light)[[as.character(round(mean_light, 1))]]

  ggplot(curve, aes(patt_freq_ret, p)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill = colour, alpha = 0.2) +
    geom_line(colour = colour, linewidth = 0.8) +
    geom_hline(yintercept = p, lty = 2) +
    annotate("segment", x = cutoff, xend = cutoff, y = 0, yend = p,
             colour = colour, linetype = "dashed", linewidth = 0.8) +
    # dodge so the two experiments don't overlap at shared frequencies
    geom_point(data = observed_props(data),
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

plot_sd_curves(pred_curves, plot_data)
plot_cutoff(fit_2afc, light_lvls, p = p_thresh, contrast = C)
plot_curves(fit_2afc, plot_data, light_lvls, p = p_thresh, contrast = C)
plot_mean_light(fit_2afc, plot_data, light_lvls, p = p_thresh, contrast = C)
