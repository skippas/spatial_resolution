# Figures for the joint model, after the two logistic-regression figures at the
# end of analysis_stimuli_at_pos2.R. Needs fit_2afc, model_data, pred_curves,
# p_thresh and C from joint_model.R, and the helpers in
# functions/joint_model_functions.R.
library(scico)
source("../theme_paper.R")

# trials from both experiments at 90 % contrast: all SR trials and the CS
# trials that match the contrast the curves are drawn at
# year of testing for the shape legend (SR 2024, CS 2025)
plot_data <- model_data %>%
  filter(contrast_ret == 0.9) %>%
  mutate(year = factor(lubridate::year(date)))

freq_breaks <- seq(0.02, 0.16, 0.02)  # tested frequencies too close to label each

# light levels: 1 log unit steps across the plotted data plus the mean
light_lvls <- light_levels(plot_data, "log_steps")

# frequency grid spanning the tested frequencies
freq_grid <- seq(floor(min(plot_data$patt_freq_ret) * 1000) / 1000,
                 ceiling(max(plot_data$patt_freq_ret) * 1000) / 1000,
                 by = 0.0005)

# fig 0: curves at the shared light levels, observed proportions ####
# I want a dashed or dotted line denoting the criterion value for SR and for CS
# 3. Where is the histogram figure? 
# 4. save these (acuity as function light level, perf as function light level) and histogram figures to resolution directory
# add the schematic in.
# add the model tables to stats script and figure out how best to feed it to latex.
# 1. use patchwork to collect the legend of CS curve and SR curves

plot_sd_curves <- function(curves, data) {
  ggplot() +
    geom_line(data = curves,
              aes(x = patt_freq_ret, y = p, colour = light_pred,
                  group = light_pred)) +
    geom_point(data = observed_props(data),
               aes(x = patt_freq_ret, y = prop_correct, size = n,
                   shape = year)) +
    scale_x_continuous(breaks = freq_breaks) +
    scale_shape_year() +
    labs(x = "Pattern frequency (cycles per degree)",
         y = "Proportion of choices for nest") +
    scale_colour_light() +
    scale_size_n() +
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
    geom_jitter(aes(shape = year), height = 0.04, width = 0.003,
                alpha = 0.5, colour = "black") +
    geom_line(data = curves, aes(y = p, colour = light_lvl),
              linewidth = 0.8) +
    geom_hline(yintercept = p, lty = 2) +
    geom_segment(data = vlines, aes(x = cutoff, xend = cutoff,
                                    y = 0, yend = p, colour = light_lvl),
                 linetype = "dashed", linewidth = 0.8) +
    scale_x_continuous(breaks = freq_breaks) +
    scale_colour_manual(values = light_colours(light)) +
    scale_shape_year() +
    labs(x = "Pattern frequency (cycles per degree)",
         y = "Proportion of choices for home",
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
    # there are shared frequencies?! doubt dodgin necessary anyway as they wont overlap in y
    geom_point(data = observed_props(data),
               aes(y = prop_correct, size = n, shape = year),
               position = position_dodge(width = 0.004),
               fill = "white", colour = "black") +
    scale_size_area(max_size = 8) +
    scale_shape_year() +
    scale_x_continuous(breaks = freq_breaks) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = "Pattern frequency (cycles per degree)",
         y = "Proportion of choices for home",
         size = "Choices",
         title = bquote("Mean returning light level" ~
                          "(" * .(round(mean_light, 1)) ~ log[10] ~ "lux)")) +
    guides(shape = guide_legend(override.aes = list(size = 3))) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.box = "vertical",
          plot.title = element_text(hjust = 0.5))
}

p <- plot_sd_curves(pred_curves, plot_data)
ggsave("../../writing/figures/contrast/performance_by_spFreq_and_light.pdf",
       device = cairo_pdf, plot = p, 
       width = 85, height = 75, units = "mm")
# plot object for the combined SR + CS figure (analyses/combine_figures.R)
dir.create("../../writing/figures/objects", showWarnings = FALSE)
saveRDS(p, "../../writing/figures/objects/sr_curves.rds")

preview(p, height = 85, width = 85)
plot_curves(fit_2afc, plot_data, light_lvls, p = p_thresh, contrast = C)
plot_mean_light(fit_2afc, plot_data, light_lvls, p = p_thresh, contrast = C)
plot_cutoff(fit_2afc, light_lvls, p = p_thresh, contrast = C)
