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
  filter(contrast_ret == contrast_pred) %>%
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
    scale_y_performance() +
    labs(x = "Pattern frequency (cycles per degree)") +
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

p <- plot_sd_curves(pred_curves, plot_data)
ggsave("../../writing/figures/contrast/performance_by_spFreq_and_light.pdf",
       device = cairo_pdf, plot = p, 
       width = 85, height = 75, units = "mm")
# plot object for the combined SR + CS figure (analyses/combine_figures.R)
dir.create("../../writing/figures/objects", showWarnings = FALSE)
saveRDS(p, "../../writing/figures/objects/sr_curves.rds")

preview(p, height = 85, width = 85)
