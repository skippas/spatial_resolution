# Exploratory: the CS and joint SR models with a light interaction.
# Refits both models with light x log contrast and light x frequency, and draws
# the paper's four-panel figure (curves over thresholds) from those fits, with
# the additive fits' thresholds dashed for comparison. Not part of the paper
# pipeline: writes only output/interaction_models.pdf.
# Run from spatial_resolution/: Rscript scripts/misc_scripts/interaction_models.R

suppressMessages({
  library(tidyverse)
  library(psyphy)
  library(patchwork)
})
set.seed(1)
source("../shared/theme_paper.R")
source("functions/drop_unused.R")
source("functions/joint_model_functions.R")

# data, as in run_analysis.R ####
source("scripts/loading_cleaning.R")
source("scripts/load_cs_data.R")
source("scripts/join_2024-25_datasets.R")
joint_model <- joint %>%
  filter(!is.na(first_decision), !is.na(patt_freq_ret), !is.na(light_pred))

p_thresh <- 0.65
contrast_pred <- 0.9
sr_data <- select_model_trials(joint_model) %>% filter(contrast_ret == contrast_pred)
cs_data <- cs   # all CS trials, as in contrast_sensitivity_model.R (ymazes)

link <- binomial(mafc.logit(2))
eta_thresh <- link$linkfun(p_thresh)

# models ####
cs_add <- glm(first_landing ~ log10(contrast_ret) + light_pred,
              family = link, data = cs_data, start = c(0, 0, 0))
cs_int <- glm(first_landing ~ log10(contrast_ret) * light_pred,
              family = link, data = cs_data, start = c(0, 0, 0, 0))
sr_add <- glm(first_decision ~ light_pred + patt_freq_ret,
              family = link, data = sr_data, start = c(0, 0, 0))
sr_int <- glm(first_decision ~ light_pred * patt_freq_ret,
              family = link, data = sr_data, start = c(0, 0, 0, 0))

cat("n CS =", nrow(cs_data), " n SR =", nrow(sr_data), "\n")
cat("\n== CS interaction model ==\n"); print(summary(cs_int)$coefficients)
print(anova(cs_add, cs_int, test = "Chisq")); cat("AIC", AIC(cs_add), AIC(cs_int), "\n")
cat("\n== SR interaction model ==\n"); print(summary(sr_int)$coefficients)
print(anova(sr_add, sr_int, test = "Chisq")); cat("AIC", AIC(sr_add), AIC(sr_int), "\n")

# thresholds (inverse prediction) ####
# with the interaction the threshold is a ratio whose denominator changes with
# light: x* = (eta - b0 - bL * L) / (bX + bXL * L)
thresh <- function(b, light, x_name, int_name) {
  bxl <- if (is.na(int_name)) 0 else b[int_name]
  unname((eta_thresh - b["(Intercept)"] - b["light_pred"] * light) /
           (b[x_name] + bxl * light))
}
thresh_table <- function(fit, light, x_name, int_name, n_sim = 1e5) {
  sims <- MASS::mvrnorm(n_sim, coef(fit), vcov(fit)) %>%
    apply(1, function(b) thresh(b, light, x_name, int_name))
  tibble(light_pred = light,
         est = thresh(coef(fit), light, x_name, int_name),
         lwr = apply(sims, 1, quantile, 0.025),
         upr = apply(sims, 1, quantile, 0.975))
}

light_range <- seq(-3.5, 1.7, length.out = 100)
cs_x <- "log10(contrast_ret)"; cs_xl <- "log10(contrast_ret):light_pred"
sr_x <- "patt_freq_ret";       sr_xl <- "light_pred:patt_freq_ret"

# with the interaction the contrast slope (b_C + b_CL * L) reaches zero at
# bright light; past that no contrast reaches criterion, so the threshold is
# undefined (the curve jumps from +Inf to -Inf). Draw it only below that light.
cs_slope_zero <- -coef(cs_int)[cs_x] / coef(cs_int)[cs_xl]
cat("\nCS contrast slope reaches zero at", round(cs_slope_zero, 2), "log lx\n")
cs_thr_int <- thresh_table(cs_int, light_range, cs_x, cs_xl) %>%
  filter(light_pred < cs_slope_zero - 0.25)
cs_thr_add <- thresh_table(cs_add, light_range, cs_x, NA)
sr_thr_int <- thresh_table(sr_int, light_range, sr_x, sr_xl)
sr_thr_add <- thresh_table(sr_add, light_range, sr_x, NA)

cat("\nCS log sensitivity at light_breaks (interaction):\n")
print(thresh_table(cs_int, light_breaks, cs_x, cs_xl) %>%
        mutate(across(-light_pred, ~ -.x)) %>% rename(sens = est, upr = lwr, lwr = upr))
cat("\nSR cutoff at light_breaks (interaction):\n")
print(thresh_table(sr_int, light_breaks, sr_x, sr_xl))

# panel A: performance against contrast ####
contrast_grid <- seq(min(cs_data$contrast_ret), max(cs_data$contrast_ret), length.out = 100)
cs_curves <- expand_grid(light_pred = light_breaks, contrast_ret = contrast_grid) %>%
  mutate(p = predict(cs_int, newdata = pick(everything()), type = "response"))
cs_obs <- cs_data %>%
  group_by(contrast_ret) %>%
  summarise(p = mean(first_landing), n = n(), .groups = "drop")

pA <- ggplot() +
  geom_line(data = cs_curves,
            aes(contrast_ret, p, colour = light_pred, group = light_pred)) +
  geom_point(data = cs_obs, aes(contrast_ret, p, size = n), shape = 21) +
  scale_x_continuous(breaks = seq(0, 1, 0.2), labels = scales::label_percent()) +
  labs(x = "Grating contrast (%)") +
  scale_colour_light() +
  scale_size_n()

# panel B: performance against frequency ####
freq_grid <- seq(min(sr_data$patt_freq_ret), max(sr_data$patt_freq_ret), length.out = 100)
sr_curves <- prediction_grid(light_breaks, freq_grid) %>% predict_band(sr_int, .)
sr_obs <- sr_data %>%
  mutate(year = factor(lubridate::year(date))) %>%
  observed_props()

pB <- ggplot() +
  geom_line(data = sr_curves,
            aes(patt_freq_ret, p, colour = light_pred, group = light_pred)) +
  geom_point(data = sr_obs,
             aes(patt_freq_ret, prop_correct, size = n, shape = year)) +
  scale_x_continuous(breaks = seq(0.02, 0.16, 0.02)) +
  labs(x = "Pattern frequency (cycles per degree)") +
  scale_shape_year() +
  scale_colour_light() +
  scale_size_n()

curves <- (pA | pB) +
  plot_layout(guides = "collect", axis_titles = "collect") &
  geom_hline(yintercept = p_thresh, linetype = "dashed") &
  coord_cartesian(ylim = c(0.2, 1)) &
  scale_y_continuous(name = "Proportion of choices for nest",
                     breaks = seq(0, 1, 0.1),
                     labels = scales::label_number(accuracy = 0.1)) &
  guides(colour = guide_legend(reverse = FALSE)) &
  theme_paper &
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.title.position = "top")

# panels C, D: thresholds, interaction (solid, band) vs additive (dashed) ####
fit_lines <- function(int, add) {
  bind_rows(mutate(int, fit = "With interaction"),
            mutate(add, fit = "Additive (paper)"))
}
inset_legend <- theme(legend.position = "inside",
                      legend.position.inside = c(0.02, 0.98),
                      legend.justification = c(0, 1),
                      legend.background = element_rect(fill = "white", colour = "grey70",
                                                       linewidth = 0.3),
                      legend.margin = margin(2, 3, 2, 3),
                      legend.key.height = unit(3, "mm"),
                      legend.key.width = unit(6, "mm"))
fit_linetypes <- scale_linetype_manual(
  name = NULL,
  values = c("With interaction" = "solid", "Additive (paper)" = "dashed",
             "Slope = 0.5 (Rose-de Vries)" = "dotted"),
  breaks = c("With interaction", "Additive (paper)", "Slope = 0.5 (Rose-de Vries)"))

# sensitivity = -log10 threshold contrast; the band's ends swap
cs_lines <- fit_lines(cs_thr_int, cs_thr_add) %>% mutate(est = -est)
anchor <- filter(cs_thr_int, abs(light_pred - (-1)) == min(abs(light_pred - (-1))))
rose <- tibble(light_pred = range(light_range),
               est = -anchor$est + 0.5 * (light_pred - anchor$light_pred),
               fit = "Slope = 0.5 (Rose-de Vries)")
cs_pts <- tibble(light_pred = light_breaks[light_breaks < cs_slope_zero],
                 est = -thresh(coef(cs_int), light_pred, cs_x, cs_xl))

pC <- ggplot(cs_lines, aes(light_pred, est)) +
  geom_ribbon(data = cs_thr_int, aes(y = NULL, ymin = -upr, ymax = -lwr),
              fill = "grey70", alpha = 0.5) +
  geom_line(aes(linetype = fit)) +
  geom_line(data = rose, aes(linetype = fit), colour = "firebrick") +
  geom_vline(xintercept = cs_slope_zero, colour = "grey40", linewidth = 0.3) +
  annotate("text", x = cs_slope_zero, y = 0.1, angle = 90, vjust = -0.5, size = 2.2,
           colour = "grey30", label = "contrast slope = 0") +
  geom_point(data = cs_pts, aes(fill = light_pred), shape = 21, size = 2) +
  scale_colour_light(aesthetics = "fill") +
  fit_linetypes +
  scale_x_light() +
  coord_cartesian(ylim = c(-0.5, 3)) +
  labs(y = "Log contrast sensitivity") +
  guides(fill = "none") +
  theme_paper + inset_legend

sr_pts <- tibble(light_pred = light_breaks,
                 est = thresh(coef(sr_int), light_breaks, sr_x, sr_xl))
pD <- ggplot(fit_lines(sr_thr_int, sr_thr_add), aes(light_pred, est)) +
  geom_ribbon(data = sr_thr_int, aes(y = NULL, ymin = lwr, ymax = upr),
              fill = "grey70", alpha = 0.5) +
  geom_line(aes(linetype = fit)) +
  geom_point(data = sr_pts, aes(fill = light_pred), shape = 21, size = 2) +
  scale_colour_light(aesthetics = "fill") +
  fit_linetypes +
  scale_x_light() +
  coord_cartesian(ylim = c(-0.1, 0.25)) +
  labs(y = "Maximum detectable frequency (cycles/°)") +
  guides(fill = "none") +
  theme_paper + inset_legend

fig <- (curves / (pC | pD)) +
  plot_layout(heights = c(1.3, 1)) +
  plot_annotation(tag_levels = "A",
                  title = "Exploratory: models with a light interaction",
                  theme = theme(plot.title = element_text(size = 9)))

dir.create("output", showWarnings = FALSE)
ggsave("output/interaction_models.pdf", fig, device = cairo_pdf,
       width = 174, height = 180, units = "mm")
ggsave("output/interaction_models.png", fig, width = 174, height = 180,
       units = "mm", dpi = 200)
