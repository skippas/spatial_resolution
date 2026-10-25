# Joint model of the spatial resolution (SR) and contrast sensitivity (CS) y maze
# data: spatial frequency and light, on the 90 % contrast trials of both
# experiments. The outcome is first_decision, the only one recorded in both.
library(psyphy)
source("functions/joint_model_functions.R")

# settings shared by the model, the predictions and the figures ####
p_thresh <- 0.65  # performance that defines the cutoff frequency: half way between
                  # chance (0.5) and the ~0.8 the bees reach at best (lapses)
contrast_pred <- 0.9   # the one contrast modelled (all SR trials were at 90 %)

# trial selection ####
# each experiment contributes the trials its own model used (nests and filters
# in select_model_trials()), at 90 % contrast only, so the model needs no
# contrast term. Keeping the lower CS contrasts with a contrast term barely
# changed the frequency and light estimates or the cutoffs (checked 2026-10-25).
model_data <- select_model_trials(joint_model) %>%
  filter(contrast_ret == contrast_pred)

# models ####
# logit link, as in the SR model (not reported in the paper)
fit_logit <- glm(first_decision ~ light_pred + patt_freq_ret,
                 family = binomial, data = model_data)
summary(fit_logit)

# two-alternative link with a 50 % guessing floor, as in the CS model
fit_2afc <- glm(first_decision ~ light_pred + patt_freq_ret,
                family = binomial(mafc.logit(2)), data = model_data,
                start = c(0, 0, 0))
summary(fit_2afc)

# predicted curves at key light levels ####
# the round log lx steps shared with the CS figures (light_breaks, theme_paper.R)
source("../shared/theme_paper.R")
freq_range <- seq(min(model_data$patt_freq_ret), max(model_data$patt_freq_ret),
                  length.out = 100)
pred_curves <- prediction_grid(light_breaks, freq_range) %>%
  predict_band(fit_2afc, .)

# cutoff frequency across the light range (inverse prediction / calibration) ####
light_range <- seq(min(model_data$light_pred), max(model_data$light_pred),
                   length.out = 100)
pred <- cutoff_table(fit_2afc, light_range, p = p_thresh, n_sim = 100000)

# export for the Theobald et al. (2006) comparison in
# ../spatiotemporal_summation (scripts/overlay_behaviour.R)
dir.create("output", showWarnings = FALSE)
write.csv(pred, "output/cutoff_vs_light.csv", row.names = FALSE)
