# Joint model of the spatial resolution (SR) and contrast sensitivity (CS) y maze
# data, using the predictors of the two separate models together:
# The outcome is first_decision, the only one recorded in both experiments.
library(psyphy)
source("functions/joint_model_functions.R")

# trial selection: each experiment contributes the trials its own model used ####
# (nests and filters in select_model_trials())
# it might be cleaner to use and explain only the 90% contrasts from the 2025 CS data
model_data <- select_model_trials(joint_model)

# models ####
# logit link, as in the SR model
fit_logit <- glm(first_decision ~ light_pred + patt_freq_ret + log10(contrast_ret),
                 family = binomial, data = model_data)
summary(fit_logit)

# two-alternative link with a 50 % guessing floor, as in the CS model
fit_2afc <- glm(first_decision ~ light_pred + patt_freq_ret + log10(contrast_ret),
                family = binomial(mafc.logit(2)), data = model_data,
                start = c(0, 0, 0, 0))
summary(fit_2afc)

# settings shared by the predictions and the figures ####
p_thresh <- 0.7   # performance that defines the cutoff frequency
C <- 0.9          # predictions are at 90 % contrast

# predicted curves at key light levels ####
# honestly instead of cs values should just have log unit change values that cover their reasonable range of their departures
light_sd <- light_levels(model_data, "sd")
freq_range <- seq(min(model_data$patt_freq_ret), max(model_data$patt_freq_ret),
                  length.out = 100)
pred_curves <- prediction_grid(light_sd, freq_range, C) %>%
  predict_band(fit_2afc, .)

# cutoff frequency across the light range (inverse prediction / calibration) ####
light_range <- seq(min(model_data$light_pred), max(model_data$light_pred),
                   length.out = 100)
pred <- cutoff_table(fit_2afc, light_range, p = p_thresh, contrast = C,
                     n_sim = 100000)
