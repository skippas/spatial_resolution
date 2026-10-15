# Helpers for the joint SR + CS model (scripts/joint_model.R) and its figures
# (scripts/joint_model_plots.R). Each calculation lives here once, so the
# scripts only choose settings and call these.

# trial selection ####
# Each experiment contributes the trials its own model used.
# SR: nests with enough testing, no training trials, pattern at position 2,
# after the learning period (as in analysis_stimuli_at_pos2.R, where these nests
# were 1.4V, 1.5H, 2.6V, 2.8V and 2.9H).
# CS: all cleaned trials (as in contrast_sensitivity_model.R).
select_model_trials <- function(joint_model) {
  sr_nests <- c("4.1.24", "5.1.24", "6.2.24", "9.2.24", "11.2.24")
  sr_trials <- joint_model %>%
    filter(experiment == "sr",
           nest %in% sr_nests,
           training_or_testing != "training" | is.na(training_or_testing),
           position_ret == 2,
           date > as.Date("2024-04-16"))
  cs_trials <- joint_model %>% filter(experiment == "cs")
  bind_rows(sr_trials, cs_trials)
}

# light levels to draw curves at ####
# "sd": the mean and +- 1 and 2 sd
# "log_steps": 1 log unit steps from the minimum, plus the mean
light_levels <- function(data, type = c("mean_sd", "log_steps")) {
  type <- match.arg(type)
  light <- data$light_pred
  switch(type,
         mean_sd = mean(light) + sd(light) * c(-2, -1, 0, 1, 2),
         log_steps = c(seq(min(light), max(light), by = 1), mean(light)))
}

# predictions ####
# every combination of light level and frequency, at one contrast
prediction_grid <- function(light, freq, contrast) {
  expand_grid(light_pred = light, patt_freq_ret = freq, contrast_ret = contrast)
}

# predicted probability with a confidence band. The band is built on the link
# scale and both ends back-transformed, so it stays inside the 0.5 floor of the
# 2afc link.
predict_band <- function(fit, newdata, level = 0.95) {
  z <- qnorm(1 - (1 - level) / 2)
  pred <- predict(fit, newdata = newdata, type = "link", se.fit = TRUE)
  newdata %>%
    mutate(p = fit$family$linkinv(pred$fit),
           lwr = fit$family$linkinv(pred$fit - z * pred$se.fit),
           upr = fit$family$linkinv(pred$fit + z * pred$se.fit))
}

# cutoff frequency (inverse prediction) ####
# frequency at which the linear predictor equals eta, given light and contrast
inv_prediction <- function(betas, eta, light, contrast) {
  unname((eta - betas["(Intercept)"] - betas["light_pred"] * light -
            betas["log10(contrast_ret)"] * log10(contrast)) /
           betas["patt_freq_ret"])
}

# cutoff frequency at performance p for each light level
cutoff_freq <- function(fit, light, p, contrast) {
  inv_prediction(coef(fit), fit$family$linkfun(p), light, contrast)
}

# the same, with a confidence interval from coefficients simulated from the
# fit's covariance matrix
cutoff_table <- function(fit, light, p, contrast, n_sim = 10000,
                         level = 0.95) {
  eta <- fit$family$linkfun(p)
  sims <- MASS::mvrnorm(n_sim, coef(fit), vcov(fit)) %>%
    apply(1, function(b) inv_prediction(b, eta, light, contrast)) %>%
    matrix(nrow = length(light))  # keeps one row per light level when light is a single value
  tibble(light_pred = light,
         cutoff = cutoff_freq(fit, light, p, contrast),
         lwr = apply(sims, 1, quantile, (1 - level) / 2),
         upr = apply(sims, 1, quantile, 1 - (1 - level) / 2))
}

# observed data ####
# proportion of first decisions for home per experiment and tested frequency
# (year kept for the shape legend; each experiment ran in one year)
observed_props <- function(data) {
  data %>%
    group_by(experiment, year, patt_freq_ret) %>%
    summarise(prop_correct = mean(first_decision), n = n(), .groups = "drop")
}

# plot helpers ####
# factor of light levels rounded to 0.1, highest first, for a discrete legend
light_factor <- function(x, light) {
  factor(round(x, 1), levels = sort(round(light, 1), decreasing = TRUE))
}

# one viridis colour per light level, named as light_factor() labels them
light_colours <- function(light) {
  labels <- levels(light_factor(light, light))
  setNames(viridisLite::viridis(length(labels), direction = -1), labels)
}
