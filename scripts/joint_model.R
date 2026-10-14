# Joint model of the spatial resolution (SR) and contrast sensitivity (CS) y maze
# data, using the predictors of the two separate models together:
# The outcome is first_decision, the only one recorded in both experiments.
library(psyphy)

# trial selection: each experiment contributes the trials its own model used ####
# SR: nests with enough testing, no training trials, pattern at position 2, after
# the learning period (as in analysis_stimuli_at_pos2.R). The V / H suffixes of
# the nest names used there now live in home_orient.
sr_nests <- tribble(
  ~nest,    ~home_orient,
  "sr_1.4", "vertical",
  "sr_1.5", "horizontal",
  "sr_2.6", "vertical",
  "sr_2.8", "vertical",
  "sr_2.9", "horizontal"
)
sr_trials <- joint_model %>%
  filter(experiment == "sr") %>%
  semi_join(sr_nests, by = c("nest", "home_orient")) %>%
  filter(training_or_testing != "training" | is.na(training_or_testing),
         position_ret == 2,
         date > as.Date("2024-04-16"))

# CS: all cleaned trials (as in contrast_sensitivity_model.R)
# it might be cleaner to use and explain only the 90% contrasts from the 2025 data
cs_trials <- joint_model %>% filter(experiment == "cs")

model_data <- bind_rows(sr_trials, cs_trials)

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

# make prediction dataframe with key light levels
# honestly instead of cs values should just have log unit change values that cover their reasonable range of their departures
library(marginaleffects)
type_light_value <- c(rep("range", 100), "-2sd", "-1sd", "mean","1sd", "2sd")
light_values <- c(
  seq(min(model_data$light_pred), max(model_data$light_pred), length.out = 100),
  mean(model_data$light_pred) + sd(model_data$light_pred)*c(-2,-1,0,1,2))
spFreq_range <- seq(min(model_data$patt_freq_ret), max(model_data$patt_freq_ret), length.out = 100)
newdata <- datagrid(
  model = fit_2afc, contrast_ret = 0.9, patt_freq_ret  = spFreq_range,
  light_pred = light_values, type_light_value = type_light_value)      
newdata <- broom::augment(fit_2afc, newdata = newdata, type.predict = "link", se_fit = T)        

# inverse prediction / calibration
inv_prediction <- function(betas, eta, light_values, contrast) {
  (eta - betas["(Intercept)"] - betas["light_pred"] * light_values -
     betas["log10(contrast_ret)"] * log10(contrast)) / betas["patt_freq_ret"]
}

p_thresh <- 0.7
eta_thresh <- fit_2afc$family$linkfun(p_thresh)
betas <- coef(fit_2afc)
C <- 0.9 
v_cutoff <- inv_prediction(
  betas = betas, eta = eta_thresh, contrast = C,
  light_values = unique(newdata$light_values))

# generate confidence intervals for inverse prediction
beta_samples <- MASS::mvrnorm(100000, coef(fit_2afc), vcov(fit_2afc))
v_cutoff_samples <- apply(
  beta_samples, 1, function(bs)
    inv_prediction(bs, eta_thresh, unique(newdata$light_values), C))
# combine the light range, vcutoff for the light range, and the 95% CI of vcutoff
pred <- tibble(light_pred = unique(newdata$light_values),
               v_cutoff = v_cutoff,
               lwr = apply(v_cutoff_samples, 1, quantile, 0.025),
               upr = apply(v_cutoff_samples, 1, quantile, 0.975))

