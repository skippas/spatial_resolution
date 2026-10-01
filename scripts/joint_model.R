# Joint model of the spatial resolution (SR) and contrast sensitivity (CS) y maze
# data, using the predictors of the two separate models together:
#   SR (analysis_stimuli_at_pos2.R):   first_decision ~ light_pred + patt_freq_ret
#   CS (contrast_sensitivity_model.R): first_landing ~ log10(contrast) + light_pred
# The outcome is first_decision, the only one recorded in both experiments.
# Run via scripts/run_analysis.R, which provides `joint_model`.

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
cs_trials <- joint_model %>% filter(experiment == "cs")

model_data <- bind_rows(sr_trials, cs_trials) %>%
  mutate(contrast = contrast_ret / 100)  # proportion, as in the CS model

# models ####
# logit link, as in the SR model
fit_logit <- glm(first_decision ~ light_pred + patt_freq_ret + log10(contrast),
                 family = binomial, data = model_data)
summary(fit_logit)

# two-alternative link with a 50 % guessing floor, as in the CS model
fit_2afc <- glm(first_decision ~ light_pred + patt_freq_ret + log10(contrast),
                family = binomial(mafc.logit(2)), data = model_data,
                start = c(0, 0, 0, 0))
summary(fit_2afc)
