# Numbers cited in the text for the joint model, written as LaTeX macros to
# writing/tables/joint_model_numbers.tex. Cite them in the text by name, e.g.
# \jmNChoices or \jmCutoffZero. All names start with jm (joint model).
# Needs fit_2afc, model_data, p_thresh and contrast_pred from joint_model.R,
# cutoffs from joint_model_tables.R and light_breaks from theme_paper.R.
library(broom)
source("../shared/latex_tables.R")  # num(), p_text(), name_num(), write_numbers()

# data ####
sr_trials <- filter(model_data, experiment == "sr")
cs_trials <- filter(model_data, experiment == "cs")

data_numbers <- c(
  jmNChoices   = nrow(model_data),
  jmNChoicesSR = nrow(sr_trials),
  jmNChoicesCS = nrow(cs_trials),
  jmNNestsSR   = n_distinct(sr_trials$nest),
  jmNNestsCS   = n_distinct(cs_trials$nest),
  jmNBees      = NA,  # no bee ID column in the data yet
  jmLightMin   = num(min(model_data$light_pred), 1),
  jmLightMax   = num(max(model_data$light_pred), 1))

# settings ####
setting_numbers <- c(
  jmCriterion = num(p_thresh, 2),
  jmContrast  = paste0(num(contrast_pred * 100, 0), "\\%"))

# coefficients of the 2AFC fit ####
# e.g. \jmLightEst, \jmLightSE, \jmLightZ, \jmLightP ("$p = 0.010$")
coef_names <- c("(Intercept)"   = "jmIntercept",
                "light_pred"    = "jmLight",
                "patt_freq_ret" = "jmFreq")

coefs <- tidy(fit_2afc) %>% mutate(name = coef_names[term])
coef_numbers <- c(setNames(num(coefs$estimate, 2), paste0(coefs$name, "Est")),
                  setNames(num(coefs$std.error, 2), paste0(coefs$name, "SE")),
                  setNames(num(coefs$statistic, 2), paste0(coefs$name, "Z")),
                  setNames(p_text(coefs$p.value), paste0(coefs$name, "P")))

# cutoff frequency at the figure light levels ####
# the same values as joint_model_cutoffs.tex, e.g. \jmCutoffMinusOne,
# \jmCutoffMinusOneLwr, \jmCutoffMinusOneUpr
cutoff_names <- paste0("jmCutoff", name_num(cutoffs$light_pred))
cutoff_numbers <- c(setNames(num(cutoffs$cutoff, 3), cutoff_names),
                    setNames(num(cutoffs$lwr, 3), paste0(cutoff_names, "Lwr")),
                    setNames(num(cutoffs$upr, 3), paste0(cutoff_names, "Upr")))

c(data_numbers, setting_numbers, coef_numbers, cutoff_numbers) %>%
  write_numbers("joint_model_numbers.tex")
