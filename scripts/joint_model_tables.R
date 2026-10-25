# Tables for the joint model, written as LaTeX fragments to writing/tables.
# Needs fit_2afc and p_thresh from joint_model.R and light_breaks from
# theme_paper.R.
library(broom)
source("../shared/latex_tables.R")  # table_dir, num(), write_tabular()

# coefficients of the 2AFC fit ####
# Wald estimates (the logit fit in joint_model.R is not reported)
coef_names <- c("(Intercept)"   = "Intercept",
                "light_pred"    = "Illuminance (log$_{10}$ lx)",
                "patt_freq_ret" = "Spatial frequency (cycles/$^\\circ$)")

tidy(fit_2afc) %>%
  transmute(Term = coef_names[term],
            Estimate = num(estimate, 2),
            SE = num(std.error, 2),
            "$z$" = num(statistic, 2),
            "$p$" = if_else(p.value < 0.001, "$<$0.001", num(p.value, 3))) %>%
  write_tabular("joint_model_coefs.tex", align = "lrrrr")

# cutoff frequency at the figure light levels ####
# frequency at which the 2AFC fit predicts p_thresh correct
# (kept as cutoffs so joint_model_numbers.R cites the same values)
cutoffs <- cutoff_table(fit_2afc, light_breaks, p = p_thresh, n_sim = 100000)
cutoffs %>%
  transmute("Illuminance (log$_{10}$ lx)" = num(light_pred, 0),
            "Cutoff (cycles/$^\\circ$)" = num(cutoff, 3),
            "95\\% CI" = paste0("[", num(lwr, 3), ", ", num(upr, 3), "]")) %>%
  write_tabular("joint_model_cutoffs.tex", align = "rrc")
