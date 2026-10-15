# Tables for the joint model, written as LaTeX fragments to writing/tables.
# Needs fit_logit, fit_2afc, p_thresh and contrast_pred from joint_model.R and
# light_breaks from theme_paper.R. Each file is a bare booktabs tabular (no
# float), so the caption and label stay in the manuscript:
#   \begin{table} \caption{...} \label{...} \input{tables/<file>.tex} \end{table}
# The manuscript needs \usepackage{booktabs}.
library(broom)

table_dir <- "../../writing/tables"
dir.create(table_dir, showWarnings = FALSE)

# fixed decimals with a proper minus sign
num <- function(x, digits) sub("^-", "$-$", formatC(x, digits = digits, format = "f"))

write_tabular <- function(data, file, align, linesep = "") {
  knitr::kable(data, format = "latex", booktabs = TRUE, escape = FALSE,
               align = align, linesep = linesep) %>%
    writeLines(file.path(table_dir, file))
}

# coefficients of the two fits ####
# Wald estimates, one block of rows per link function
coef_names <- c("(Intercept)"         = "Intercept",
                "light_pred"          = "Illuminance (log$_{10}$ lx)",
                "patt_freq_ret"       = "Spatial frequency (cycles/$^\\circ$)",
                "log10(contrast_ret)" = "Contrast (log$_{10}$)")

list("Logit" = fit_logit, "2AFC logit" = fit_2afc) %>%
  map(tidy) %>%
  bind_rows(.id = "Link") %>%
  transmute(Link = if_else(term == "(Intercept)", Link, ""),
            Term = coef_names[term],
            Estimate = num(estimate, 2),
            SE = num(std.error, 2),
            "$z$" = num(statistic, 2),
            "$p$" = if_else(p.value < 0.001, "$<$0.001", num(p.value, 3))) %>%
  write_tabular("joint_model_coefs.tex", align = "llrrrr",
                linesep = c("", "", "", "\\midrule"))  # rule between the two fits

# cutoff frequency at the figure light levels ####
# frequency at which the 2AFC fit predicts p_thresh correct, at contrast_pred
cutoff_table(fit_2afc, light_breaks, p = p_thresh, contrast = contrast_pred,
             n_sim = 100000) %>%
  transmute("Illuminance (log$_{10}$ lx)" = num(light_pred, 0),
            "Cutoff (cycles/$^\\circ$)" = num(cutoff, 3),
            "95\\% CI" = paste0("[", num(lwr, 3), ", ", num(upr, 3), "]")) %>%
  write_tabular("joint_model_cutoffs.tex", align = "rrc")
