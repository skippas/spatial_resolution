source("functions/calculate_spatial_frequency.R")
# contrast sensitivity data ####
# re use the cleaning code already in the CS repo
cs_dir <- "../contrast_sensitivity_analysis"
source(file.path(cs_dir, "functions/cleaning_ymaze_data.R"), chdir = TRUE)
cs <- clean_ymazes(file.path(cs_dir, "data/contrast_experiment.xlsx"))
cs <- add_spatial_frequency(cs)
