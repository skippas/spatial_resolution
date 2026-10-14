# Orchestrator. Run from the project root.
#   load + clean both experiments -> join -> joint model

library(tidyverse)
source("functions/drop_unused.R")

# load and clean both experiments (leaves `sr` and `cs`)
source("scripts/loading_cleaning.R")
source("scripts/load_cs_data.R")

source("scripts/join_2024-25_datasets.R")
# trials usable in the joint model: a decision, a spatial frequency and a light level
# need to check analysis at pos2 code to see what rows I was excluding / including there. 
# I need to go through data and have a solid training / testing column for excluding / including data.
# But that's long term. 
joint_model <- joint %>%
  filter(!is.na(first_decision), !is.na(patt_freq_ret), !is.na(light_pred))
# joint model ####
source("scripts/joint_model.R")

source("scripts/descriptive_plots.R")

source("scripts/joint_model_plots.R")
