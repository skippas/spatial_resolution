library(tidyverse)
source("functions/calculate_spatial_frequency.R")

# re use the cleaning code already in the CS repo
cs_dir <- "../contrast_sensitivity_analysis"
source(file.path(cs_dir, "cleaning_ymaze_data.R"), chdir = TRUE)
cs <- clean_ymazes(file.path(cs_dir, "data/contrast_experiment.xlsx"))

# clean SR data (leaves `decisions`)
source("scripts/loading_cleaning.R")
sr <- decisions

drop_unused <- function(df, keep = character(), extra = character()) {
  baseline <- c(
    # spatial resolution model uses first decision, not landing:
    "first_landing",
    # video / recording bookkeeping
    "video_name", "t_video_start", "notes",
    # departure (outbound) info - models use the return only
    "side_home_dep", "t_dep", "ELTD", "position_dep", "cycle_width_dep", "contrast_dep",
    # elapsed / extended return times (not useful for analysis)
    "ELTR", "extended_return",
    # raw choice sequences
    "multiple_decisions", "side_choices", "position_choices_.landings.",
    # light-model internals (light_pred is kept)
    "time_rel_setrise", "light_pred_extrapolated",
    # others
    "loss_of_control", "gives_up", "learning_flight", "training_or_test"
  )
  df %>% select(-any_of(setdiff(c(baseline, extra), keep)))
}

# spatial frequency of the return pattern, computed the same way for both ####
sr <- add_spatial_frequency(sr)
cs <- add_spatial_frequency(cs)

# join ####
# the columns the joint model needs; everything else is left behind, so a new
# column in either sheet can't slip into the joint data unnoticed
joint_cols <- c("ymaze", "nest", "flight", "date", "t_ret", "session",
                "home_orient", "side_home_ret", "position_ret", "view_dist",
                "cycle_width_ret", "patt_period", "patt_freq_ret",
                "contrast_ret", "first_decision", "light_pred")

standardise <- function(df, experiment) {
  df %>%
    select(all_of(joint_cols)) %>%
    mutate(
      # nest ids repeat across experiments (e.g. 1.2, 2.1), so prefix them
      nest = if_else(is.na(nest), NA_character_,
                     paste0(experiment, "_", sprintf("%.1f", as.numeric(nest)))),
      side_home_ret = na_if(side_home_ret, "?")
    )
}

joint <- bind_rows(sr = standardise(sr, "sr"),
                   cs = standardise(cs, "cs"),
                   .id = "experiment")

# trials usable in the joint model: a decision, a spatial frequency and a light level
joint_model <- joint %>%
  filter(!is.na(first_decision), !is.na(patt_freq_ret), !is.na(light_pred))
