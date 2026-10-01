
# re use the cleaning code already in the CS repo
cs_dir <- "../contrast_sensitivity_analysis"
source(file.path(cs_dir, "cleaning_ymaze_data.R"), chdir = TRUE)
cs <- clean_ymazes(file.path(cs_dir, "data/contrast_experiment.xlsx"))

# clean SR data 
source("scripts/loading_cleaning.R")

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


# recalculating pattern frequencies ####

# first, simply add a column called viewing distance instead of position 
decisions <- decisions %>%
  mutate(view_dist = case_when(
    position_ret == "1" ~ 50,
    position_ret == "2" ~ 180,
    # I'm assuming this distance for 2.5. I haven't found where i wrote this
    # down yet. (Could also be approximated from films, not done yet).
    position_ret == "2.5" ~ 250,
    position_ret == "3" ~ 312,
    TRUE ~ as.numeric(position_ret)  # Keep the original value if no match
  ))
# second, the pattern period (mm) is the cycle width recorded in the sheet
decisions <- decisions %>% mutate(patt_period = as.numeric(cycle_width_ret))

# then, make calculation for new column, patt_freq_ret
source("functions/calculate_spatial_frequency.R")

decisions <- decisions %>%
  mutate(patt_freq_ret = calculate_cycles_per_degree(patt_period, view_dist))