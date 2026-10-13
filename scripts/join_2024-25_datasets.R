# join 2024 and 2025 datasets (sr and cs datasets)
# join ####
# the columns the joint model needs; everything else is left behind, so a new
# column in either sheet can't slip into the joint data unnoticed
joint_cols <- c("ymaze", "nest", "flight", "date", "t_ret", "session",
                "home_orient", "side_home_ret", "position_ret", "view_dist",
                "cycle_width_ret", "patt_period", "patt_freq_ret",
                "contrast_ret", "first_decision", "light_pred",
                "training_or_testing")

standardise <- function(df, experiment) {
  df %>%
    rename(any_of(c(training_or_testing = "training_or_test"))) %>%
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