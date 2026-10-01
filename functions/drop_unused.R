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
