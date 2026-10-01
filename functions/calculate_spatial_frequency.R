calculate_cycles_per_degree <- function(patt_period, view_dist) {
  theta <- 2 * atan(patt_period / (2 * view_dist))
  theta_degrees <- theta * (180 / pi)
  cycles_per_degree <- 1 / theta_degrees
  cycles_per_degree <- round(cycles_per_degree, 3)
  return(cycles_per_degree)
}

# Adds view_dist (mm), patt_period (mm) and patt_freq_ret (cycles / degree) from
# the return position and the cycle width recorded in the sheet. Used for both
# the spatial resolution and the contrast sensitivity y maze data.
add_spatial_frequency <- function(df) {
  df %>%
    mutate(
      view_dist = case_when(
        position_ret == 1 ~ 50,
        position_ret == 2 ~ 180,
        # I'm assuming this distance for 2.5. I haven't found where i wrote this
        # down yet. (Could also be approximated from films, not done yet).
        position_ret == 2.5 ~ 250,
        position_ret == 3 ~ 312,
        TRUE ~ NA_real_
      ),
      patt_period = as.numeric(cycle_width_ret),
      patt_freq_ret = calculate_cycles_per_degree(patt_period, view_dist)
    )
}
