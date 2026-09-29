calculate_cycles_per_degree <- function(patt_period, view_dist) {
  theta <- 2 * atan(patt_period / (2 * view_dist))
  theta_degrees <- theta * (180 / pi)
  cycles_per_degree <- 1 / theta_degrees
  cycles_per_degree <- round(cycles_per_degree, 3)
  return(cycles_per_degree)
}