#' Calculate time relative to sunrise or sunset
#'
#' @param decisions A data.frame containing 'date', 'session', and 't_ret' columns
#' @param sun_data A data.frame with 'date', 'rise', and 'set' datetime columns
#' @param timezone Timezone string (default = "EST")
#'
#' @return The same `decisions` dataframe with a new column: `time_rel_setrise`
calc_time_rel_to_sun <- function(decisions, sun_data, timezone = "EST") {
  
  sun_data$date <- as.Date(sun_data$date)
  if (is.character(sun_data$rise)) sun_data$rise <- as.POSIXct(sun_data$rise, format = "%Y-%m-%d %H:%M", tz = timezone)
  if (is.character(sun_data$set))  sun_data$set  <- as.POSIXct(sun_data$set, format = "%Y-%m-%d %H:%M", tz = timezone)
  
  # Ensure timezone awareness
  sun_data$rise <- lubridate::force_tz(sun_data$rise, tzone = timezone)
  sun_data$set  <- lubridate::force_tz(sun_data$set, tzone = timezone)
  
  # Join on date
  merged <- dplyr::left_join(decisions, sun_data, by = "date")
  
  # Calculate time difference in minutes
  merged$time_rel_setrise <- ifelse(
    merged$session == "am",
    as.numeric(difftime(merged$t_ret, merged$rise, units = "mins")), # NB! Decisions dataframe must also be formatted correctly!
    as.numeric(difftime(merged$t_ret, merged$set, units = "mins"))
  )
  
  # Remove unnecessary columns
  merged <- dplyr::select(merged, -rise, -set)
  
  return(merged)
}
