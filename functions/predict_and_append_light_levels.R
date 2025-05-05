# Function to predict and append light levels
predict_and_append_light_levels <- function(df, time_var = "time_rel_setrise",
                                            session_var = "session",
                                            light_level_var = "light_pred"){
  # Check for necessary variables
  if (!time_var %in% names(df)) {
    stop(paste("The dataframe does not have a", time_var, "variable."))
  }
  if (!session_var %in% names(df)) {
    stop(paste("The dataframe does not have a", session_var, "variable."))
  }
  
  # Predict light levels based on the period (am/pm)
  df[[light_level_var]] <- mapply(function(time, session) {
    new_data <- data.frame(time_rel_setrise = time)
    if (!is.na(session)) {
      if (session == "am") {
        predict(loess_light_lvl_fit_am, newdata = new_data)
      } else if (session == "pm") {
        predict(loess_light_lvl_fit_pm, newdata = new_data)
      } else {
        NA  # Return NA if period is neither "am" nor "pm"
      }
    } else {
      NA  # Return NA if session is NA
    }
  }, df[[time_var]], df[[session_var]])
  
  return(df)
}
