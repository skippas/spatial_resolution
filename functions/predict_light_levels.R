#' Predict light levels based on time relative to sunrise/sunset
#'
#' @param decisions A data frame containing 'session' and 'time_rel_setrise'
#' @param fit_am The loess model fit for morning (am) sessions
#' @param fit_pm The loess model fit for afternoon (pm) sessions
#'
#' @return The same data frame with a new column `light_pred`
predict_light_levels <- function(decisions, fit_am, fit_pm) {
  decisions <- decisions %>%
    mutate(
      light_pred =
          ifelse(session == "am", predict(fit_am, decisions), # predict looks for the predictor variable it was trained on in the decision dataframe (ie. time_rel_setrise). Can also use this line instead of 'decisions': newdata = data.frame(time_rel_setrise = time_rel_setrise)
          ifelse(session == "pm",predict(fit_pm, decisions),
                 NA))
    ) %>%
    mutate(
      light_pred = if_else(
        is.na(light_pred) & !is.na(t_ret),
        max(fit_pm$fitted, na.rm = TRUE),
        light_pred
      )
    )
  
  return(decisions)
}
