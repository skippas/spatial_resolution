calculate_relative_return_time <- function(sunrise_set, return_times){
  
  # Merge the dataframes by date
  df_merged<- left_join(return_times, sunrise_set, by = "date")  
  # Calculate relative return time
  df_merged$time_rel_setrise <- ifelse(df_merged$session == 'am',
                                       (difftime(df_merged$t_ret, df_merged$rise, units = "mins")),
                                       (difftime(df_merged$t_ret, df_merged$set, units = "mins"))
  )
  
  # drop the rise and set time variables
  df_merged <- subset(df_merged, select = -c(rise, set))
  # Return the modified dataframe
  return(df_merged)
  