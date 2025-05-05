library(tidyverse)
library(lubridate)

# reading maze data ####
m2decisions <- readxl::read_xlsx("data/Spatial_resolution_experiment.xlsx",
                                 sheet = "maze2", na = "NA", n_max = 1000)
m1decisions <- readxl::read_xlsx("data/Spatial_resolution_experiment.xlsx",
                                 sheet = "maze1", na = "NA", n_max = 1000)
decisions <- rbind(m1decisions, m2decisions)
rm(m1decisions, m2decisions)

# modifying dataframe ####
decisions <- decisions %>% 
  # Remove unnecessary variables
  select(!c(vpatt_freq_dep, pos_vpatt_dep, t_dep,
            elapse_t_dep, elapse_t_ret, notes, proxy_method, light_level_proxy,
            light_level_issue, t_video_start, ymaze,
            video_name, multiple_decisions, light_level_ret)) %>% 
  # make light level numeric, changing cells that cannot be coerced (eg. bc they have alpha character eg. 0.2 / 2.0)
  mutate(
    # append a session variable to data
         session = ifelse(
           format(decisions$t_ret, "%H") >= 12, "pm", "am"),
         # format t_ret so that it has correct date but NAs also remain NA
         # why is it NB that NAs remain NA?
         date = ymd(date))

# Extract the time part from 'incorrect_time' and combine it with 'correct_date'
decisions$t_ret <- decisions$date + hms(format(decisions$t_ret, "%H:%M:%S"))
# East. Stand. Time (EST) so that difftime works with sunrise/set data which 
# is currently in EST time
decisions$t_ret <- force_tz(decisions$t_ret, tzone = "EST") 

# filtering all rows that dont have a return decision, a return time, or a nest name ####
# check vars below to make sure we aren't losing any data unnecessarily
decision_no_nest <- decisions %>% filter( ! is.na(first_decision) & is.na(nest))
decision_no_t_ret <- decisions %>% filter( ! is.na(first_decision) & is.na(t_ret))
decisions_NA <- decisions %>% filter(is.na(first_decision))
valid_returns <- decisions %>% filter(! is.na(t_ret))
rm(list = c("valid_returns", "decision_no_nest",
            "decision_no_t_ret", "decisions_NA"))

decisions <- decisions %>% filter(!is.na(first_decision), ! is.na(t_ret), ! is.na(nest)) 

# recalculating pattern frequencies ####

# what I want is a column called pattern_frequency_return
# and another column called pattern_frequency_pos3
# vpatt_freq_ret must be renamed to patt_freq_ret_pos3
# and patt_freq_ret must be calculated from position and vpatt_freq_ret columns

# first, simply add a column called viewing distance instead of position 
decisions <- decisions %>%
  mutate(view_dist = case_when(
    pos_vpatt_ret == "1" ~ 50,
    pos_vpatt_ret == "2" ~ 180,
    pos_vpatt_ret == "3" ~ 312,
    TRUE ~ as.numeric(pos_vpatt_ret)  # Keep the original value if no match
  ))
#decisions <- decisions %>% rename(view_dist = pos_vpatt_ret)
# second, add a column with pattern period instead of vpatt_freq_ret
decisions$vpatt_freq_ret <- as.numeric(decisions$vpatt_freq_ret)
decisions <- decisions %>%
  mutate(
    vpatt_freq_ret = case_when(
    vpatt_freq_ret == 0.05 ~ 2.1707,
    vpatt_freq_ret == 0.10 ~ 4.34139,
    vpatt_freq_ret == 0.15 ~ 6.51209,
    vpatt_freq_ret == 0.20 ~ 8.68279,
    vpatt_freq_ret == 0.25 ~ 5*2.1707,
    vpatt_freq_ret == 0.30 ~ 6*2.1707,
    TRUE ~ vpatt_freq_ret)) %>%  # Keep the original value if no match
  mutate(vpatt_freq_ret = 250 / vpatt_freq_ret)
decisions <- decisions %>% rename(patt_period = vpatt_freq_ret)

# then, make calculation for new column, patt_freq_ret
calculate_cycles_per_degree <- function(patt_period, view_dist) {
  theta <- 2 * atan(patt_period / (2 * view_dist))
  theta_degrees <- theta * (180 / pi)
  cycles_per_degree <- 1 / theta_degrees
  cycles_per_degree <- round(cycles_per_degree, 3)
  return(cycles_per_degree)
}
decisions <- decisions %>%
  mutate(patt_freq_ret = calculate_cycles_per_degree(patt_period, view_dist))

# make pattern orientation column:
source("functions/cacl_time_rel_sunrise_sunset.R")
sun_data <- read.csv("data/sunrise_set_data/sunrise_set_times_formatted_2024.csv")
decisions <- calc_time_rel_to_sun(decisions, sun_data = sun_data)

# load fits produced by fitting_light_levels_to_sunriseSet_2014_data.R script
# source("predict_light_levels//fitting_light_levels_to_sunriseSet_2014_data.R", local = T)
fit_am <- readRDS("predict_light_levels/loess_light_lvl_fit_am.rds")
fit_pm <- readRDS("predict_light_levels/loess_light_lvl_fit_pm.rds")

source("functions//predict_light_levels.R")
decisions_new <- predict_light_levels(decisions, fit_am, fit_pm)

# Check distribution of light levels 
# p <- p + geom_point(data = decisions,
#                     aes(x = as.numeric(time_rel_setrise), y = light_pred),color = "black")+
#   labs(color = "log(lux)")+
#   theme_gray(base_size = 25)
# ggsave("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/my_phd/images/scatter_predictedLightLevels_facetBySession.png",
#        plot = p, width = 10, height = 8, dpi = 300)

# adding a trial counter ####
decisions <- decisions %>%
  arrange(nest, date, session) %>%
  group_by(nest, date, session) %>%
  mutate(trial_number = cur_group_id())

# make dataframe more readable ####
decisions <- decisions %>% 
  relocate(date, t_ret, session, .after = training_or_testing) %>%
  relocate(trial_number, .after = nest) 

# predict light level in cd / sqmeter from Kelber data ####
loess_light_lvl_fit_pm_candela <- readRDS("predict_light_levels//loess_light_lvl_fit_pm_candela.rds")
loess_light_lvl_fit_am_candela <- readRDS("predict_light_levels//loess_light_lvl_fit_am_candela.rds")

decisions <- decisions %>%
  ungroup() %>% # it was the fact that the data was grouped that caused problems with predict LOESS function!!!
  mutate(
    light_pred_candela =
      ifelse(session == "am",
             predict(loess_light_lvl_fit_am_candela, decisions), # i dont know why, but using the dataframe directly here doesnt work
             ifelse(session == "pm",
                    predict(loess_light_lvl_fit_pm_candela, decisions),
                    NA)))

# Compare the lux predictions to the candela predictions:
# Define the range of time values for predictions
time_values <- seq(min(decisions$time_rel_setrise),
                   max(decisions$time_rel_setrise), length.out = 100)
predict(loess_light_lvl_fit_pm, loess_light_lvl_fit_am$fitted)
loess_light_lvl_fit_am$y
# Generate predictions for each model over the time range
prediction_data <- data.frame(
  time_rel_setrise = time_values,
  light_pred_am = predict(loess_light_lvl_fit_am, time_values),
  light_pred_pm = predict(loess_light_lvl_fit_pm, time_values),
  light_pred_am_candela = predict(loess_light_lvl_fit_am_candela, time_values),
  light_pred_pm_candela = predict(loess_light_lvl_fit_pm_candela, time_values)
) 
# to neaten up plot below, could pivot dataframe longer so that unit and session have their own cols 
# %>% pivot_longer(values_to = ) 

# Plotting the predictions
ggplot(decisions, aes(x = time_rel_setrise)) +
  # Add the best-fit lines from each model
  geom_line(data = prediction_data, aes(y = light_pred_am, color = "lux fit"), size = 1) +
  geom_line(data = prediction_data, aes(y = light_pred_pm, color = "lux fit"), size = 1) +
  geom_line(data = prediction_data, aes(y = light_pred_am_candela, color = "candela fit"), size = 1) +
  geom_line(data = prediction_data, aes(y = light_pred_pm_candela, color = "candela fit"), size = 1) +
  scale_color_manual(values = c("lux fit" = "blue", "candela fit" = "red", "am" = "blue", "pm" = "red")) +
  theme_minimal()

# Assume the data frame `decisions` has time (minutes), light_pred_candela, and light_pred_lux columns
ggplot(decisions, aes(x = time_rel_setrise)) +
  # Primary prediction plot
  geom_point(aes(y = light_pred_candela), color = "blue") +
  labs(y = "Light Level (Candela)") +  # Label for the primary axis
  # Secondary axis
  geom_point(aes(y = light_pred), color = "red") +  
  scale_y_continuous(
    sec.axis = sec_axis(~ ., name = "Light Level (Lux)")  # Scale back the secondary axis to original magnitude
  ) +
  # Axis labels and title
  labs(x = "Time Relative to Sunrise (Minutes)", title = "Light Predictions in Candela and Lux") +
  theme_minimal()

# I think the best would be to calculate an average conversion factor between the curves, 
# and then use that conversion factor to rather transform all values from lux to candela
conversion_factor <- mean(decisions$light_pred - decisions$light_pred_candela, na.rm = T)
