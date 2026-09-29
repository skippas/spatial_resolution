library(tidyverse)

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
  select(!c(cycle_width_dep, position_dep, t_dep,
            ELTD, ELTR, notes, t_video_start, ymaze,
            video_name, multiple_decisions)) %>%
  mutate(
    # append a session variable to data
    session = ifelse(
      format(decisions$t_ret, "%H") >= 12, "pm", "am"), 
    # format t_ret so that it has correct date but NAs also remain NA
    # why is it NB that NAs remain NA?
    date = ymd(date))

# filtering all rows that dont have a return decision, a return time, or a nest name ####
# check vars below to make sure we aren't losing any data unnecessarily
decision_no_nest <- decisions %>% filter( ! is.na(first_decision) & is.na(nest))
decision_no_t_ret <- decisions %>% filter( ! is.na(first_decision) & is.na(t_ret))
decisions_NA <- decisions %>% filter(is.na(first_decision))
valid_returns <- decisions %>% filter(! is.na(t_ret))
rm(list = c("valid_returns", "decision_no_nest", "decision_no_t_ret", "decisions_NA"))
decisions <- decisions %>% filter(!is.na(first_decision), ! is.na(t_ret), ! is.na(nest)) 

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
calculate_cycles_per_degree <- function(patt_period, view_dist) {
  theta <- 2 * atan(patt_period / (2 * view_dist))
  theta_degrees <- theta * (180 / pi)
  cycles_per_degree <- 1 / theta_degrees
  cycles_per_degree <- round(cycles_per_degree, 3)
  return(cycles_per_degree)
}
decisions <- decisions %>%
  mutate(patt_freq_ret = calculate_cycles_per_degree(patt_period, view_dist))

# adding a trial counter ####
decisions <- decisions %>%
  arrange(nest, date, session) %>%
  group_by(nest, date, session) %>%
  mutate(trial_number = cur_group_id())

# make dataframe more readable ####
decisions <- decisions %>% 
  relocate(date, t_ret, session, .after = training_or_testing) %>%
  relocate(trial_number, .after = nest) 

# Excel clock times come in on a dummy 1899-12-31 date: keep the time of day,
# put it on the real date, and label it local time (same tz as add_light_level)
decisions$date  <- as_date(decisions$date)
decisions$t_ret <- force_tz(as_datetime(decisions$date) + round(as.numeric(decisions$t_ret) %% 86400), "EST")
