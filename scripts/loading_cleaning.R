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

# Excel clock times come in on a dummy 1899-12-31 date: keep the time of day,
# put it on the real date, and label it local time (same tz as add_light_level)
decisions$date  <- as_date(decisions$date)
decisions$t_ret <- force_tz(as_datetime(decisions$date) + round(as.numeric(decisions$t_ret) %% 86400), "EST")
