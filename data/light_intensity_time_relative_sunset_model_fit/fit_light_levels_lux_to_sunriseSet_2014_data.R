library(tidyverse)
# load formatted data produced by format_merge_2014_light_level_sunriseSet_data.R
light_levels_2014 <-
  readRDS(file = "data//light_intensity_data//light_intensity_data_2014//light_level_2014_sunriseSet_time_merged.csv")

# get time relative to sunrise / set variable
## First make session variable for easy subsetting of evening / morning light levels
light_levels_2014$session <- ifelse(
  format(light_levels_2014$time, "%H") >= 12, "pm", "am")

light_levels_2014$rise <- hms::as_hms(light_levels_2014$rise)
light_levels_2014$set <- hms::as_hms(light_levels_2014$set)
light_levels_2014$time <- hms::as_hms(light_levels_2014$time)

light_levels_2014$time_rel_rise <- as.numeric(difftime(light_levels_2014$time,
                                            light_levels_2014$rise, units="mins"))
light_levels_2014$time_rel_set <- as.numeric(difftime(light_levels_2014$time,
                                           light_levels_2014$set, units="mins"))

# we want time relative to rise or set in one column, so will need to pivot longer
# also, we want a time relative to set or rise, not both
light_levels_2014 <- light_levels_2014 %>% pivot_longer(cols = c(time_rel_rise, time_rel_set),
                                                        values_to = "time_rel_setrise",
                                                        names_to = "times_relative_to") %>%
  filter((session == "pm" & times_relative_to == "time_rel_set") |
           (session == "am" & times_relative_to == "time_rel_rise"))
# We also no longer need the time_rel_to variable as all pm readings are relative to set and am to rise

# plotting 2014 light level data by time relative to rise / set ####

# light levels at the same time vary over the year
ggplot(light_levels_2014, aes(time,
                              log10(light_level), 
                              color = date))+
  geom_point()+
  theme_bw()+
  scale_colour_viridis_c(trans = "date")+
  facet_wrap(~ session, scales = "free_x")

# Removing the first 5 or so minutes cleans up readings. Although there is still some noise
# remove the first minute of each reading.
# ALSO NEED TO GROUP BY SESSION!!!
## group by date, and remove the first minute of each date / day / reading
light_levels_2014 <- light_levels_2014 %>%
  group_by(date, session) %>%
  slice_max(time_rel_setrise, n = -3) %>%
  slice_min(time_rel_setrise, n = -3) %>%
  ungroup()
# Not all the noise is gone yet. And I'm not sure where it comes from. But it probably doesnt affect prediction much

# Time relative to set or rise accounts for most of this
p <- ggplot(light_levels_2014, aes(as.numeric(time_rel_setrise),
                              log10(light_level), 
                              color = date))+
  geom_point()+
  scale_colour_viridis_c(trans = "date")+
  facet_wrap(~ session, scales = "free_x")+
  labs(x = "time relative to sun rise / set",
       y = "light level log10(lux)")+
  geom_smooth(method = "loess", se = TRUE, color = "black")+
  theme(axis.title = element_text(size = 20))
ggsave("plots/scatter_light_levels_rel_sunrise_2014.png",
       plot = p, width = 10, height = 8, dpi = 300)

# fitting relationship betw 2014 light level and time relative to set or rise ####
loess_light_lvl_fit_am <- loess(log10(light_level) ~ as.numeric(time_rel_setrise),
                                data = light_levels_2014[light_levels_2014$session == "am",])
loess_light_lvl_fit_pm <- loess(log10(light_level) ~ as.numeric(time_rel_setrise),
                                data = light_levels_2014[light_levels_2014$session == "pm",])
saveRDS(loess_light_lvl_fit_pm, file = "predict_light_levels//loess_light_lvl_fit_pm.rds")
saveRDS(loess_light_lvl_fit_am, file = "predict_light_levels//loess_light_lvl_fit_am.rds")

# then, convert t_return to t_return relative to sunrise or set for 2024
# predicting light levels in 2024 data ####

# plot predicted data over 2014 data:
p <- ggplot(data = light_levels_2014, aes(time_rel_setrise, log10(light_level)))+
  geom_point(color = "grey")

p <- ggplot(light_levels_2014, aes(time_rel_setrise,
                                   log10(light_level), color = log10(light_level)))+
  geom_point()+
  facet_wrap(~ session, scales = "free_x")+
  labs(x = "time relative to sun rise / set",
       y = "log10 light level (lux?)")+
  geom_smooth(method = "loess", se = TRUE, color = "black")+
  theme(axis.title = element_text(size = 20))+
  theme_dark()

