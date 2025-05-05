library(tidyverse)

# calculating t_rel sunrise / set ####
# read / format 2014 sunrise/set time data ####
rise_set_times_2014 <- readxl::read_xlsx("data//sunrise_set_data//sunrise_set_times_2014_2024.xlsx",
                              sheet = "sunrise_set_times_2014", 
                              range = "A1:Y32")
names(rise_set_times_2014) <- tolower(names(rise_set_times_2014))

rise_set_times_2014 <- rise_set_times_2014 %>% pivot_longer(
  cols = january.rise:december.set,
  names_to = c("month", "rise_or_set"),
  names_sep = "\\.",
  values_to = "rise_set_time") %>%
  mutate(year = 2014)

rise_set_times_2014$month_num <- match(tolower(rise_set_times_2014$month),
                                       tolower(month.name))
# Create the date column using make_date()
rise_set_times_2014$date <- make_date(year = rise_set_times_2014$year,
                                      month = rise_set_times_2014$month_num,
                                      day = rise_set_times_2014$day)
rise_set_times_2014 <- rise_set_times_2014 %>%
  select(! c(day, month_num, month, year))

# change time format from hhmm to hh:mm
hours <- substr(rise_set_times_2014$rise_set_time, 1, 2)
minutes <- substr(rise_set_times_2014$rise_set_time, 3, 4)
rise_set_times_2014$rise_set_time <- paste(hours, minutes, "00", sep = ":")
rm(list = c("hours", "minutes"))

# Keep rows with no NA values
rise_set_times_2014 <- rise_set_times_2014[complete.cases(rise_set_times_2014),]

rise_set_times_2014 <- rise_set_times_2014 %>%
  pivot_wider(names_from = "rise_or_set",
              values_from = "rise_set_time") 

# Read / format 2014 light levels ####
# Get the list of text files in the directory
# This code gives a warning 'embedded nuls' . No time to check this.
get_txt_files <- function(filename){
  tryCatch(read.table(filename, skip = 1, sep = "", skipNul = T, fill = T,
                      na.strings = c("", "++!%"),
                      fileEncoding = "UTF-8",
                      col.names = c("date", "time", "light_level")), 
           error=function(e) NULL)
}

directory <- "data//light_intensity_data//light_intensity_data_2014"
filenames <- list.files(directory, full.names = TRUE, pattern = '*.TXT')
light_levels_2014 <- lapply(filenames, get_txt_files)
light_levels_2014 <- data.table::rbindlist(light_levels_2014, idcol = "session")

# Convert the date format
light_levels_2014$date <- as.Date(light_levels_2014$date, format="%d/%m/%y")

# change time and light level class
light_levels_2014 <- light_levels_2014 %>% 
  mutate(time = as.POSIXct(time, format="%H:%M:%S"),
         light_level = as.numeric(light_level))

# Keep rows with no NA values (ie. remove strange cases)
strange_cases <- light_levels_2014[is.na(as.numeric(light_levels_2014$light_level)),]
rm(strange_cases)
light_levels_2014 <- light_levels_2014[complete.cases(light_levels_2014),]

# what to do about negative values?
## they seem sensible, so take the absolute value
# light_levels_2014 <- light_levels_2014 %>%
#   mutate(light_level = abs(light_level))
## They do not appear sensible! I wonder if they are zeroing errors. Originally I thought they were at start / end of readings, but now not sure.

## Lets filter them out:
ngtv_light_levels_2014 <- light_levels_2014 %>%
  filter(light_level < 0)
light_levels_2014 <- light_levels_2014 %>%
  filter(light_level > 0)

# Create a new column with time rounded down to the nearest minute
light_levels_2014$time <- floor_date(light_levels_2014$time, "minute")
# Calculate the average light level for each minute
light_levels_2014 <- light_levels_2014 %>%
  group_by(time, date) %>%
  summarize(light_level = mean(light_level))

rm(list = c("directory", "filenames", "get_txt_files"))

# Merge 2014 dataframes ####
light_levels_2014 <- merge(light_levels_2014, rise_set_times_2014[, c("date", "rise", "set")], 
                           by.x = "date", by.y = "date")

# get time relative to sunrise / set variable
## First make session variable for easy subsetting of evening / morning light levels
light_levels_2014$session <- ifelse(
  format(light_levels_2014$time, "%H") >= 12, "pm", "am")

light_levels_2014$rise <- as.POSIXct(light_levels_2014$rise,
                                     format="%H:%M:%S")
light_levels_2014$set <- as.POSIXct(light_levels_2014$set,
                                    format="%H:%M:%S")
light_levels_2014$time_rel_rise <- difftime(light_levels_2014$time,
                                            light_levels_2014$rise, units="mins")
light_levels_2014$time_rel_set <- difftime(light_levels_2014$time,
                                           light_levels_2014$set, units="mins")

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
       y = "log10 light level (lux?)")+
  geom_smooth(method = "loess", se = TRUE, color = "black")+
  theme(axis.title = element_text(size = 20))
ggsave("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/my_phd/images/scatter_light_levels_rel_sunrise_2014.png",
       plot = p, width = 10, height = 8, dpi = 300)

# fitting relationship betw 2014 light level and time relative to set or rise ####
loess_light_lvl_fit_am <- loess(log10(light_level) ~ as.numeric(time_rel_setrise),
                                data = light_levels_2014[light_levels_2014$session == "am",])
loess_light_lvl_fit_pm <- loess(log10(light_level) ~ as.numeric(time_rel_setrise),
                                data = light_levels_2014[light_levels_2014$session == "pm",])
saveRDS(loess_light_lvl_fit_pm, file = "predict_light_levels//loess_light_lvl_fit_pm.rds")
saveRDS(loess_light_lvl_fit_am, file = "predict_light_levels//loess_light_lvl_fit_am.rds")

rm(list = c("ngtv_light_levels_2014", "rise_set_times_2014"))
# then, convert t_return to t_return relative to sunrise or set for 2024
# predicting light levels in 2024 data ####

# Function to predict light levels using the existing LOESS fit
# deleted and turned into external function

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

# p <- p + geom_point(data = decisions,
#                aes(x = time_rel_setrise, y = light_pred), color = "black")
# so now we can see how decision is affected by light level

####
# Validating 2014 data by checking fit with 2024 light data ####
# how good is the fit of earlier and later 2024 data with 2014 data?
# if early 2024 data is off the 2014 curve, check if converting from foot candle to lux improves fit
# ggplot(decisions, aes(time_rel_setrise,
#                       log(light_level_ret), 
#                       color = as.Date(date)))+
#   geom_point()+
#   scale_colour_viridis_c(trans = "date")+
#   facet_wrap(~ session, scales = "free_x")
# 
# ggplot(data = light_levels_2014, aes(time_rel_setrise, log(light_level)))+
#   geom_point(color = "grey")
# # to see the difference in match before and after the reset, colour code by date
# p2 <- p + geom_point(data = decisions, aes(x = time_rel_setrise,
#                                        y = log(light_level_ret), 
#                                        color = date < 
#                                          as.Date("2024-04-03")),
#                show.legend = FALSE) +
#   scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red"))+
#   labs(x = "time relative to sun rise / set",
#        y = "log light level (lux?)")+
#   theme(axis.title = element_text(size = 20))
# ggsave("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/my_phd/images/scatter_light_levels_rel_sunrise_2014_t_ret_2024.png",
#        plot = p, width = 10, height = 8, dpi = 300)
# #the blue data still fits the 2014 data well although it is lower on avg. I wonder if 2014 data used two different factors. hopefully not.
# 
# # does using the tranformation from footcandles to lux improve old data (blue) fit?
# # To go from footcandles to current, multiply by fc factor. to go from current to lux, divide by lux factor: 
# # (values*(1.081*10^-5)) / (1.004*10^-6)
# decisions$light_level_tf <- ifelse(decisions$date < as.Date("2024-04-03"),
#                                    (decisions$light_level_ret),
#                                    decisions$light_level_ret)
# 
# p + geom_point(data = decisions, aes(x = time_rel_setrise,
#                                      y = log(light_level_tf), 
#                                      color = date < 
#                                        as.Date("2024-04-03")),
#                show.legend = FALSE) +
#   scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red"))
# Using the fc - lux conversion seems to make values too large. So I'm not sure what factor the blue dots were reading in

# I could compare last years light data just to confirm the units did change somewhere along the line.
# Get the list of text files in the directory
# Read and clean 2023 light level data ####

# filenames <- list.files("light_metre_data_2023", full.names = TRUE, pattern = '*.TXT')
# filenames <- filenames[! basename(filenames) %in% c("2023-02-19_evening2.TXT",
#                                                     "2023-02-19_evening1_18.21.TXT",
#                                                     "230329_evening.TXT",
#                                                     "2023-04-12_approx5.30_am.TXT")]
# light_levels_2023 <- lapply(filenames, get_txt_files)
# names(light_levels_2023) <- gsub(".TXT", "", basename(filenames), fixed = T)
# light_levels_2023 <- data.table::rbindlist(light_levels_2023, idcol = T)
# light_levels_2023[878979,] # I've no idea of the purpose of this line. seems to be fewer rows than this anyway.
# light_levels_2023 <- light_levels_2023 %>% separate(".id", into = c("date", "start_time", "session"), 
#                           sep = "_")
# 
# strange_cases <- light_levels_2023[is.na(as.numeric(light_levels_2023$light_level)),]
# light_levels_2023 <- na.omit(light_levels_2023)
# 
# light_levels_2023 <- light_levels_2023 %>% mutate(light_level = as.numeric(light_level)) # some values are coerced to NA, and I dont know why, they seem fine, and when I make them numeric individually, it works. 
# light_levels_2023[is.na(light_levels_2023$light_level),]
# light_levels_2023 <- na.omit(light_levels_2023)

# averaging readings per second. Should do it per minute
# start time

# light_levels_2023 <- light_levels_2023 %>% group_by(start_time, date, session, elapsed_time) %>%
#   summarise("light_level" = mean(light_level))
# 
# light_levels_2023 <- light_levels_2023 %>% 
#   mutate(elapsed_time = parse_date_time(elapsed_time, orders = c("%H:%M:%S")),
#          elapsed_time = substr(elapsed_time, 11, 20))

# I need to get proper time (start + elapsed) and then merge with 2023 sunrise and set

# light_levels_2023$start_time <- format(strptime(light_levels_2023$start_time, format = "%H.%M"), format = "%H:%M:%S")
# class(light_levels_2023$start_time)
# 
# 
# 
# colour <- colour %>% 
#   mutate(across(contains("elapsed"), parse_date_time, orders = c("%H:%M:%S")), 
#          across(contains("elapsed"), substr, 11, 20)) %>%
#   pivot_longer(cols = contains("elapsed"), names_to = "events",
#                values_to = "elapsed_time") 
# 
# colour <- merge(colour, data,  by.y = c("date", "session", "elapsed_time"),
#                 by.x = c("date", "session", "elapsed_time"), all.x = T) %>%
#   pivot_wider(names_from = "events", values_from = c("elapsed_time", "light_level"))
# 
# write.csv(colour, "data/colour_vision_data/Training_gayathri_Colour_Vision_light_level_appended.csv")

# read in 2014 lunar phase data ####
# add / merge lunar phase ####

# I should look at this from lunar phase to lunar phase (facet by phase number) 
# as Eric sugguested. And see how elevation predicts data

# # First make light_level date time correct so it can be used to match with lunar data
# light_levels_2014$elapsed_mins <- hms(format(light_levels_2014$elapsed_mins, "%H:%M:%S"))
# # Combine the correct date with the extracted time
# light_levels_2014$date_time <- light_levels_2014$date + light_levels_2014$elapsed_mins
# 
# light_levels_2014$date_time_hour <- floor_date(
#   as.POSIXct(light_levels_2014$date_time, format="%H:%M:%S"), "hour")
# 
# light_levels_2014 <- merge(light_levels_2014, lunar_phase_2014[, c("correct_datetime", "phase")], 
#                            by.x = "date_time_hour", by.y = "correct_datetime")
# 
# p <- ggplot(light_levels_2014, aes(as.numeric(time_rel_setrise),
#                                    log(light_level), 
#                                    color = phase))+
#   geom_point()+
#   labs(x = "time relative to sun rise / set",
#        y = "log light level (lux?)",
#        color = "moon phase")+
#   theme(axis.title = element_text(size = 20))
# ggsave("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/my_phd/images/scatter_light_levels_rel_sunrise_2014_color_by_moonphase.png",
#        plot = p, width = 10, height = 8, dpi = 300)
# 
# light_levels_2014$month <- month(light_levels_2014$date_time, label = TRUE)

# Plotting with facets for each month
# p <- ggplot(light_levels_2014, aes(x = time_rel_setrise,
#                                    y = log(light_level),
#                                    color = phase)) +
#   geom_point()+
#   labs(x = "time relative to sun rise / set",
#        y = "log light level (lux?)",
#        color = "moon phase")+
#   facet_wrap(~month, scales = 'free_x')
# ggsave("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/my_phd/images/scatter_light_levels_rel_sunrise_2014_color_by_moonphase_facet_month.png",
#        plot = p, width = 10, height = 8, dpi = 300)

# numerically, what kind of error can I expect if I use time relative to sunrise / set to predict light level?

# read and merge lunar phase data with decisions data ####

# lunar_phase_2014 <- readxl::read_xlsx("moon_data//moon_info_2014.xlsx",
#                                       sheet = "moon_phase_2014",
#                                       range = "A1:P8761")
# 
# lunar_phase_2014$month_num <- match(lunar_phase_2014$month, month.abb)
# lunar_phase_2014$date <- make_date(year = lunar_phase_2014$year,
#                                    month = lunar_phase_2014$month_num,
#                                    day = lunar_phase_2014$day)
# lunar_phase_2014 <- lunar_phase_2014 %>%
#   select(! c(day, month_num, month, year))
# 
# # Extract the time part from the incorrect date-time
# lunar_phase_2014$time <- hms(format(lunar_phase_2014$time, "%H:%M:%S"))
# # Combine the correct date with the extracted time
# lunar_phase_2014$correct_datetime <- lunar_phase_2014$date + lunar_phase_2014$time
# # convert to Panama time
# lunar_phase_2014$correct_datetime <- lunar_phase_2014$correct_datetime - hours(5)

