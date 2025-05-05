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

# csv loses all the work that goes into time formats and things
# write.csv(light_levels_2014,
#           file = "data//light_intensity_data//light_intensity_data_2014//light_level_2014_sunriseSet_time_merged.csv", row.names = FALSE)
saveRDS(light_levels_2014, file = "data//light_intensity_data//light_intensity_data_2014//light_level_2014_sunriseSet_time_merged.csv")

