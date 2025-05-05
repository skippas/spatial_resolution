# read and clean 2024 rise/set data ####
rise_set_times_2024 <- readxl::read_xlsx("data//sunrise_set_data//sunrise_set_times_2014_2024.xlsx",
                                         sheet = "sunrise_set_times_2024", 
                                         range = "A1:Y32")

rise_set_times_2024 <- rise_set_times_2024 %>% pivot_longer(
  cols = january.rise:december.set,
  names_to = c("month", "rise_or_set"),
  names_sep = "\\.",
  values_to = "rise_set_time") %>%
  mutate(year = 2024)
# Keep rows with no NA values
rise_set_times_2024 <- rise_set_times_2024[complete.cases(rise_set_times_2024),]
# there is probs a way to avoid this subsequent pivot wider step by changing pivot longer above
rise_set_times_2024 <- rise_set_times_2024 %>%
  pivot_wider(names_from = "rise_or_set",
              values_from = "rise_set_time") 

# Create the date column using make_date()
rise_set_times_2024$month_num <- match(tolower(rise_set_times_2024$month),
                                       tolower(month.name))
rise_set_times_2024$date <- make_date(year = rise_set_times_2024$year,
                                      month = rise_set_times_2024$month_num,
                                      day = rise_set_times_2024$day)
rise_set_times_2024 <- rise_set_times_2024 %>%
  select(! c(day, month_num, month, year))
# rise_set_time will be "06:37:00" .  This is NB for difftime.
rise_set_times_2024$rise <- anytime::anytime(paste(rise_set_times_2024$date,
                                                   rise_set_times_2024$rise))
#rise_set_times_2024$rise<- format(rise_set_times_2024$rise, "%H:%M:%OS")
rise_set_times_2024$set <- anytime::anytime(paste(rise_set_times_2024$date,
                                                  rise_set_times_2024$set))
# csv doesnt preserve the formatting on date columms and things. better to save as rds for easy continuing by other scripts
saveRDS(rise_set_times_2024, file = "data//sunrise_set_data//sunrise_set_times_2024_formatted.rds")

# write.csv(rise_set_times_2024,
#           file = "data//sunrise_set_data//sunrise_set_times_2024_formatted.csv", row.names = FALSE)

# time zones must be set properly for difftime (used in loading_cleaning) to work properly
# rise_set_times_2024$rise <- force_tz(rise_set_times_2024$rise, tzone = "EST") 
# rise_set_times_2024$set <- force_tz(rise_set_times_2024$set, tzone = "EST") 
