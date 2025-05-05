# Load libraries
library(tidyverse)
library(lubridate)
library(readr)
library(openxlsx)  # for writing Excel files

# Define the raw text as lines (you may also load from a txt file)
raw_text <- readLines("data/USNO_sun_and_moon_data/sunrise_set_data/2025/sunrise_set_times_2025.txt")

# Extract only the lines that begin with day data (e.g. "01 0635 1811 ...")
day_lines <- raw_text[grepl("^\\s*\\d{2}\\s", raw_text)]

# Define the months (12 total)
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Function to parse each line into 12 pairs of rise/set
parse_line <- function(line, year = 2025) {
  values <- unlist(strsplit(trimws(line), "\\s+"))
  
  day <- as.integer(values[1])
  times <- values[-1]
  
  # Only consider complete rise/set pairs
  n_pairs <- floor(length(times) / 2)
  if (n_pairs == 0) return(NULL)
  
  months_available <- 1:n_pairs
  
  entries <- tibble(
    month = rep(months_available, each = 2),
    time_type = rep(c("rise", "set"), times = n_pairs),
    time_hm = times[1:(n_pairs * 2)],
    day = day
  )
  
  entries <- entries %>%
    pivot_wider(names_from = time_type, values_from = time_hm) %>%
    mutate(
      date = lubridate::make_date(year, month, day),
      rise = sprintf("%02d:%02d", as.integer(substr(rise, 1, 2)), as.integer(substr(rise, 3, 4))),
      set  = sprintf("%02d:%02d", as.integer(substr(set,  1, 2)), as.integer(substr(set,  3, 4)))
    ) %>%
    select(date, rise, set)
  
  return(entries)
}


# Parse all day lines and combine
all_data <- map_dfr(day_lines, parse_line)
all_data_formatted <- all_data %>%
  mutate(
  rise = as.POSIXct(paste(date, rise), format = "%Y-%m-%d %H:%M", tz = "UTC"),
  set  = as.POSIXct(paste(date, set),  format = "%Y-%m-%d %H:%M", tz = "UTC")
)

# View result
head(all_data)

# Save to Excel
write.xlsx(all_data_formatted, "data/USNO_sun_and_moon_data/sunrise_set_data/2025/sunrise_set_times_2025.xlsx")
write.csv(all_data_formatted, "data/USNO_sun_and_moon_data/sunrise_set_data/2025/sunrise_set_times_2025.csv")

