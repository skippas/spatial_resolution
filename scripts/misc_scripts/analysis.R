# script to merge light meter readings into main excel data sheet

library(tidyverse)
library(lubridate)

excel_file <- "/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/panama/data/2024//Spatial_resolution_experiment_maze1.csv"

maze1 <- read.csv(excel_file, sep = ";", na.strings = c("NA", ""))
maze1$time_ret <- as.POSIXct(hms::parse_hms(maze1$t_ret))

excel_file <- "/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/panama/data/2024//Spatial_resolution_experiment_maze2.csv"

maze2 <- read.csv(excel_file, sep = ";",  na.strings = c("NA", ""))
maze2$time_ret <- as.POSIXct(hms::parse_hms(maze2$t_ret))

choices <- rbind(maze1, maze2)

excel_file <- "/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/panama/data/2024//Spatial_resolution_experiment_lightlevels.csv"

lightlevels <- read.csv(excel_file, sep = ";",  na.strings = c("NA", ""))
lightlevels$time_ret <- as.POSIXct(hms::parse_hms(lightlevels$t_ret))

choices<- merge(choices[, -which(names(choices) == "light_level_ret")],
                lightlevels[, c("t_ret", "date", "light_level_ret")], 
          by = c("t_ret", "date"), all.x = T)
choices$light_level_ret <- as.numeric(choices$light_level_ret)
choices$nest <- as.factor(choices$nest)
choices$X1st_decision <- as.character(choices$X1st_decision)

choices %>% 
  filter(time_ret < as.POSIXct(hms::parse_hms("17:00:00")) ) %>%
  ggplot(aes(x = time_ret, y = log(light_level_ret)))+
  geom_point()+
  scale_x_time()

summary(as.numeric(choices$light_level_ret), na.rm = T)

choices %>% 
  ggplot(aes(x = X1st_decision, y = log(light_level_ret)))+
  geom_boxplot()

# what is the frequency of correct decisions at different spatial frequencies?
# First, calculate the frequencies
choice_freq <- choices %>%
  group_by(vpatt_freq_ret, X1st_decision) %>%
  summarise(frequency = n())

# Now, create the plot
ggplot(choice_freq, aes(x = vpatt_freq_ret, y = frequency, fill = as.factor(X1st_decision))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("red", "green"), 
                    labels = c("Incorrect", "Correct"),
                    name = "Choice") +
  labs(x = "Spatial Frequency", y = "Frequency of Choices", 
       title = "Frequencies of Correct Choices Across Different Spatial Frequencies") +
  theme_minimal()

choices %>%
  ggplot(aes(x = nest, y = log(light_level_ret)))+
  geom_boxplot()

maze1 %>% filter(time_ret > as.POSIXct(hms::parse_hms("17:00:00")) ) %>%
  ggplot(aes(x = time_ret, y = light_level_ret))+
  geom_point()+
  scale_x_time()

# Read the second sheet into another dataframe
ligt_levels_from_cam <- read_excel(excel_file, sheet = "light_levels_from_videos")

# Now df1 and df2 contain the data from the first and second sheets, respectively
