rm(list = ls())
# merging maze data and light levels sheets
# 1. source the maze data
source("loading_cleaning_maze_data.r")
# 2. load and clean the light level data
m1light_levels <- readxl::read_xlsx("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/panama/data/2024/Spatial_resolution_experiment.xlsx",
                                  sheet = "light_levels_maze1", na = "NA")
m2light_levels <- readxl::read_xlsx("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/panama/data/2024/Spatial_resolution_experiment.xlsx",
                                  sheet = "light_levels_maze2", na = "NA")
# unselect unwanted vars

# merge maze 1 and 2 light level data
light_levels <- rbind(m1light_levels, m2light_levels)

# 3. merge light level data and maze data
decisions <- merge(decisions,
                   light_levels[, c("t_ret", "date", "light_level_ret")], 
                   by = c("t_ret", "date"), all.x = T)


# checking which rows are present in both light level and maze 1 dataframes, and which are present in only 1
result <- anti_join(light_levels, decisions, by = c("date", "t_ret"))
# only 1 row is in light level that is not in maze1
result <- anti_join(decisions, light_levels, by = c("date", "t_ret"))
# 23 are in maze1 not in light level
# remove rows that have no decision
decisions <- decisions %>% filter(first_decision != "NA") 
result <- anti_join(decisions, light_levels, by = c("date", "t_ret"))
# but only 5 rows that have a decision are not in the light level sheet. 
