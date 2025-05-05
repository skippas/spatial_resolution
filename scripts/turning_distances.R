# Turning distance analysis
library(tidyverse)

# loading and cleaning
# bring in maze 1 data
turndist <- readxl::read_xlsx("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/panama/data/2024/Spatial_resolution_experiment.xlsx",
                                 sheet = "turning_frequencies_maze1", na = "NA", n_max = 1000,
                              range = "A1:L49")
turndist$maze <- "1"

turndistm2 <- readxl::read_xlsx("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/panama/data/2024/Spatial_resolution_experiment.xlsx",
                              sheet = "turning_frequencies_maze2", na = "NA", n_max = 1000,
                              range = "A1:L39")
turndistm2$maze <- "2"

turndist <- rbind(turndist, turndistm2)

# get rid of rows that have no turn 
turndist <- turndist %>% filter( ! is.na(turning_distance_cm)) 

turndist <- turndist %>%
  separate(screenshot_name, into = c("video_name", "bee_flight", "turn_number"),
           sep = "_") 

# remove letters from turn and flight columns
turndist$bee_flight <- as.numeric(gsub("\\D", "", turndist$bee_flight))
turndist$turn_number <- as.numeric(gsub("\\D", "", turndist$turn_number))

# which rows need light level prediction, or need video names checked?
# bring in light data
m2light_levels <- readxl::read_xlsx("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/panama/data/2024/Spatial_resolution_experiment.xlsx",
                                    sheet = "light_levels_maze2", na = "NA")

m1light_levels <- readxl::read_xlsx("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/panama/data/2024/Spatial_resolution_experiment.xlsx",
                                    sheet = "light_levels_maze1", na = "NA")
light_levels <- rbind(m1light_levels, m2light_levels)

light_levels$video_name <- light_levels$maze_video_name
light_levels$bee_flight <- as.character(light_levels$bee_flight)
light_levels$maze <- as.character(light_levels$maze)


turndist_nomatch <- turndist %>% 
  select(! light_level_ret) %>%
  mutate(bee_flight = as.character(bee_flight)) %>% 
  anti_join(light_levels[,c("maze", "video_name", "bee_flight", "light_level_ret", "light_level_proxy")],
            by = c("maze", "video_name", "bee_flight"))
rm(turndist_nomatch)

turndist <- turndist %>% 
  select(! light_level_ret) %>%
  mutate(bee_flight = as.character(bee_flight)) %>% 
  inner_join(light_levels[,c("maze", "video_name", "bee_flight", "light_level_ret", "light_level_proxy")],
             by = c("maze", "video_name", "bee_flight"))


# move values from light level proxy to light level if there is a proxy value provided
turndist<- turndist %>% mutate(light_level_obs_prox = if_else(!is.na(as.numeric(light_level_proxy)),
                    light_level_proxy, as.numeric(light_level_ret)))



# and average the distances of turns within a flight
turndist %>% group_by(video_name, bee_flight) %>%
  summarise(mean(turning_distance_cm), 
            mean(light_level_ret))

give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

p <- turndist %>% 
  ggplot(aes(x = as.character(patt_freq_ret),
             y = turning_distance_cm,
             fill = as.character(patt_freq_ret)))+
  geom_boxplot()+
  geom_jitter(width = 0.1, alpha = 0.3)+
  stat_summary(fun.data = give.n, geom = "text", size = 5, fontface = "bold")+
  labs(y = "distance along arm of bee turn (cm)",
       x = "pattern frequency as viewed from 31.2cm (cpd)")+
  guides(fill = "none")+
  theme_light(base_size = 25)
ggsave("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/my_phd/images/boxplot_turning_distances_for_diff_patterns.png",
       plot = p, width = 8, height = 8, dpi = 300)

 p <- turndist %>%
  ggplot(aes(x = log(as.numeric(light_level_obs_prox)),
                     y = turning_distance_cm,
             colour = as.character(patt_freq_ret)))+
         geom_point(size = 4, alpha =0.7)+
  labs(y = "distance along arm of bee turn (cm)",
       x = "light level (footcandles)",
       colour = "Pattern frequency \nviewed from 31.2cm")+
  theme_light(base_size = 25)+
   theme(legend.position = "bottom", legend.box = "horizontal")+
  guides(colour=guide_legend(title.position="top"))
ggsave("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/my_phd/images/scatterplot_turning_distances_according_to_light_level.png",
       plot = p, width = 8, height = 8, dpi = 300)

mod1 <- lm(turning_distance_cm~log(as.numeric(light_level_obs_prox))+patt_freq_ret,
           data = turndist)
summary(mod1)

mean_turn_freq <- turndist %>%
  mutate(recalculated_patt_freq = as.numeric(recalculated_patt_freq)) %>%
  filter(recalculated_patt_freq < Inf) %>%
  group_by(patt_freq_ret) %>%
  summarise(mean_freq = mean(recalculated_patt_freq))

p <- turndist %>%
  mutate(recalculated_patt_freq = as.numeric(recalculated_patt_freq),
         patt_freq_ret = as.character(patt_freq_ret)) %>%
  ggplot(aes(x = recalculated_patt_freq,
             fill = patt_freq_ret))+
  geom_histogram()+
  geom_vline(data = mean_turn_freq,
             aes(xintercept = mean_freq))+
  labs(x = "frequency of pattern\nat closest approach")+
  facet_wrap(facets = vars(patt_freq_ret), ncol =1)+
  theme_light(base_size = 25)+
  guides(fill="none")
ggsave("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/my_phd/images/histogram_pattern_freq_closest_approach_for_diff_patterns.png",
       plot = p, width = 8, height = 8, dpi = 300)

p <- turndist %>%
  ggplot(aes(x = log(light_level_obs_prox),
             y = as.numeric(recalculated_patt_freq),
             colour = as.character(patt_freq_ret)))+
  geom_point(size = 4, alpha = 0.7)+
  labs(y = "frequency of pattern\nat closest approach",
       x = "light level (footcandles)", 
       colour = "Pattern frequency \nviewed from 31.2cm")+
  scale_y_continuous(limits = c(0,0.15))+
  theme_light(base_size = 25)+
  theme(legend.position = "bottom",
        legend.box = "horizontal")+
  guides(colour=guide_legend(title.position="top"))
ggsave("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/my_phd/images/scatterplot_freq_closest_approach_according_to_light_level.png",
       plot = p, width = 8, height = 8, dpi = 300)

hist(turndist[turndist$patt_freq_ret == "0.05",]$turning_distance_cm)
# merge light levels data



# Is there a relationship between distance and light level yet?
## might need to bring in some light levels from the curve

