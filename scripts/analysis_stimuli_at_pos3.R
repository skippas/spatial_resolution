library(tidyverse)
library(lubridate)
rm(list = ls())
# color scale
bin_fill <- c('1' = '#27b376', '0' = '#bf212f')
fill_scale <- scale_fill_manual(name = "first_decision", values = bin_fill)

#### loading, cleaning ####
source("loading_cleaning_maze_data.r")
# analysis for testing at pos 3 ####
decisions <- decisions %>% filter(date < "2024-04-16")

# much more sampling for 0.05
table(decisions$patt_freq_ret, decisions$pos_vpatt_ret)
# what do correct vs incorrect decisions look like? 
decisions_freq <- decisions %>%
  group_by(patt_freq_ret, first_decision) %>%
  summarise(frequency = n())

# correct decisions across frequencies?
p <- decisions %>%  
  filter(first_decision == "1" | first_decision == "0",
         patt_freq_ret != "NA") %>%
  ggplot(aes(x = as.character(patt_freq_ret),
             fill = as.character(first_decision))) +
  geom_bar(position = "dodge")+
  labs(fill = "first decision",
       x = "pattern frequency")+
  theme_light(base_size = 25)
ggsave("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/my_phd/images/barplot_first_decisions_pattern_at_315mm.png", plot = p, width = 10, height = 8, dpi = 300)

decisions %>% 
  filter(first_decision == "1" | first_decision == "0",
         patt_freq_ret != "NA") %>%
  ggplot(aes(x = as.character(patt_freq_ret),
             fill = as.character(first_decision))) +
  geom_bar(position = "fill")+
  labs(fill = "first decision",
       x = "pattern frequency")

# View 2024 light levels
# evening
decisions %>%  filter(!is.na(light_level_ret)) %>%
  filter(format(t_ret, "%H:%M:%S") >= "17:00:00") %>%
  ggplot(aes(x = t_ret, y = log(light_level_ret)))+
  geom_point()+
  scale_x_time()
# morning
decisions %>%  filter(!is.na(light_level_ret)) %>%
  filter(format(t_ret, "%H:%M:%S") <= "17:00:00") %>%
  ggplot(aes(x = t_ret, y = log(light_level_ret)))+
  geom_point()+
  scale_x_time()

class(decisions)
# what do correct decisions across light levels look like?
mirror_plot_data <- decisions %>%
  filter(patt_freq_ret != "NA" & patt_freq_ret != "0.3") 
ggplot(mirror_plot_data, aes(x = log(light_level_ret), fill = first_decision)) +
  geom_histogram(data = subset(mirror_plot_data, first_decision == '1'),
                 aes(y = ..count..), binwidth = 1) +
  geom_histogram(data = subset(mirror_plot_data, first_decision == '0'),
                 aes(y = -..count..), binwidth = 1) +
  scale_y_continuous(labels = abs) + # To display positive counts on both sides
  labs(x = "Light Level", y = "Count") +
  theme_minimal()+
  facet_wrap(facets = "patt_freq_ret")
rm(mirror_plot_data)
#coord_flip() # Optional: flips the axes to display light levels on the y-axis

decisions %>%
  filter(!is.na(first_decision)) %>%
  filter(!is.na(patt_freq_ret)) %>%
  ggplot(aes(x = log(light_level_ret), y = first_decision, 
             colour = first_decision))+
  geom_jitter(height = 0.03)+
  facet_wrap(facets = "patt_freq_ret")

p <- decisions %>%
  filter(!is.na(first_decision)) %>%
  filter(!is.na(patt_freq_ret)) %>%
  ggplot(aes(y = log(light_level_ret), x = first_decision,
             fill = first_decision))+
  geom_boxplot()+
  facet_wrap(facets = "patt_freq_ret", strip.position = "bottom")+
  labs(x = "first decisions for different pattern frequencies", y = "log(illuminance in footcandles?)", 
       fill = "first decision")+
  theme_light(base_size = 25)+
  theme(axis.text.x = element_blank())
ggsave("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/my_phd/images/boxplot_decisions_at_diff_light_levels.png",
       plot = p, width = 10, height = 8, dpi = 300)

# make vpatt ret a character
# make first decision numeric
decisions <- decisions %>% mutate(light_level_ret = as.numeric(light_level_ret),
                                  patt_freq_ret = as.character(patt_freq_ret),
                                  first_decision = as.numeric(first_decision))
# do light levels affect 1, 0 ?
mod1<- glm(first_decision ~ light_level_ret,
           data = decisions[decisions$patt_freq_ret == "0.15",], family = binomial)
summary(mod1)

mod2<- glm(first_decision ~ log(light_level_ret) + patt_freq_ret,
           data = decisions, family = binomial)
summary(mod2)

# TO DO ####
# Is all the data in the maze1 sheet filled out? 
# I want a row in the light level sheet for every row in the maze 1 sheet
# Then filter from maze1, 
# I have rows with data for decisions, but 9 rows with no data for the return freq of those decisions. Why?
# Why do I have 1 NA in return time?
# binomial stats and adding CIs on the barplots
# understanding the output of the logistic regression
# histograms or something more intuitive than the dotplot I have to show relationship of light level and decision


