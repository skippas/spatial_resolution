# predicting light levels

library(tidyverse)
library(lubridate)
#### loading, cleaning and analyzing maze 1  ####
decisions <- readxl::read_xlsx("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/panama/data/2024/Spatial_resolution_experiment.xlsx",
                               sheet = "maze1", na = "NA",
                               range = "A1:T105")
decisions$t_ret <- format(decisions$t_ret, format = "%H:%M:%S")
decisions$ymaze <- "maze1"


m2decisions <- readxl::read_xlsx("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/panama/data/2024/Spatial_resolution_experiment.xlsx",
                                 sheet = "maze2", na = "NA",
                                 range = "A1:T103")
m2decisions$t_ret <- format(m2decisions$t_ret, format = "%H:%M:%S")
m2decisions$ymaze <- "maze2"

decisions <- decisions %>% select(!c(notes,
                                     vpatt_freq_dep,
                                     side_vpatt_dep,
                                     pos_vpatt_dep,
                                     t_dep,
                                     elapse_t_dep,
                                     elapse_t_ret
))  

m2decisions <- m2decisions %>% select(!c(notes,
                                         vpatt_freq_dep,
                                         side_vpatt_dep,
                                         pos_vpatt_dep,
                                         t_dep,
                                         elapse_t_dep,
                                         elapse_t_ret
)) 

decisions <- rbind(m2decisions, decisions)

# we want all data for which there is a return:
decisions <- filter(decisions, ! is.na(t_ret))

# any light levels that are funky, negative or have * will be converted to NA
decisions$light_level_ret <- as.numeric(decisions$light_level_ret) 
# keep light levels that have NA for light level proxy, as these are ok readings
excluded <- filter(decisions, ! is.na(light_level_issue))
excluded$light_level_type <- "predicted"
decisions <- filter(decisions, is.na(light_level_issue))
decisions$light_level_type <- "observed"

# later, we will want to bring excluded rows back and predict light levels for them.

# what do light levels look like?
decisions$t_ret <- as.POSIXct(hms::parse_hms(decisions$t_ret))
decisions$light_level_ret <- as.numeric(decisions$light_level_ret)

decisions %>%  filter(!is.na(light_level_ret)) %>%
  filter(t_ret > as.POSIXct(hms::parse_hms("17:00:00"))) %>%
  ggplot(aes(x = t_ret, y = log(light_level_ret)))+
  geom_point()+
  scale_x_time()+
  geom_smooth(method = 'lm') 

decisions %>%  filter(!is.na(light_level_ret)) %>%
  filter(t_ret < as.POSIXct(hms::parse_hms("17:00:00"))) %>%
  ggplot(aes(x = t_ret, y = log(light_level_ret)))+
  geom_point()+
  scale_x_time()+
  geom_smooth(method = 'lm') 
  
# outlier light levels, mistakes?


# estimate the relationship
# For afternoon

# For morning
filter(t_ret < as.POSIXct(hms::parse_hms("17:00:00"))) %>%


light_mod <- lm(log(light_level_ret)~t_ret, data = decisions)
summary(light_mod)

excluded$t_ret <- as.POSIXct(hms::parse_hms(excluded$t_ret))
m2excluded$t_ret <- as.POSIXct(hms::parse_hms(m2excluded$t_ret))

excluded$light_level_ret <- exp(predict(light_mod, excluded))

class(excluded$t_ret)
# much more sampling for 0.05
decisions %>% count(vpatt_freq_ret)

# what do correct vs incorrect decisions look like? 
decisions_freq <- decisions %>%
  group_by(vpatt_freq_ret, first_decision) %>%
  summarise(frequency = n())

# correct decisions across frequencies?
decisions %>% 
  ggplot(aes(x = as.character(vpatt_freq_ret),
             fill = as.character(first_decision))) +
  geom_bar(position = "dodge") 

decisions %>% 
  ggplot(aes(x = as.character(vpatt_freq_ret),
             fill = as.character(first_decision))) +
  geom_bar(position = "fill") 

# what do light levels look like?
# what do correct decisions across light levels look like?

# make light level numeric, changing cells that cannot be coerced (eg. bc they have alpha character eg. 0.2 / 2.0)
# make vpatt ret a character
decisions <- decisions %>% mutate(light_level_ret = as.numeric(light_level_ret),
                                  vpatt_freq_ret = as.character(vpatt_freq_ret))

# do light levels affect 1, 0 ?
mod1<- glm(first_decision ~ light_level_ret,
           data = decisions[decisions$vpatt_freq_ret == "0.15",], family = binomial)
summary(mod1)

mod2<- glm(first_decision ~ light_level_ret + vpatt_freq_ret,
           data = decisions, family = binomial)
summary(mod2)

# what do correct vs incorrect decisions look like? 
decisions_freq <- decisions %>%
  group_by(vpatt_freq_ret, first_decision) %>%
  summarise(frequency = n())

# correct decisions across frequencies?
p <- decisions %>% 
  filter(first_decision == "1" | first_decision == "0",
         vpatt_freq_ret != "NA") %>%
  ggplot(aes(x = as.character(vpatt_freq_ret),
             fill = as.character(first_decision))) +
  geom_bar(position = "dodge")+
  labs(fill = "first decision",
       x = "pattern frequency")+
  theme_light(base_size = 25)
ggsave("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/my_phd/images/barplot_first_decisions_pattern_at_315mm.png", plot = p, width = 10, height = 8, dpi = 300)

decisions %>% 
  filter(first_decision == "1" | first_decision == "0",
         vpatt_freq_ret != "NA") %>%
  ggplot(aes(x = as.character(vpatt_freq_ret),
             fill = as.character(first_decision))) +
  geom_bar(position = "fill")+
  labs(fill = "first decision",
       x = "pattern frequency")


