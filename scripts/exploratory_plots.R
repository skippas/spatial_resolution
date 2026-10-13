# exploratory plots
# this will fail until the joining of cs and sr dataframes is turned into a script that can be called after loading cleaning
source("scripts/loading_cleaning.R")
source("scripts/join_2024-25_datasets.R")

# what light levels are they returning at? 
joint %>% 
  ggplot(aes(x = light_pred, fill = session))+
  geom_histogram(colour = "grey30")+
  scale_fill_viridis_d(option = "G", direction = -1)+
  theme_bw()

joint %>% 
  ggplot(aes(x = light_pred))+
  geom_histogram(fill = "#3F7D4E", colour = "grey10")+
  scale_fill_viridis_d(option = "G", direction = -1)+
  scale_x_continuous(limits = c(-4, 2), breaks = seq(-4,2)) +
  labs(x = "Returning light level log", 
       y = "Count of returns to nest") +
  theme_bw()

mean(joint$light_pred)
sd(joint$light_pred)
