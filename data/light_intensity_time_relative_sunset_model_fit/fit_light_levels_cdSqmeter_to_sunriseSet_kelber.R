# predict light levels in candela per square meter from almut kelbers data obtained 
# through webplotdigitizer
# Load necessary libraries
library(dplyr)
library(ggplot2)

light_data <- readxl::read_xlsx(
  "data/light_intensity_data/light_level_candela_kelber_data/light_level_relative_sunrise_and_set_forest_edge.xlsx",
                                         sheet = "Sheet1")
light_data <- rename(light_data, time_rel_setrise = time_relative_sunrise_or_set_minutes)

light_data <- light_data %>%
  mutate(light_level_candelas_per_sqmeter = as.numeric(light_level_candelas_per_sqmeter),
         time_rel_setrise = as.numeric(time_rel_setrise))

# Fit smooth lines for each curve
loess_light_lvl_fit_am_candela <- loess(log10(light_level_candelas_per_sqmeter) ~
                                  as.numeric(time_rel_setrise),
                                data = light_data[light_data$session == "am",])

loess_light_lvl_fit_pm_candela <- loess(log10(light_level_candelas_per_sqmeter) ~
                                  as.numeric(time_rel_setrise),
                                data = light_data[light_data$session == "pm",])

saveRDS(loess_light_lvl_fit_pm_candela, file = "predict_light_levels//loess_light_lvl_fit_pm_candela.rds")
saveRDS(loess_light_lvl_fit_am_candela, file = "predict_light_levels//loess_light_lvl_fit_am_candela.rds")

# check curves make sense
light_data <- light_data %>%
  mutate(
    light_pred =
      if_else(session == "am",
              predict(loess_light_lvl_fit_am_candela, light_data),
              if_else(session == "pm",
                      predict(loess_light_lvl_fit_pm_candela, light_data),
                      NA))) 

 light_data %>% 
  ggplot() +
  geom_point(aes(x = time_rel_setrise,
                                    y = log10(light_level_candelas_per_sqmeter),
                                    colour = curve), size = 1.5) +
  geom_line(aes(x = time_rel_setrise,
                 y = light_pred)) +
  facet_wrap(vars(session))+
  labs(x = "Time relative to sunrise (minutes)",
       y = expression(paste("Light Level (candela", " m"^"-2",")"))) +
  theme_minimal() 

 