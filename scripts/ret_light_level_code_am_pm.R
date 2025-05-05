# returning light level density plot am / pm and overall

p <- combined_dataset %>% #filter(session != "overall") %>%
  ggplot(aes(x = factor(session, level = c("pm", "am", "overall")), y = light_pred,
             colour = session, fill = session)) + 
  ggdist::stat_halfeye(
    alpha = 1,
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.1, 
    point_colour = NA
  ) + 
  geom_point(
    size = 1.5,
    alpha = .2,
    position = position_jitter(
      seed = 1, width = .02
    )
  ) + 
  stat_summary(
    geom = "text",
    fun = "mean",
    colour = "black",
    aes(label = round(after_stat(y), 2)),
    fontface = "bold",
    size = 4,
    vjust = -1.5
  ) +
  scale_colour_manual(
    values = colour_mapping, labels = custom_labels)+
  scale_fill_manual(
    values = colour_mapping, labels = custom_labels)+
  labs(
    x = NULL,
    y = expression("Returning light level"~"("~log[10]~"lux"~")"),
    size = 13)+
  scale_x_discrete(
    labels = c("pm" = "Evening", "am" = "Morning", "overall" = "Overall"))+
  coord_flip()+
  theme_dark()+
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),  # Bold and enlarge x-axis ticks
    axis.text.y = element_text(size = 13, face = "bold"),   # Bold and enlarge y-axis ticks
    panel.grid.major.y = element_blank(),  # Remove major horizontal grid lines
    panel.grid.minor.y = element_blank()   # Remove minor horizontal grid lines
  )
ggsave("plots/returning_light_level_am_pm_overall.png",
       plot = p, width = 5, height = 4, dpi = 300)
