# exploratory and descriptive plots of the trial data
# Needs joint (join_2024-25_datasets.R) and model_data (joint_model.R).
source("../shared/theme_paper.R")

# what light levels are they returning at? 
p_light_session <- joint %>% 
  ggplot(aes(x = light_pred, fill = session))+
  geom_histogram(colour = "grey30")+
  scale_fill_viridis_d(option = "G", direction = -1)+
  theme_bw()

p_light_returns <- joint %>% 
  ggplot(aes(x = light_pred))+
  geom_histogram(fill = "#3F7D4E", colour = "grey10")+
  scale_fill_viridis_d(option = "G", direction = -1)+
  scale_x_continuous(limits = c(-4, 2), breaks = seq(-4,2)) +
  labs(x = "Returning light level log", 
       y = "Number of nest returns per light bin") +
  theme_bw()

mean(joint$light_pred)
sd(joint$light_pred)

# proportion of choices for nest by light level, one panel per frequency ####
# all trials at 90 % contrast, from both experiments; styled like the CS figure
# decisionByLightLevel_FacetByCont (contrast_sensitivity_analysis/scripts/plotting.R)
dec_plot <- model_data %>%
  filter(contrast_ret == contrast_pred) %>%
  mutate(light_bins = light_bin(light_pred))   # shared with the CS figure (theme_paper.R)

bin_labels <- dec_plot %>%
  group_by(patt_freq_ret, light_bins) %>%
  summarise(n = n(), .groups = "drop")

freqs <- sort(unique(dec_plot$patt_freq_ret))
freq_labels <- setNames(paste0(freqs, " cycles/°"), freqs)

p_decisions <- dec_plot %>%
  ggplot(aes(x = light_bins, fill = as.character(first_decision)))+
  geom_bar(position = "fill", colour = "black", linewidth = 0.3) +
  scale_fill_choice() +
  # single count at the top of each full bar
  geom_text(data = bin_labels,
            aes(x = light_bins, y = 0.9, label = n),
            inherit.aes = FALSE,
            size = 8, size.unit = "pt") +
  geom_hline(yintercept = 0.5, lty = 2)+
  scale_x_discrete(drop = FALSE)+   # every bin in every figure
  scale_y_performance() +
  labs(x = expression("Binned"~log[10]~"illuminance (lx)"))+
  # ten frequencies across both experiments: two rows so the panels stay readable
  facet_wrap(facets = "patt_freq_ret", nrow = 2,
             labeller = labeller(patt_freq_ret = freq_labels))+
  theme_classic(base_size = 8)+
  theme(legend.key.spacing.y = unit(0, "mm"),   # keys touch (ggplot2 ≥ 3.5)
        legend.key.width     = unit(4, "mm"),
        legend.key.height    = unit(6, "mm"),
        strip.background = ggh4x::element_part_rect(side = "b", colour = "black", fill = NA))

ggsave("../../writing/figures/resolution/decisionByLightLevel_FacetByPattFreq.pdf",
       device = cairo_pdf, plot = p_decisions,
       width = 180, height = 120, units = "mm")
# plot object for the combined SR + CS figure (analyses/combine_figures.R)
dir.create("../../writing/figures/objects", showWarnings = FALSE)
saveRDS(p_decisions, "../../writing/figures/objects/sr_decisions.rds")
