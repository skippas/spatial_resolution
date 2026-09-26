library(tidyverse)
library(lubridate)

#### data from 17/04 . Testing 2nd position ####
rm(list = ls())
bin_fill <- c('1' = '#27b376', '0' = '#bf212f')
fill_scale <- scale_fill_manual(name = "first_decision", values = bin_fill)

#source("scripts/loading_cleaning_maze_data.r")
decisions <- read.csv("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/projects/panama/analyses/spatial_resolution/data/cleaned_light_level_appended/spatial_resolution_experiment_cleaned_light_level_appended.csv")

# filtering ####
# Filter out the training phases (patterns at pos 1, no alternation of side, or bee enters wrong hole)
# Only keep testing at pos 2 and get rid of NA decisions
# Only keep nests for which significant (enough testing that the influence of learning is low) testing occurred
decisions <- decisions %>% filter((nest == "2.6V" | nest == "1.5H" | nest == "1.4V" |
                                  nest == "2.8V" | nest == "2.9H") &
                            (training_or_testing != "training" | is.na(training_or_testing)) & 
                              pos_vpatt_ret == 2 &
                            date > "2024-04-16")
# ensure no NAs in decisions
sum(is.na(decisions$first_decision))

# check sampling balanced with regards to side and session of home pattern
table(decisions$patt_freq_ret, decisions$side_vpatt_ret,
      decisions$session, decisions$nest)
table(decisions$nest,  decisions$side_vpatt_ret, decisions$patt_freq_ret)
#####
# During the first phase of 0.05 testing for nest 1.4V, there was some uneven side-to-side testing for 0.05 pattern (other patterns even). Was this a training phase and i focused on the side they couldnt do, or kept them on that side until they could do it then started testing? 
# Perhaps surprising that it isnt 100% given turning freq data and bees were making mistakes on it. Although could be attributed to training or that lines dont really appear like lines, or to do with contrast per area of visual field.
# Because testing focused on the side that they had a turning bias towards, true p of 0.05 pattern underestimated
#####

# correct decisions faceted by pattern freq and individual
p <- decisions %>% #filter(patt_freq_ret != "0.3") %>%
  ggplot(aes(x = as.character(side_vpatt_ret),
             fill = as.character(first_decision)))+
  geom_bar(position = "stack", width = 0.5)+
  labs(x = "side of pattern on return")+
  facet_grid(cols = vars(nest), rows = vars(patt_freq_ret))+
  theme_gray(base_size = 25)+
  fill_scale
ggsave("plots/barp_facetNestxPatt_sideBias.png",
       plot = p, width = 10, height = 8, dpi = 300)
# prevent widening of bars with this: position = position_dodge(preserve = "single")
# same as above but proportions on y axis instead of counts
p <- decisions %>% 
  mutate(p_value = if_else(patt_freq_ret == 0.1,
                           "p = 0.012", NA)) %>%
  ggplot(aes(x = as.character(patt_freq_ret),
             fill = as.character(first_decision)))+
  geom_bar(position = "fill")+
  geom_hline(yintercept = 0.5, lty =2)+
  #geom_text(aes(label = p_value, y = 0.6), size = 8)+ # throws error bc other freq p values not shown
  labs(x = "Grating spatial frequency", y = "proportion of decisions")+
  facet_wrap(facets = "nest")+
  theme_gray(base_size = 25)+
  fill_scale

# statistics on proportions of decisions
# confidence intervals on bars: (I'll leave them off for now, wont look good without more data)
# decision_props <- with(decisions, table(patt_freq_ret, first_decision))
# binom.test(rev(decision_props[2,]), p = 0.5, alternative = "greater")
# confInts <- binom.test(rev(decision_props[2,]), p = 0.5, alternative = "two.sided")
# confInts["conf.int"]
# rm(decision_props, confInts)
# visualize decisions correct in bar plot, facetting by pattern freq and combining individuals
p <- decisions %>% 
  mutate(p_value = if_else(patt_freq_ret == 0.1,
                                      "p = 0.012", NA)) %>%
  ggplot(aes(x = as.character(patt_freq_ret),
             fill = as.character(first_decision)))+
  geom_bar(position = "fill")+
  geom_hline(yintercept = 0.5, lty =2)+
  #geom_text(aes(label = p_value, y = 0.6), size = 8)+ # throws error bc other freq p values not shown
  labs(x = "Grating spatial frequency", y = "proportion of decisions")+
  theme_gray(base_size = 25)+
  fill_scale
ggsave("plots/barp_propCorrectAcrossSF.png",
       plot = p, width = 10, height = 8, dpi = 300)
# show the overall plot with absolute sample sizes as well
p <- decisions %>% 
  ggplot(aes(x = as.character(patt_freq_ret),
             fill = as.character(first_decision))) +
  geom_bar(position = "dodge")+
  labs(x = "Grating spatial frequency")+
  theme_gray(base_size = 25)+
  fill_scale
ggsave("plots/barp_countsCorrectAcrossSF.png",
       plot = p, width = 10, height = 8, dpi = 300)

# find the videos for which the wrong turns are for 0.1 patterns and look at turning distances, also across light level
# You also could possibly look at correct decision turning distances? Srini didnt do this, but maybe bc you know the side pref you can? 
  
# Incorporating light level ####

# sampling count by session (am / pm)
p <- ggplot(decisions, aes(x = light_pred, fill = session))+
  geom_histogram(data = subset(decisions, session == 'am'),
                 aes(y = ..count..), binwidth = 1)+
  geom_histogram(data = subset(decisions, session == 'pm'),
                 aes(y = -..count..), binwidth = 1)+
  scale_y_continuous(labels = abs) + # To display positive counts on both sides
  labs(x = "Light Level", y = "Count")+
  scale_fill_grey(start = 0.8, end = 0.1)+
  labs(x = "Light level (log(lux))")+
  theme_classic(base_size = 25)
ggsave("plots/hist_lightLevelsOfReturnsBySession.png",
       plot = p, width = 10, height = 8, dpi = 300)
# PM sessions give you returns in lower light levels
# eveness of sampling across light level and pattern
totals <- decisions %>%
  group_by(patt_freq_ret, session) %>%
  summarise(total = n())
p <- ggplot(decisions, aes(x = light_pred, fill = session))+
  geom_histogram(data = subset(decisions, session == 'am'),
                 aes(y = ..count..), binwidth = 1)+
  geom_histogram(data = subset(decisions, session == 'pm'),
                 aes(y = -..count..), binwidth = 1)+
  geom_text(data = totals, aes(label = total, x = Inf,
                               y = ifelse(session == 'am', 6, -5),
                               color = session),  
            position = position_nudge(x = 1), hjust = 1,
            size = 8)+
  scale_y_continuous(labels = abs) + # To display positive counts on both sides
  labs(x = "Light Level", y = "Count")+
  scale_fill_grey(start = 0.6, end = 0.1)+
  scale_color_grey(start = 0.6, end = 0.1)+
  theme_classic(base_size = 25)+
  facet_wrap(facets = "patt_freq_ret")
ggsave("plots/hist_facetByPattFreq_lightLevelsOfReturnsBySession.png",
       plot = p, width = 10, height = 8, dpi = 300)
# I need to test 0.1 in PM and 0.3 in AM to balance out sampling
class(decisions$patt_freq_ret)
# back to back histogram 
means <- decisions %>%
  group_by(first_decision) %>%
  summarise(mean_light_pred = mean(light_pred), count = n())

p <- ggplot(decisions, aes(x = light_pred, fill = first_decision))+
  geom_histogram(data = subset(decisions, first_decision == '1'),
                 aes(y = ..count..), binwidth = 1) +
  geom_histogram(data = subset(decisions, first_decision == '0'),
                 aes(y = -..count..), binwidth = 1) +
  # geom_segment(data = means, aes(x = mean_light_pred, xend = mean_light_pred,
  #                                y = 0, yend = ifelse(first_decision == '1', 1, -1)),
  #              linetype = "solid", color = "black")+
  scale_y_continuous(labels = abs)+ # To display positive counts on both sides
  labs(x = "Light Level", y = "Count")+
  theme_gray(base_size = 25)+
  theme(legend.position='bottom')+
  fill_scale+
  facet_wrap(facets = "patt_freq_ret", scales = "free_y")
ggsave("plots/hist_facetByPattFreq_decisionsByLightLevel.png",
       plot = p, width = 10, height = 8, dpi = 300)

# as above but proportion on y-axis instead of count
# this would look a lot better if I could mirror it. But that may take some time
# calculate counts so can be added to bars: 
decisions_mod <- decisions %>%
  mutate(patt_freq_ret = paste0(patt_freq_ret, " cycles/°"))

p <- decisions_mod %>%
  ungroup() %>%
  ggplot(aes(x = light_pred, 
             fill = as.character(first_decision)))+
  geom_bar(position = "fill")+
  geom_text(stat = 'count', aes(label = ..count..),
            position = position_fill(vjust = 0.1), size = 5) +
  scale_x_binned(n.breaks = 6)+
  geom_hline(yintercept = 0.5, lty =2)+
  labs(x = "log light level", y = "proportion of decisions")+
  facet_wrap(facets = "patt_freq_ret", nrow = 1,
             labeller = labeller())+
  theme_minimal(base_size = 25)+
  theme(legend.position = 'top')+
  fill_scale
ggsave("plots/hist_facetByPattFreq_decisionPropByLightLevel.png",
       plot = p, width = 12, height = 8, dpi = 300)

# heat plot / tile / contour
# Bin the light_pred variable
light_sf_bin <- ungroup(decisions) %>%
    mutate(light_pred_bin = cut(light_pred, breaks = 6, 
                                include.lowest = TRUE, right = FALSE))

# Calculate the proportion of correct decisions for each combination of patt_freq_ret and light_pred_bin
light_sf_bin <- light_sf_bin %>%
    group_by(patt_freq_ret, light_pred_bin) %>%
    summarise(prob_correct = mean(as.numeric(first_decision)), .groups = 'drop',
              n = n())

# this version makes the legend accurate
library(scales)
p <- ggplot(light_sf_bin, aes(x = as.character(patt_freq_ret),
                         y = light_pred_bin, fill = prob_correct))+
  geom_tile()+
  geom_text(aes(label = round(prob_correct, 2), # from cedric scherer. color labels according to tile color
                color = abs(prob_correct) < .55),
            size = 5)+
  scale_fill_viridis_c(breaks = c(0, 0.5, 1),
                       rescaler = function(x, to = c(0, 1), from = NULL) {
    ifelse(x>0.5, 
           scales::rescale(x,
                           to = to,
                           from = c(0.5, 1)),
           0)})+
  scale_color_manual(
    values = c("black", "white"),
    guide = "none"
  ) +
  labs(x = "Pattern Frequency", y = "Light Level (Binned)",
       fill = "Probability Correct")+
  theme_minimal(base_size = 25)+
  theme(legend.position = 'top')
ggsave("plots/tilePlot_lightLevel_sf_probCorrect.png",
       plot = p, width = 10, height = 10, dpi = 300)

p <- ggplot(light_sf_bin, aes(x = as.character(patt_freq_ret),
                              y = light_pred_bin, fill = prob_correct))+
  geom_tile()+
  scale_x_discrete(limits = c('0.028', '0.04', '0.055', '0.07',
                              '0.082', '0.109', '0.164'))+
  scale_fill_viridis_c(rescaler = function(x, to = c(0, 1), from = NULL) {
    ifelse(x>0.5, 
           scales::rescale(x,
                           to = to,
                           from = c(0.5, 1)),
           0)})+
  labs(x = "Pattern Frequency", y = "Light Level (Binned)",
       fill = "Probability Correct")+
  theme_minimal(base_size = 25)+
  theme(legend.position = 'none')
ggsave("plots/tilePlot_lightLevel_sf_probCorrect_testingGaps.png",
       plot = p, width = 10, height = 8, dpi = 300)
# short code form of above, legend scale not ideal
p <- ggplot(light_sf_bin, aes(x = as.character(patt_freq_ret),
                              y = light_pred_bin, fill = prob_correct)) +
    geom_tile(color = "white") +
    scale_x_discrete()+
    scale_fill_viridis_c(limits = c(0.5, 1), oob = scales::squish) +
    labs(x = "Pattern Frequency", y = "Light Level (Binned)", 
         fill = "Probability Correct") +
    theme_minimal(base_size = 15)
  

# logistic regression suggests a relationship? logistic regression with SF and light level as factors
# light level will probably only affect a couple of SFs, as some finer ones will always result in 50/50 choice ratios
library(jtools)
mod1<- glm(as.numeric(first_decision) ~ light_pred + patt_freq_ret,
           data = decisions, family = binomial)
mod2<- glm(as.numeric(first_decision) ~ light_pred * patt_freq_ret,
           data = decisions, family = binomial)
summary(mod1)
summary(mod2)

# i was initially worried that light level effect would have vastly different slope depending on 
# value of spatial frequency (ie. flat at high freq and at low freq) and that light_intensity * sf
# wouldn't capture this. but logistic regression can produce an 'S' shaped curve which might capture this
# relationship quite nicely. I should simulate some data to check this sometime. 
# here ive categorized sf
# thinking about it again. i think that interaction would have problems,
# because effect of light level would need to be high at intermediate sfs, and low otherwise. that's not linear.
decisions <- decisions %>% group_by(patt_freq_ret) %>%
  mutate(n_per_sf = n(),
         patt_freq_ret = as.factor(patt_freq_ret))
mod3 <- glm(as.numeric(first_decision) ~ light_pred * patt_freq_ret,
                         data = decisions, family = binomial)
summary(mod3)
library(lsmeans)
m.lst <- lstrends(mod3, "patt_freq_ret", var="light_pred")
library(car)
Anova(mod3)

# code from https://stats.oarc.ucla.edu/r/dae/logit-regression/
light_pred <- mean(decisions$light_pred)
light_pred <- rep(c(light_pred, light_pred+1.5, light_pred-1.5), 5)
patt_freq_ret <- rep(unique(decisions$patt_freq_ret), 3)
predicted_p<- data.frame(light_pred, patt_freq_ret)
predicted_p<- left_join(predicted_p, unique(decisions[, c("patt_freq_ret", "n_per_sf")]))

predicted_p <- cbind(predicted_p, predict(mod3, newdata = predicted_p, type = "link",
                                    se = TRUE))
predicted_p <- within(predicted_p, {
  predicted_p <- plogis(fit)
  ll <- plogis(fit - (1.96 * se.fit)) # plogis is same as invlogit = function(x) 1/(1+exp(-x))
  ul <- plogis(fit + (1.96 * se.fit))
})

predicted_p <- arrange(predicted_p, patt_freq_ret, light_pred)
library(gt)
gt_tbl <- predicted_p[,c("patt_freq_ret","n_per_sf","light_pred","predicted_p",
                         "ul","ll")] %>%
  group_by(patt_freq_ret, n_per_sf) %>%
  mutate(patt_freq_ret = paste0(patt_freq_ret, " cpd"),
         n_per_sf = paste0("n = ", n_per_sf)) %>%
  gt() |>
  fmt_number(
    decimals = 2,
    use_seps = FALSE
  ) |>
  cols_move_to_start(columns = c(patt_freq_ret, light_pred, predicted_p)) |>
  cols_label(#patt_freq_ret = "Pattern spatial frequency (cpd)",
             light_pred = "Light intensity (log10(lux))",
             predicted_p = "Probability of a correct decision",
             ul = "97.5%", ll = "2.5%") |>
  tab_spanner(
    label = "Confidence levels",
    columns = c(ul, ll)) |>
  cols_width(
    light_pred ~ px(150),
    predicted_p ~ px(150),
    ll ~ px(80),
    ul ~ px(80)
  ) 
  # tab_header(
  #   title = "probability of a correct choice for different spatial frequencies at different light intensities ",
  # )
gtsave(gt_tbl, "log_model_predictions.png")

unit_labeller <- function(string) {
  labeled_string <- paste(string, "cpd")
  return(labeled_string)
}

p <- ggplot(decisions, aes(x = light_pred, y = as.numeric(first_decision))) +
  geom_jitter(alpha = 0.5, width = 0.02, height = 0.02) +  # Add jitter to the points
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              se = T, color = "grey70") +  # Logistic regression line
  geom_hline(yintercept = 0.5, lty = 2)+
  geom_point(data = predicted_p, aes(x = light_pred, y = predicted_p))+
  geom_errorbar(data = predicted_p, inherit.aes = F, width = 0.3,
                aes(x = light_pred, ymin = ll, ymax = ul))+
  geom_text(aes(x = 0.5, y = 0.125, size = 15,
                label = paste0("n = ", n_per_sf)))+
  #geom_vline(xintercept = mean(decisions$light_pred))+
  facet_wrap(vars(patt_freq_ret),
             scales = "fixed", nrow = 2,
             labeller = labeller(patt_freq_ret = unit_labeller)) +  # Separate plots for each category
  labs(x = "Light intensity (log10(lux))",
       y = "Probability of correct choice") +
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        strip.text = element_text(size = 15))
ggsave("plots/probability_correct_choice_as_function_light_intensity.png",
       plot = p, width = 10, height = 10, dpi = 300)

# do bees learn which side they are on as they depart? 
# Are probabilities of a correct decision higher when side is not swapped while they are gone versus when they are swapped?
# we dont want to include testing trials because it will bias results, because there were likely lots of correct decisions, and the side was not changed
decisions <- decisions %>%
  mutate(category = case_when(
    side_vpatt_dep == side_vpatt_ret ~ 'no_switch',
    side_vpatt_dep != side_vpatt_ret ~ 'switch',
    TRUE ~ 'other'
  ))

p <- decisions %>% 
  filter(category != 'other') %>%
  ggplot(aes(x = as.character(category),
             fill = as.character(first_decision)))+
  geom_bar(position = "fill")+
  geom_hline(yintercept = 0.5, lty =2)+
  #geom_text(aes(label = p_value, y = 0.6), size = 8)+ # throws error bc other freq p values not shown
  labs(x = "trial category", y = "proportion of decisions")+
  theme_gray(base_size = 25)+
  fill_scale
# slight effect (significant?), but possibly it is actually attributable to the 
# fact that no switches tend to also happen more often in sequential runs. ie. 
# it's actually the sequential runs on the same side that explains the increase in p correct decisions

# modelling probability as a function of continuous light level and spatial freq ####
decisions <- decisions %>% 
  mutate(patt_freq_ret = as.numeric(as.character(patt_freq_ret)),
         first_decision = as.numeric(first_decision)) # necessary for modelling

# creating prediction data
# light levels of interest to plot on the prob asfunc sp freq plot 
decisions <- decisions %>% filter(!is.na(light_pred))
overall_mean <- mean(decisions$light_pred)
light_lvl_range <- seq(min(decisions$light_pred), max(decisions$light_pred), by = 1)
light_lvl_range <-  c(light_lvl_range, overall_mean)

patt_freq_ret_range <- seq(
  from = floor(min(decisions$patt_freq_ret) * 1000) / 1000,  # Round down to nearest 0.0005
  to = ceiling(max(decisions$patt_freq_ret) * 1000) / 1000,  # Round up to nearest 0.0005
  by = 0.0005)

prediction_data <- data.frame(
  patt_freq_ret = rep(patt_freq_ret_range, times = length(light_lvl_range)),
  light_pred = rep(light_lvl_range, each = length(patt_freq_ret_range))
)

# create model
mod3<- glm(as.numeric(first_decision) ~ light_pred + patt_freq_ret,
           data = decisions, family = binomial)
summary(mod3)

# Finding value of sp freq (given light lvl) where p = 0.5. See Oysteins GLM notes
coefs <- coef(mod3)
sf_50_overall <- (- (coefs[1] + (coefs[2] * overall_mean))) / coefs[3]

prediction_list <- predict.glm(mod3, newdata = prediction_data,
                               type = "response", se.fit = T)
prediction_data <- prediction_data %>%
  mutate(
    probability = prediction_list$fit,
    se = prediction_list$se.fit,
    lower = probability - 1.96 * se,
    upper = probability + 1.96 * se
    ) 
 
# Plot using ggplot2
combined_dataset <- rbind(decisions %>% mutate(session = "overall"), decisions)
source("plot_code//plot_ret_light_level_am_pm.R") # moved this bc it takes lots of space
p

prediction_data %>%
  #filter(session == "overall") %>%
  ggplot(aes(x = patt_freq_ret, y = probability,
                            group = light_pred, colour = light_pred)) +
  coord_cartesian(ylim = c(0.5, 0.85), xlim = c(0.03,0.15)) + 
  geom_hline(yintercept = 0.5, lty = 2, size = 0.8)+
  geom_line(size = 1) +
  # geom_point(
  #   data = change_lightlvl,
  #   size = 3,
  #   shape = 21,
  #   colour = "black",
  #   stroke = 1)+
  # geom_text(
  #   data = change_lightlvl, 
  #   aes(x = patt_freq_ret, y = probability,
  #       label = paste0("P = ", round(probability, 2))), 
  #   hjust = -0.2,
  #   vjust = -0.5,
  #   fontface = "bold",
  #   size = 4) +
  # geom_point(
  #   data = change_spfreq,
  #   size = 3,
  #   shape = 21,
  #   colour = "black",
  #   stroke = 1)+
  # geom_text(
  #   data = change_spfreq,
  #   aes(x = patt_freq_ret, y = probability,
  #       label = paste0("P = ", round(probability, 2))), 
  #   hjust = -0.2, vjust = -0.5, fontface = "bold", size = 4) +
  # geom_point(
  #   data = chance_sfs, #%>% filter(session == "overall"),
  #   size = 3,
  #   shape = 21,
  #   colour = "black",
  #   stroke = 1)+
  # geom_text(
  #   data = chance_sfs %>% filter(session != "overall"),
  #   aes(label = paste0(round(patt_freq_ret, 2), " cpd")),
  #   vjust = -0.5,
  #   hjust = -0.05,
  #   fontface = "bold")+
  # geom_ribbon(
  #   aes(ymin = lower, ymax = upper),
  #   alpha = 0.2,
  #   colour = NA) +
  # custColScale+
  # custFillScale+
  scale_color_viridis_c(option = "D", direction = 1)+
  scale_x_continuous(breaks = seq(0.02, 0.16, 0.02))+
  labs(
    x = "Pattern frequency (cycles per degree)",
    y = "Home pattern choice probability",
    color = "Returning light level",
    fill = "Returning light level"
  ) +
  guides(colour = guide_legend(override.aes = list(linetype = "solid", label = "")))+
  theme_minimal() +
  theme_dark() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),
    legend.key = element_blank(),
    legend.position.inside = c(0.9, 0.9),   # Top-right corner (relative coordinates)
    legend.justification = c("right", "top"),  # Align legend box
    legend.background = element_rect(fill = "grey50", color = "grey30"), 
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5))
ggsave("plots/p_correct_asfunc_spfreq_and_lightlvl_am_pm_avg.png",
       plot = p, width = 5, height = 4, dpi = 300)

# illustration of line fitting with a logistic regression
# Convert light_pred to a factor with levels in ascending order
prediction_data$light_pred <- round(prediction_data$light_pred, 1)
prediction_data$light_pred <- factor(prediction_data$light_pred, 
                                     levels = sort(unique(prediction_data$light_pred),
                                                   decreasing = T))

# plot vertical lines
coefs<- coef(mod3)
# find patt freq ret values that give p = 0.5 at diff light levels
spfreq_est <- -(coefs["(Intercept)"] + coefs["light_pred"] * light_lvl_range) / coefs["patt_freq_ret"]
vlines <- data.frame(patt_freq_ret = spfreq_est,
                     light_pred = light_lvl_range)
vlines$light_pred <- round(vlines$light_pred, 1)
vlines$light_pred <- factor(
  vlines$light_pred, levels = sort(unique(vlines$light_pred),
                                   decreasing = T))

color_palette <- viridisLite::viridis(length(levels(prediction_data$light_pred)),
                                      option = "D", direction = -1)
named_colors <- setNames(color_palette, levels(prediction_data$light_pred))

set.seed(8)
p <- ggplot(decisions,
  aes(
    x = patt_freq_ret,
    y = as.numeric(first_decision))) + 
  geom_jitter(height = 0.04, width = 0.003, alpha = 0.5, colour = "white") + 
  geom_line(
    data = prediction_data[prediction_data$probability > 0.5, ],
    aes(
      y = probability,
      colour = light_pred,
      group = light_pred
    ), size = 0.8) +
  scale_x_continuous(
    breaks = unique(decisions$patt_freq_ret)  # Set breaks to actual x variable values
  ) +
  scale_colour_manual(
    values = named_colors
  )+
  # scale_color_viridis_d(
  #   option = "D",
  #   direction = -1)+
  geom_hline(yintercept = 0.5, lty = 2) + 
  geom_segment(data = vlines,
             aes(x = patt_freq_ret, 
                 y = 0,
                 yend = 0.5,
                 color = light_pred),
             linetype = "dashed",
             size = 0.8) +
  #geom_vline(xintercept = 0.055, lty = 3) +
  labs(
    x = "Pattern frequency (cycles per degree)",
    y = "Proportion of choices for home",
    color = expression(atop("Returning light level",~"("~log[10]~"lux"~")"))
  ) + 
 guides(colour = guide_legend(override.aes = list(linetype = "solid", label = ""))) + 
  theme_dark() + 
  theme(
    legend.position = "none",
    legend.background = element_rect(fill = "grey50", color = "grey30")  # Optional: Add border
  )
ggsave("plots/logistic_reg_illustration/p_correct_asfunc_spfreq_50_line_vmaxdrawn.png",
       plot = p, width = 5, height = 4, dpi = 300)

# Define a function to calculate the predictor value at P = 0.5
calc_spfreq_at_50 <- function(data, fixed_light_pred) {
  mod <- glm(as.numeric(first_decision) ~ patt_freq_ret + light_pred,
             data = data, family = binomial)
  coef <- coef(mod)
  spfreq_est <- -(coef["(Intercept)"] + coef["light_pred"] * fixed_light_pred) / coef["patt_freq_ret"]
  return(spfreq_est)
}

# Define a function for bootstrapping at a fixed light level
bootstrap_spfreq_at_50 <- function(data, fixed_light_pred, n_boot = 1000) {
  set.seed(123) 
  bootstrap_results <- replicate(n_boot, {
    boot_data <- data[sample(1:nrow(data), replace = TRUE), ]
    tryCatch(calc_spfreq_at_50(boot_data, fixed_light_pred), error = function(e) NA)
  }, simplify = TRUE)
  
  # Remove NA values and calculate confidence intervals
  bootstrap_results <- na.omit(bootstrap_results)
  ci_lower <- quantile(bootstrap_results, 0.025)
  ci_upper <- quantile(bootstrap_results, 0.975)
  
  return(tibble(
    fixed_light_pred = fixed_light_pred,
    estimate = mean(bootstrap_results, na.rm = TRUE),
    ci_lower = ci_lower,
    ci_upper = ci_upper
  ))
}

# List of fixed light levels (light_pred values)
light_lvl_range <- seq(min(decisions$light_pred), max(decisions$light_pred), by = 1)
# Apply the bootstrap function to each light level
results <- purrr::map_dfr(fixed_light_pred, ~ bootstrap_spfreq_at_50(decisions, .x))

p <- ggplot(results, aes(x = fixed_light_pred, y = estimate,
       colour = fixed_light_pred, fill = fixed_light_pred))+
  geom_ribbon(
    aes(ymin = ci_lower, ymax = ci_upper),
    colour = NA, fill = "white", alpha = 0.2)+
  geom_line(size = 1.2)+
  geom_point(shape = 21, size = 2, colour = "black")+
  scalecolourvid(option = "D", direction = 1)+
  scale_fill_viridis_c(option = "D", direction = 1)+
  labs(
    x = expression("Returning light level"~"("~log[10]~"lux"~")"),
    y = "Max detectable freq.\n(cycles per degree) ",
    color = "Returning light level"
  ) +
  theme_dark()+
  theme(
    legend.position = "none"
  )
ggsave("plots/logistic_reg_illustration/max_spfreq_asfunc_lightlvl.png",
       plot = p, width = 5, height = 4, dpi = 300)
  
# junk

# colour scales
custom_colours <- c(viridisLite::viridis(10)[c(10,3)], "white")
colour_mapping <- setNames(custom_colours, c("am", "pm", "overall"))
custColScale <- scale_colour_manual(
  values = colour_mapping,
  labels = custom_labels)
custFillScale <- scale_fill_manual(
  name = "session",
  values = colour_mapping,
  labels = custom_labels)

custom_labels <- c(
  "am" = bquote(.(round(am_mean, 2)) ~ log[10] ~ "(lux)" ~ "- morning avg"),
  "pm" = bquote(.(round(pm_mean, 2)) ~ log[10] ~ "(lux)" ~ " - evening avg"),
  "overall" = bquote(.(round(overall_mean, 2)) ~ log[10] ~ "(lux)" ~ " - overall avg")
)

chance_sfs <- data.frame(
  patt_freq_ret = c(sf_50_morn, sf_50_eve, sf_50_overall), 
  session = c("am", "pm", "overall"),
  probability = c(0.5, 0.5, 0.5))
# p change between 0.04 cpd and 0.06 for the evening slope
change_spfreq <- prediction_data %>% 
  filter(patt_freq_ret %in% c(0.04, 0.06) & session == "am")
# p change between morning and evening averages at 0.04 cpd
change_lightlvl <- prediction_data %>% 
  filter(patt_freq_ret %in% c(0.04))