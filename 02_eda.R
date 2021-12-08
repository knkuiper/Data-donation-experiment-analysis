####### Prelim analysis of experiment data ------------

## Import libraries
library(readxl)
library(dplyr)
library(tidyverse)
library(MASS)
library(ggplot2)
library(viridis)
library(cowplot)
library(stringr)

## Load data
pre_survey <- read_csv("data/Clean_data/pre_survey.csv")
experiment_round_A <- read_csv("data/Clean_data/ConditionA.csv")
experiment_round_B <- read_csv("data/Clean_data/ConditionB.csv")
experiment_round_C <- read_csv("data/Clean_data/ConditionC.csv")
experiment_rounds <- read_csv("data/Clean_data/binded_conditions.csv")
post_survey <- read_csv("data/Clean_data/post_survey.csv")
complete_data <- read_csv("data/Clean_data/complete_data.csv")

# colors and levels for plotting
levels_barchart = c("1", "2", "3", "4", "5", "6", "7")
colors_barchart = c("1" = "#E69F00", "2" = "#56B4E9", "3" = "#009E73", "4" = "#F0E442", "5" = "#0072B2", "6" = "#D55E00", "7" = "#CC79A7", "NA" = "#999999")
labels_barchart = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")
#colors_barchart = c("Strongly disagree" = "#E69F00", "Disagree" = "#56B4E9", "Somewhat disagree" = "#009E73", "Neither agree nor disagree" = "#F0E442", "Somewhat agree" = "#0072B2", "Agree" = "#D55E00", "Strongly agree" = "#CC79A7", "NA" = "#999999")

color_condA = c("#E69F00") #Condition A #E69F00
color_condB = c("#56B4E9") #Condition B #56B4E9
color_condC = c("#009E73") #Condition C #009E73
color_NA = c("#999999")
color_conditions = c(color_condA, color_condB, color_condC)
color_conditions_w_grey = c(color_NA, color_condA, color_condB, color_condC)

## Pre-survey ------------
summary(pre_survey)

# Age distribution
mean(complete_data$Q3.2_pre)
max(complete_data$Q3.2_pre)
min(complete_data$Q3.2_pre)
sd(complete_data$Q3.2_pre)
## Mean age: 33.04167
## Max 66, Min 24

ggplot(complete_data, aes(x = Q3.2_pre, fill = Q3.1_pre)) +
  geom_histogram(bins = 6, position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Age distribution", x = "Age", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# saved 02.11.21
# ggsave("figures/age_dist.png", width = 9, height = 9, bg = "white")

# Gender distribution
ggplot(pre_survey, aes(x = Q3.1_pre, fill = Q3.1_pre)) + 
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "", y = "Count") + # title = "Gender distribution", x = "Gender
  theme_minimal() +
  ylim(0,15) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 20), axis.text = element_text(size = 20))

# saved 15.11.21
# ggsave("figures/gender_dist.png", width = 9, height = 9, bg = "white")

# Education distribution
pre_survey %>% 
  mutate(Q3.3 = factor(Q3.3_pre, levels = c("High school", "Bachelor's degree", "Master's degree", "PhD"))) %>% 
    ggplot(aes(x = Q3.3_pre)) + #, fill = Q3.3_pre 
    geom_bar(fill = "cornflowerblue") +
    scale_x_discrete(drop = FALSE) +
    scale_fill_brewer(palette="Set2") +
    labs(x = "", y = "Count") + #title= "Education distribution", x = "Highest level of education",
    theme_minimal() +
    ylim(0,15) +
    theme(legend.position = "none") +
    theme(text = element_text(size = 20), axis.text = element_text(size = 20))

# saved 15.11.21
# ggsave("figures/educ_dist.png", width = 9, height = 9, bg = "white")

### Propensity to trust ------------

#Faith in general technology Q4.2 - Q4.5
pre_survey %>%
  mutate(Q4.2_pre = factor(Q4.2_pre, levels_barchart)) %>%
    ggplot(aes(x = Q4.2_pre)) +
    geom_bar(fill = "cornflowerblue") +
    scale_x_discrete(labels = labels_barchart, drop = FALSE) +
    labs(title= "", x = "Faith in general technology Q4.2", y = "Count") +
    theme_minimal() +
    theme(legend.position = "none") + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))

# saved 15.11.21
# ggsave("figures/pre_survey_edaplots/PTT_FGT_Q4.2.png", width = 9, height = 9, bg = "white")

pre_survey %>%
  mutate(Q4.3_pre = factor(Q4.3_pre, levels_barchart)) %>%
    ggplot(aes(x = Q4.3_pre)) +
    geom_bar(fill = "cornflowerblue") +
    scale_x_discrete(labels = labels_barchart, drop = FALSE) +
    scale_fill_manual("legend", values = colors_barchart) +
    labs(title= "", x = "Faith in general technology Q4.3", y = "Count") +
    theme_minimal() +
    theme(legend.position = "none") + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))

# saved 15.11.21
# ggsave("figures/pre_survey_edaplots/PTT_FGT_Q4.3.png", width = 9, height = 9, bg = "white")

pre_survey %>%
  mutate(Q4.4_pre = factor(Q4.4_pre, levels_barchart)) %>%
    ggplot(aes(x = Q4.4_pre, fill = Q4.4_pre)) +
    geom_bar(fill = "cornflowerblue") +
    scale_x_discrete(labels = labels_barchart, drop = FALSE) +
    scale_fill_manual("legend", values = colors_barchart) +
    labs(title= "", x = "Faith in general technology Q4.4", y = "Count") +
    theme_minimal() +
    theme(legend.position = "none") + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))

# saved 15.11.21
# ggsave("figures/pre_survey_edaplots/PTT_FGT_Q4.4.png", width = 9, height = 9, bg = "white")

pre_survey %>%
  mutate(Q4.5_pre = factor(Q4.5_pre, levels_barchart)) %>%
    ggplot(aes(x = Q4.5_pre, fill = Q4.5_pre)) +
    geom_bar(fill = "cornflowerblue") +
    scale_x_discrete(labels = labels_barchart, drop = FALSE) +
    scale_fill_manual("legend", values = colors_barchart) +
    labs(title= "", x = "Faith in general technology Q4.5", y = "Count") +
    theme_minimal() +
    theme(legend.position = "none") + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))

# saved 15.11.21
# ggsave("figures/pre_survey_edaplots/PTT_FGT_Q4.5.png", width = 9, height = 9, bg = "white")

#Trusting stance Q5.2 - Q5.4

pre_survey %>%
  mutate(Q5.2_pre = factor(Q5.2_pre, levels_barchart)) %>%
  ggplot(aes(x = Q5.2_pre)) +
  geom_bar(fill = "cornflowerblue") +
  scale_x_discrete(labels = labels_barchart, drop = FALSE) +
  scale_fill_manual("legend", values = colors_barchart) +
  labs(title= "", x = "Trusting stance Q5.2", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))

# saved 15.11.21
# ggsave("figures/pre_survey_edaplots/PTT_TS_Q5.2.png", width = 9, height = 9, bg = "white")

pre_survey %>%
  mutate(Q5.3_pre = factor(Q5.3_pre, levels_barchart)) %>%
  ggplot(aes(x = Q5.3_pre)) +
  geom_bar(fill = "cornflowerblue") +
  scale_x_discrete(labels = labels_barchart, drop = FALSE) +
  scale_fill_manual("legend", values = colors_barchart) +
  labs(title= "", x = "Trusting stance Q5.3", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))

# saved 15.11.21
# ggsave("figures/pre_survey_edaplots/PTT_TS_Q5.3.png", width = 9, height = 9, bg = "white")

pre_survey %>%
  mutate(Q5.4_pre = factor(Q5.4_pre, levels_barchart)) %>%
  ggplot(aes(x = Q5.4_pre)) +
  geom_bar(fill = "cornflowerblue") +
  scale_x_discrete(labels = labels_barchart, drop = FALSE) +
  scale_fill_manual("legend", values = colors_barchart) +
  labs(title= "", x = "Trusting stance Q5.4", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))

# saved 15.11.21
# ggsave("figures/pre_survey_edaplots/PTT_TS_Q5.4.png", width = 9, height = 9, bg = "white")

## Experiment rounds ------------
summary(experiment_rounds)

#Donation all rounds
ggplot(experiment_rounds, aes(x = Q2.1, fill = Q2.1)) +
  geom_bar(fill = "cornflowerblue") +
  labs(x = "Consent to donate") +
  theme_minimal() +
  theme(legend.position = "none")

#Donation per condition
donationA_plot <- 
  ggplot(complete_data, aes(x = Q2.1_condA, fill = Q2.1_condA)) +
  geom_bar() +
  scale_fill_manual(values = color_condA) +
  labs(x = "Condition A") +
  ylim(0, 25) +
  theme_minimal() +
  theme(legend.position = "none")

donationB_plot <-
  ggplot(complete_data, aes(x = Q2.1_condB, fill = Q2.1_condB)) +
  geom_bar() +
  #scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = color_condB) +
  labs(x = "Condition B") +
  ylim(0, 25) +
  theme_minimal() +
  theme(legend.position = "none")

donationC_plot <-
  ggplot(complete_data, aes(x = Q2.1_condC, fill = Q2.1_condC)) +
  geom_bar() +
  #scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = color_condC) +
  labs(x = "Condition C") +
  ylim(0, 25) +
  theme_minimal() +
  theme(legend.position = "none")

plot_grid(donationA_plot, donationB_plot, donationC_plot, nrow = 1)

# saved 15.11.21
# ggsave("figures/exp_rounds_edaplots/donation.png", width = 9, height = 9, bg = "white")

#Willingness per condition
aggregate(Q2.2_1 ~ Condition, 
          data = experiment_rounds,
          function(x) round(c(mean = mean(x), sd = sd(x)), 2)
          )

# Condition Q2.2_1.mean Q2.2_1.sd
# 1 Condition A        8.05      1.10
# 2 Condition B        7.59      1.53
# 3 Condition C        7.62      1.36

ggplot(experiment_rounds, aes(x = Condition, y = Q2.2_1, fill = Condition)) +
  geom_boxplot() +
  scale_fill_manual(values = color_conditions) +
  ylim(0, 10) +
  labs(title = "Donation willingness per condition", x = "", y = "Scale") +
  theme_minimal() +
  theme(legend.position = "none")

# saved 02.11.21
# ggsave("figures/exp_rounds_edaplots/willingness_per_cond.png")

#Willingness per round
aggregate(Q2.2_1 ~ Round, 
          data = experiment_rounds,
          function(x) round(c(mean = mean(x), sd = sd(x)), 2)
)

# Round Q2.2_1.mean Q2.2_1.sd
# 1 Round1        7.95      1.21
# 2 Round2        7.57      1.60
# 3 Round3        7.70      1.22

ggplot(experiment_rounds, aes(x = Round, y = Q2.2_1, fill = Round)) +
  geom_boxplot() +
  scale_fill_manual(values = color_conditions) +
  ylim(0, 10) +
  labs(title = "Donation willingness per round", x = "", y = "Scale") +
  theme_minimal() +
  theme(legend.position = "none")

# saved 02.11.21
# ggsave("figures/exp_rounds_edaplots/willingness_per_round.png")

willingnessA_plot <- 
  ggplot(complete_data, aes(y = Q2.2_1_condA, x = "Condition A", fill = "Condition A")) +
  geom_boxplot() +
  scale_fill_manual(values = color_condA) +
  ylim(0, 10) +
  labs(title = "Willingness cond A", x = "", y = "Scale") +
  theme_minimal() +
  theme(legend.position = "none")

willingnessB_plot <-
  ggplot(complete_data, aes(y = Q2.2_1_condB, x = "Condition B", fill = "Condition B")) +
  geom_boxplot() +
  scale_fill_manual(values = color_condB) +
  ylim(0, 10) +
  labs(title = "Willingness cond B", x = "", y = "Scale") +
  theme_minimal() +
  theme(legend.position = "none")

willingnessC_plot <-
  ggplot(complete_data, aes(y = Q2.2_1_condC, x = "Condition C", fill = "Condition C")) +
  geom_boxplot() +
  scale_fill_manual(values = color_condC) +
  ylim(0, 10) +
  labs(title = "Willingness cond C", x = "", y = "Scale") +
  theme_minimal() +
  theme(legend.position = "none")

plot_grid(willingnessA_plot, willingnessB_plot, willingnessC_plot, nrow = 1)

# saved 02.11.21
# ggsave("figures/exp_rounds_edaplots/willingness_per_cond_grid.png")

#Trust in specific technology Q3.2 - Q3.7
#Reliability and functionality

experiment_rounds %>%
  mutate(Q3.2 = factor(Q3.2, levels = levels_barchart)) %>%
    ggplot(aes(x = Q3.2)) +
    geom_bar(fill = "cornflowerblue") +
    scale_x_discrete(labels = labels_barchart, drop = FALSE) +
    scale_fill_manual("legend", values = colors_barchart) +
    labs(title= "Trust in specific technology Q3.2 Rel1", x = "Q3.2", y = "Count") +
    theme_minimal() +
    theme(legend.position = "top") + 
    theme(axis.text.x = element_text(angle = 45, vjust = .5, hjust=.5)) +
    facet_wrap(~Condition)

# saved 27.10.21
# ggsave("figures/exp_rounds_edaplots/TST_Q3.2_Rel1.png")

experiment_rounds %>%
  mutate(Q3.3 = factor(Q3.3, levels = levels_barchart)) %>%
    ggplot(aes(x = Q3.3)) +
    geom_bar(fill = "cornflowerblue") +
    scale_x_discrete(labels = labels_barchart, drop = FALSE) +
    scale_fill_manual("legend", values = colors_barchart) +
    labs(title= "Trust in specific technology Q3.3 Rel2", x = "Q3.3", y = "Count") +
    theme_minimal() +
    theme(legend.position = "none") + 
    theme(axis.text.x = element_text(angle = 45, vjust = .5, hjust=.5)) +
    facet_wrap(~Condition)

# saved 27.10.21
# ggsave("figures/exp_rounds_edaplots/TST_Q3.3_Rel2.png")

experiment_rounds %>%
  mutate(Q3.4 = factor(Q3.4, levels = levels_barchart)) %>%
  ggplot(aes(x = Q3.4)) +
  geom_bar(fill = "cornflowerblue") +
  scale_x_discrete(labels = labels_barchart, drop = FALSE) +
  scale_fill_manual("legend", values = colors_barchart) +
  labs(title= "Trust in specific technology Q3.4 Rel4", x = "Q3.3", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5)) +
  facet_wrap(~Condition)

# saved 27.10.21
# ggsave("figures/exp_rounds_edaplots/TST_Q3.4_Rel4.png")

experiment_rounds %>%
  mutate(Q3.5 = factor(Q3.5, levels = levels_barchart)) %>%
  ggplot(aes(x = Q3.5)) +
  geom_bar(fill = "cornflowerblue") +
  scale_x_discrete(labels = labels_barchart, drop = FALSE) +
  scale_fill_manual("legend", values = colors_barchart) +
  labs(title= "Trust in specific technology Q3.5 Func1", x = "Q3.5", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5)) +
  facet_wrap(~Condition)

# saved 27.10.21
# ggsave("figures/exp_rounds_edaplots/TST_Q3.5_Func1.png")

experiment_rounds %>%
  mutate(Q3.6 = factor(Q3.6, levels = levels_barchart)) %>%
  ggplot(aes(x = Q3.6)) +
  geom_bar(fill = "cornflowerblue") +
  scale_x_discrete(labels = labels_barchart, drop = FALSE) +
  scale_fill_manual("legend", values = colors_barchart) +
  labs(title= "Trust in specific technology Q3.6 Func2", x = "Q3.6", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5)) +
  facet_wrap(~Condition)

# saved 27.10.21
# ggsave("figures/exp_rounds_edaplots/TST_Q3.6_Func2.png")

experiment_rounds %>%
  mutate(Q3.7 = factor(Q3.7, levels = levels_barchart)) %>%
  ggplot(aes(x = Q3.7)) +
  geom_bar(fill = "cornflowerblue") +
  scale_x_discrete(labels = labels_barchart, drop = FALSE) +
  scale_fill_manual("legend", values = colors_barchart) +
  labs(title= "Trust in specific technology Q3.7 Func3", x = "Q3.7", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5)) +
  facet_wrap(~Condition)

# saved 27.10.21
# ggsave("figures/exp_rounds_edaplots/TST_Q3.7_Func3.png")

## Post-survey ------------
summary(post_survey)

#Q4.3_1 How confident did you feel about donating data for version 1? - Version 1
#Q4.4_1 How confident did you feel about donating data for version 2? - Version 2
#Q4.5_1 How confident did you feel about donating data for version 3? - Version 3
box_plotA <- ggplot(post_survey, aes(x = "Condition A", y = Q4.3_1_post, fill = "Condition A")) +
  geom_boxplot() +
  scale_fill_manual(values = color_condA) +
  labs(x = "", y = "") +
  theme_minimal() + 
  ylim(0, 10) +
  theme(legend.position = "none")

box_plotB <- ggplot(post_survey, aes(x = "Condition B", y = Q4.4_1_post, fill = "Conition B")) +
  geom_boxplot() +
  scale_fill_manual(values = color_condB) +
  labs(x = "", y = "") + 
  theme_minimal() + 
  ylim(0, 10) +
  theme(legend.position = "none")

box_plotC <- ggplot(post_survey, aes(x = "Condition C", y = Q4.5_1_post, fill = "Conition C")) +
  geom_boxplot() +
  scale_fill_manual(values = color_condC) +
  labs(x = "", y = "") +
  theme_minimal() + 
  ylim(0, 10) +
  theme(legend.position = "none")

plot_grid(box_plotA, box_plotB, box_plotC, ncol = 3)

# saved 02.11.21
# ggsave("figures/post_survey_edaplots/version_conf.png")

#Q4.6 Which version made you feel the most confident about giving consent to donate data to the research study?
ggplot(post_survey, aes(x = Q4.6_post)) +
  geom_bar(fill = "cornflowerblue") +
  #scale_fill_manual(values = color_conditions_w_grey) +
  labs(title = "", x = "", y = "Count") +
  scale_x_discrete(labels = str_wrap(
    c("Don't know / No preference", "BASELINE", "ABSTRACT", "CONCRETE"), width = 13), drop = FALSE) +
  theme_minimal() + 
  ylim(0, 8) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 17), axis.text = element_text(size = 17))

# saved 03.12.21
# ggsave("figures/post_survey_edaplots/version_pref.png", width = 9, height = 9, bg = "white")

#Q5.1 Willingness to donate again
ggplot(post_survey, aes(x = Q5.1_post)) +
  geom_bar(fill = "cornflowerblue") +
  scale_x_discrete(labels = str_wrap(
    c("No, I would probably not be willing to", 
      "Yes, I would definitely be willing to", 
      "Yes, I would probably be willing to"), width = 23), drop = FALSE) +
  labs(title = "", x = "", y = "Count") +
  theme_minimal() + 
  theme(legend.position = "none") +
  theme(text = element_text(size = 17), axis.text = element_text(size = 17))

# saved 03.12.21
 ggsave("figures/post_survey_edaplots/willingness_donate_again.png", width = 9, height = 9, bg = "white")

