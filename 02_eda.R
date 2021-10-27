####### Prelim analysis of experiment data ------------

## Import libraries
library(readxl)
library(dplyr)
library(tidyverse)
library(MASS)
library(ggplot2)
library(viridis)
library(cowplot)

## Load data
pre_survey <- read_csv("data/Clean_data/pre_survey.csv")
experiment_rounds <- read_csv("data/Clean_data/all_conditions.csv")
experiment_round_A <- read_csv("data/Clean_data/ConditionA.csv")
experiment_round_B <- read_csv("data/Clean_data/ConditionB.csv")
experiment_round_C <- read_csv("data/Clean_data/ConditionC.csv")
post_survey <- read_csv("data/Clean_data/post_survey.csv")

## Pre-survey ------------
summary(pre_survey)

# Age distribution
mean(pre_survey$Q3.2)
max(pre_survey$Q3.2)
min(pre_survey$Q3.2)
## Mean age: 30.59
## Max 66, Min 18

ggplot(pre_survey, aes(x = Q3.2)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Age distribution", x = "Age", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("figures/age_dist.png")

# Gender distribution
ggplot(pre_survey, aes(x = Q3.1, fill = Q3.1)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Gender distribution", x = "Gender", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("figures/gender_dist.png")

# Education distribution
pre_survey %>% 
  mutate(Q3.3 = factor(Q3.3, levels = c("High school", "Bachelor's degree", "Master's degree", "PhD"))) %>% 
    ggplot(aes(x = Q3.3, fill = Q3.3)) + 
    geom_bar() +
    scale_x_discrete(labels = labels_barchart, drop = FALSE) +
    scale_fill_brewer(palette="Set2") +
    labs(title= "Education distribution", x = "", y = "Count") + #x = "Highest level of education",
    theme_minimal() +
    theme(legend.position = "none")

ggsave("figures/educ_dist.png")

### Propensity to trust ------------
levels_barchart = c("1", "2", "3", "4", "5", "6", "7")
colors_barchart = c("1" = "#E69F00", "2" = "#56B4E9", "3" = "#009E73", "4" = "#F0E442", "5" = "#0072B2", "6" = "#D55E00", "7" = "#CC79A7", "NA" = "#999999")
labels_barchart = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")
#colors_barchart = c("Strongly disagree" = "#E69F00", "Disagree" = "#56B4E9", "Somewhat disagree" = "#009E73", "Neither agree nor disagree" = "#F0E442", "Somewhat agree" = "#0072B2", "Agree" = "#D55E00", "Strongly agree" = "#CC79A7", "NA" = "#999999")

#Faith in general technology Q4.2 - Q4.5
pre_survey %>%
  mutate(Q4.2 = factor(Q4.2, levels_barchart)) %>%
    ggplot(aes(x = Q4.2, fill = Q4.2)) +
    geom_bar() +
    scale_x_discrete(labels = labels_barchart, drop = FALSE) +
    scale_fill_manual("legend", values = colors_barchart) +
    labs(title= "Faith in general technology Q4.2", x = "Q4.2", y = "Count") +
    theme_minimal() +
    theme(legend.position = "none") + 
    ylim(0, 15) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))

ggsave("figures/pre_survey_edaplots/PTT_FGT_Q4.2.png")

pre_survey %>%
  mutate(Q4.3 = factor(Q4.3, levels_barchart)) %>%
    ggplot(aes(x = Q4.3, fill = Q4.3)) +
    geom_bar() +
    scale_x_discrete(labels = labels_barchart, drop = FALSE) +
    scale_fill_manual("legend", values = colors_barchart) +
    labs(title= "Faith in general technology Q4.3", x = "Q4.3", y = "Count") +
    theme_minimal() +
    theme(legend.position = "none") + 
    ylim(0, 15) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))

ggsave("figures/pre_survey_edaplots/PTT_FGT_Q4.3.png")

pre_survey %>%
  mutate(Q4.4 = factor(Q4.4, levels_barchart)) %>%
    ggplot(aes(x = Q4.4, fill = Q4.4)) +
    geom_bar() +
    scale_x_discrete(labels = labels_barchart, drop = FALSE) +
    scale_fill_manual("legend", values = colors_barchart) +
    labs(title= "Faith in general technology Q4.4", x = "Q4.4", y = "Count") +
    theme_minimal() +
    theme(legend.position = "none") + 
    ylim(0, 15) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))

ggsave("figures/pre_survey_edaplots/PTT_FGT_Q4.4.png")

pre_survey %>%
  mutate(Q4.5 = factor(Q4.5, levels_barchart)) %>%
    ggplot(aes(x = Q4.5, fill = Q4.5)) +
    geom_bar() +
    scale_x_discrete(labels = labels_barchart, drop = FALSE) +
    scale_fill_manual("legend", values = colors_barchart) +
    labs(title= "Faith in general technology Q4.5", x = "Q4.5", y = "Count") +
    theme_minimal() +
    theme(legend.position = "none") + 
    ylim(0, 15) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))

ggsave("figures/pre_survey_edaplots/PTT_FGT_Q4.5.png")

#Trusting stance Q5.2 - Q5.4

pre_survey %>%
  mutate(Q5.2 = factor(Q5.2, levels_barchart)) %>%
  ggplot(aes(x = Q5.2, fill = Q5.2)) +
  geom_bar() +
  scale_x_discrete(labels = labels_barchart, drop = FALSE) +
  scale_fill_manual("legend", values = colors_barchart) +
  labs(title= "Trusting stance Q5.2", x = "Q5.2", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none") + 
  ylim(0, 15) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))

ggsave("figures/pre_survey_edaplots/PTT_TS_Q5.2.png")

pre_survey %>%
  mutate(Q5.3 = factor(Q5.3, levels_barchart)) %>%
  ggplot(aes(x = Q5.3, fill = Q5.3)) +
  geom_bar() +
  scale_x_discrete(labels = labels_barchart, drop = FALSE) +
  scale_fill_manual("legend", values = colors_barchart) +
  labs(title= "Trusting stance Q5.3", x = "Q5.3", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none") + 
  ylim(0, 15) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))

ggsave("figures/pre_survey_edaplots/PTT_TS_Q5.3.png")

pre_survey %>%
  mutate(Q5.4 = factor(Q5.4, levels_barchart)) %>%
  ggplot(aes(x = Q5.4, fill = Q5.4)) +
  geom_bar() +
  scale_x_discrete(labels = labels_barchart, drop = FALSE) +
  scale_fill_manual("legend", values = colors_barchart) +
  labs(title= "Trusting stance Q5.4", x = "Q5.4", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none") + 
  ylim(0, 15) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))

ggsave("figures/pre_survey_edaplots/PTT_TS_Q5.4.png")

## Experiment rounds ------------
summary(experiment_rounds)

#Donation per condition
ggplot(experiment_rounds, aes(x = Q2.1)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "none")
# same for all - everyone consented ones participating

ggsave("figures/exp_rounds_edaplots/donation.png")


#Willingness per condition
#check this
ggplot(experiment_rounds, aes(x = Condition, y = Q2.2_1, fill = Condition)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, 10) +
  labs(title = "Donation willingness per condition", x = "", y = "Scale") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("figures/exp_rounds_edaplots/willingness_per_cond.png")

#Willingness per round
#check this
ggplot(experiment_rounds, aes(x = Round, y = Q2.2_1, fill = Round)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, 10) +
  labs(title = "Donation willingness per round", x = "", y = "Scale") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("figures/exp_rounds_edaplots/willingness_per_round.png")

summary(experiment_round_A)
summary(experiment_round_B)
summary(experiment_round_C)


#Trust in specific technology Q3.2 - Q3.7
#Reliability and functionality

experiment_rounds %>%
  mutate(Q3.2 = factor(Q3.2, levels = levels_barchart)) %>%
    ggplot(aes(x = Q3.2, fill = Q3.2)) +
    geom_bar() +
    scale_x_discrete(labels = labels_barchart, drop = FALSE) +
    scale_fill_manual("legend", values = colors_barchart) +
    labs(title= "Trust in specific technology Q3.2 Rel1", x = "Q3.2", y = "Count") +
    theme_minimal() +
    theme(legend.position = "none") + 
    ylim(0, 10) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5)) +
      facet_wrap(~Condition)

ggsave("figures/exp_rounds_edaplots/TST_Q3.2_Rel1.png")

experiment_rounds %>%
  mutate(Q3.3 = factor(Q3.3, levels = levels_barchart)) %>%
    ggplot(aes(x = Q3.3, fill = Q3.3)) +
    geom_bar() +
    scale_x_discrete(labels = labels_barchart, drop = FALSE) +
    scale_fill_manual("legend", values = colors_barchart) +
    labs(title= "Trust in specific technology Q3.3 Rel2", x = "Q3.3", y = "Count") +
    theme_minimal() +
    theme(legend.position = "none") + 
    ylim(0, 10) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5)) +
      facet_wrap(~Condition)

ggsave("figures/exp_rounds_edaplots/TST_Q3.3_Rel2.png")

experiment_rounds %>%
  mutate(Q3.4 = factor(Q3.4, levels = levels_barchart)) %>%
  ggplot(aes(x = Q3.4, fill = Q3.4)) +
  geom_bar() +
  scale_x_discrete(labels = labels_barchart, drop = FALSE) +
  scale_fill_manual("legend", values = colors_barchart) +
  labs(title= "Trust in specific technology Q3.4 Rel4", x = "Q3.3", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none") + 
  ylim(0, 10) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5)) +
    facet_wrap(~Condition)

ggsave("figures/exp_rounds_edaplots/TST_Q3.4_Rel4.png")

experiment_rounds %>%
  mutate(Q3.5 = factor(Q3.5, levels = levels_barchart)) %>%
  ggplot(aes(x = Q3.5, fill = Q3.5)) +
  geom_bar() +
  scale_x_discrete(labels = labels_barchart, drop = FALSE) +
  scale_fill_manual("legend", values = colors_barchart) +
  labs(title= "Trust in specific technology Q3.5 Func1", x = "Q3.5", y = "Count") +
  theme_minimal() +
  ylim(0, 10) +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5)) +
    facet_wrap(~Condition)

ggsave("figures/exp_rounds_edaplots/TST_Q3.5_Func1.png")

experiment_rounds %>%
  mutate(Q3.6 = factor(Q3.6, levels = levels_barchart)) %>%
  ggplot(aes(x = Q3.6, fill = Q3.6)) +
  geom_bar() +
  scale_x_discrete(labels = labels_barchart, drop = FALSE) +
  scale_fill_manual("legend", values = colors_barchart) +
  labs(title= "Trust in specific technology Q3.6 Func2", x = "Q3.6", y = "Count") +
  theme_minimal() +
  ylim(0, 10) +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5)) +
    facet_wrap(~Condition)

ggsave("figures/exp_rounds_edaplots/TST_Q3.6_Func2.png")

experiment_rounds %>%
  mutate(Q3.7 = factor(Q3.7, levels = levels_barchart)) %>%
  ggplot(aes(x = Q3.7, fill = Q3.7)) +
  geom_bar() +
  scale_x_discrete(labels = labels_barchart, drop = FALSE) +
  scale_fill_manual("legend", values = colors_barchart) +
  labs(title= "Trust in specific technology Q3.7 Func3", x = "Q3.7", y = "Count") +
  theme_minimal() +
  ylim(0, 10) +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5)) +
    facet_wrap(~Condition)

ggsave("figures/exp_rounds_edaplots/TST_Q3.7_Func3.png")

## Post-survey ------------
summary(post_survey)

#Q4.3_1 How confident did you feel about donating data for version 1? - Version 1
#Q4.4_1 How confident did you feel about donating data for version 2? - Version 2
#Q4.5_1 How confident did you feel about donating data for version 3? - Version 3
box_plotA <- ggplot(post_survey, aes(x = "Condition A", y = Q4.3_1, fill = "Condition A")) +
  geom_boxplot() +
  scale_fill_manual(values = "#E69F00") +
  labs(x = "", y = "") +
  theme_minimal() + 
  ylim(0, 10) +
  theme(legend.position = "none")

box_plotB <- ggplot(post_survey, aes(x = "Condition B", y = Q4.4_1, fill = "Conition B")) +
  geom_boxplot() +
  scale_fill_manual(values = "#56B4E9") +
  labs(x = "", y = "") +
  theme_minimal() + 
  ylim(0, 10) +
  theme(legend.position = "none")

box_plotC <- ggplot(post_survey, aes(x = "Condition C", y = Q4.5_1, fill = "Conition C")) +
  geom_boxplot() +
  scale_fill_manual(values = "#009E73") +
  labs(x = "", y = "") +
  theme_minimal() + 
  ylim(0, 10) +
  theme(legend.position = "none")

plot_grid(box_plotA, box_plotB, box_plotC, ncol = 3)

ggsave("figures/post_survey_edaplots/version_conf.png")

#Q4.6 Which version made you feel the most confident about giving consent to donate data to the research study?
ggplot(post_survey, aes(x = Q4.6, fill = Q4.6)) +
  geom_bar() +
  #scale_fill_brewer("Set2") +
  labs(title = "Version preference", x = "", y = "Count") +
  theme_minimal() + 
  ylim(0, 8) +
  theme(legend.position = "none")

ggsave("figures/post_survey_edaplots/version_pref.png")

#Q5.1 Willingness to donate again
ggplot(post_survey, aes(x = Q5.1, fill = Q5.1)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  labs(title = "Willingness to donate again", x = "", y = "Count") +
  theme_minimal() + 
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))

ggsave("figures/post_survey_edaplots/willingness_donate_again.png")
