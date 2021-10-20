####### Prelim analysis of experiment data ------------

## Import libraries
library(readxl)
library(dplyr)
library(tidyverse)
library(MASS)
library(ggplot2)
library(viridis)

## Load data
pre_survey <- read_csv("data/Clean_data/pre_survey")
experiment_rounds <- read_csv("data/Clean_data/all_conditions")
experiment_round_A <- read_csv("data/Clean_data/ConditionA")
experiment_round_B <- read_csv("data/Clean_data/ConditionB")
experiment_round_C <- read_csv("data/Clean_data/ConditionC")
post_survey <- read_csv("data/Clean_data/post_survey")

## Pre-survey ------------
summary(pre_survey)

# Age distribution
mean(pre_survey$Q3.2)
max(pre_survey$Q3.2)
min(pre_survey$Q3.2)
## Mean age: 30.59
## Max 66, Min 18

ggplot(pre_survey, aes(x = Q3.2, fill = Q3.2)) +
  geom_density() +
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
    scale_fill_brewer(palette="Set2") +
    labs(title= "Education distribution", x = "", y = "Count") + #x = "Highest level of education",
    theme_minimal() +
    theme(legend.position = "none")

ggsave("figures/educ_dist.png")

### Trust in general technology / Propensity to trust ------------

ggplot(pre_survey, aes(x = Q4.2, fill = Q4.2)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  #labs(title= "Education distribution", x = "Highest level of education", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")




## Experiment rounds ------------
summary(experiment_rounds)

#Donation per condition
ggplot(experiment_rounds, aes(x = Q2.1, fill = Condition)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "none")
# same for all - everyone consented ones participating

#Willingness per condition
ggplot(experiment_rounds, aes(x = Condition, y = Q2.2_1, fill = Condition)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, 10) +
  labs(title = "Donation willingness per condition", x = "", y = "Scale") +
  theme_minimal() +
  theme(legend.position = "none")

#Willingness per round
ggplot(experiment_rounds, aes(x = Round, y = Q2.2_1, fill = Round)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, 10) +
  labs(title = "Donation willingness per round", x = "", y = "Scale") +
  theme_minimal() +
  theme(legend.position = "none")

summary(experiment_round_A)
summary(experiment_round_B)
summary(experiment_round_C)

## Post-survey ------------
summary(post_survey)

#Q4.3_1 How confident did you feel about donating data for version 1? - Version 1
#Q4.4_1 How confident did you feel about donating data for version 1? - Version 2
#Q4.5_1 How confident did you feel about donating data for version 1? - Version 3
ggplot(post_survey, aes(y = Q4.5_1)) +
  geom_boxplot() +
  #scale_fill_brewer("Set2") +
  labs(title = "Confidence preference", x = "", y = "Scale") +
  theme_minimal() + 
  #ylim(0, 10) +
  theme(legend.position = "none")

#Q4.6 Which version made you feel the most confident about giving consent to donate data to the research study?
ggplot(post_survey, aes(x = Q4.6, fill = Q4.6)) +
  geom_bar() +
  #scale_fill_brewer("Set2") +
  labs(title = "Version preference", x = "", y = "Count") +
  theme_minimal() + 
  ylim(0, 8) +
  theme(legend.position = "none")
