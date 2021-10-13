####### Analysis of experiment data ------------

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

## Pre-survey
summary(pre_survey)

ggplot(pre_survey) +
  aes(x = Group, y = Q3.2) +
  geom_boxplot() +
  theme(legend.position = "none")

## Experiment rounds
summary(experiment_rounds)

ggplot(experiment_rounds) +
  aes(x = Condition, y = Q2.2_1) +
  geom_boxplot() +
  theme(legend.position = "none")

ggplot(experiment_rounds) +
  aes(x = Condition, y = Q2.2_1, color = Round) +
  geom_jitter()
  #theme(legend.position = "none")

summary(experiment_round_A)
summary(experiment_round_B)
summary(experiment_round_C)

## Post-survey
summary(post_survey)

ggplot(post_survey) +
  aes(x = Condition, y = Q4.6) +
  geom_boxplot() +
  theme(legend.position = "none")
