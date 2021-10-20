####### Analysis of experiment data ------------

## Import libraries
library(readxl)
library(dplyr)
library(tidyverse)
library(MASS)
library(ggplot2)
library(ggpubr)
library(viridis)
library(car)

## Load data
pre_survey <- read_csv("data/Clean_data/pre_survey")
experiment_rounds <- read_csv("data/Clean_data/all_conditions")
experiment_cond_A <- read_csv("data/Clean_data/ConditionA")
experiment_cond_B <- read_csv("data/Clean_data/ConditionB")
experiment_cond_C <- read_csv("data/Clean_data/ConditionC")
post_survey <- read_csv("data/Clean_data/post_survey")

########################################################################
# H1 Trust
########################################################################

###Pre-survey
#Propensity to trust (PTT)

#Faith in general technology Q4.2 - Q4.5

#Trusting stance Q5.2 - Q5.4


###Experiment rounds
#Reliability and functionality
#Trust in specific technology Q3.2 - Q3.7




#Scale data
#Friedman test

#normality
shapiro.test(dat_hist$value)






########################################################################
# H2 Willingness (maximize consent)
# Q2.2_1 in experiment rounds
########################################################################

willingness_level <- experiment_rounds %>%
  dplyr::select(2, 11, 13)

#Willingness per condition boxplot
ggplot(willingness_level, aes(x = Condition, y = Q2.2_1, fill = Condition)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, 10) +
  labs(title = "Donation willingness per condition", x = "", y = "Scale") +
  theme_minimal() +
  theme(legend.position = "none")

#ANOVA one-way
h2_res_aov <- aov(Q2.2_1 ~ Condition, data = willingness_level)

summary(h2_res_aov)
#             Df Sum Sq Mean Sq F value Pr(>F)
# Condition    2   3.12   1.560   0.843  0.436
# Residuals   59 109.22   1.851               
# 1 observation deleted due to missingness

# Not statistically significant, cannot reject the null hypothesis

par(mfrow = c(1, 2)) # combine plots

# histogram
hist(h2_res_aov$residuals)

# QQ-plot
qqPlot(h2_res_aov$residuals, 
       id = FALSE # id = FALSE to remove point identification
)

#Normality
shapiro.test(experiment_rounds$Q2.2_1)
#       Shapiro-Wilk normality test
# 
# data:  experiment_rounds$Q2.2_1
# W = 0.83324, p-value = 7.275e-07

plot(h2_res_aov, which = 2)

#not assume normality
kruskal.test(Q2.2_1 ~ Condition, data = experiment_rounds)

#Willingness per condition
ggplot(experiment_rounds, aes(x = Condition, y = Q2.2_1, fill = Condition)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, 10) +
  labs(title = "Donation willingness per condition", x = "", y = "Scale") +
  theme_minimal() +
  theme(legend.position = "none")

# descriptive statistics 
aggregate(Q2.2_1 ~ Condition, 
          data = experiment_rounds,
          function(x) round(c(mean = mean(x), sd = sd(x)), 2)
)
#   Condition   Q2.2_1.mean Q2.2_1.sd
# 1 Condition A        8.11      1.10
# 2 Condition B        7.59      1.53
# 3 Condition C        7.55      1.39 

ggqqplot(willingness_level, "Q2.2_1", facet.by = "Condition")

########################################################################
# H3 Confidence in donation

#Q4.3_1 How confident did you feel about donating data for version 1? - Version 1
#Q4.4_1 How confident did you feel about donating data for version 1? - Version 2
#Q4.5_1 How confident did you feel about donating data for version 1? - Version 3

#Q4.6 Which version made you feel the most confident about giving consent to donate data to the research study?
########################################################################

conf_conditionA <- post_survey %>%
  dplyr::select(1,11) %>%
  rename("Confidence_level" = Q4.3_1) %>%
  mutate(Condition = "Condition A")

conf_conditionB <- post_survey %>%
  dplyr::select(2,11) %>%
  rename("Confidence_level" = Q4.4_1) %>%
  mutate(Condition = "Condition B")

conf_conditionC <- post_survey %>%
  dplyr::select(3,11) %>%
  rename("Confidence_level" = Q4.5_1) %>%
  mutate(Condition = "Condition C")

conf_condition <- bind_rows(conf_conditionA, conf_conditionB, conf_conditionC)

#ANOVA one-way
h3_res_aov <- aov(Confidence_level ~ Condition, data = conf_condition)

summary(h3_res_aov)
#             Df Sum Sq Mean Sq F value Pr(>F)
# Condition    2    9.4   4.702   1.241  0.297
# Residuals   54  204.5   3.788   

# Not statistically significant, cannot reject the null hypothesis

par(mfrow = c(1, 2)) # combine plots

# histogram
hist(h3_res_aov$residuals)

# QQ-plot
qqPlot(h3_res_aov$residuals, 
       id = FALSE # id = FALSE to remove point identification
)

#Normality
shapiro.test(conf_condition$Confidence_level)
#         Shapiro-Wilk normality test
# 
# data:  conf_condition$Confidence_level
# W = 0.91859, p-value = 0.000941

#another normality plot
plot(h3_res_aov, which = 2)

#not assume normality
conf_condition %>% friedman_test(Confidence_level ~ Condition |ParticipantID)
# # A tibble: 1 x 6
# .y.                     n   statistic   df      p method       
#   1 Confidence_level    20      5.67     2 0.0586 Friedman test

#Confidence level per condition
ggplot(conf_condition, aes(x = Condition, y = Confidence_level, fill = Condition)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, 10) +
  labs(title = "Confidence donation per condition", x = "", y = "Scale") +
  theme_minimal() +
  theme(legend.position = "none")

# descriptive statistics 
aggregate(Confidence_level ~ Condition,
          data = conf_condition,
          function(x) round(c(mean = mean(x), sd = sd(x)), 2)
)
#   Condition   Confidence_level.mean Confidence_level.sd
# 1 Condition A                  6.74                2.18
# 2 Condition B                  7.47                2.12
# 3 Condition C                  7.68                1.45  

ggqqplot(conf_condition, "Confidence_level", facet.by = "Condition")
