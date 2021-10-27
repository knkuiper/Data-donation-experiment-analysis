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
library(rstatix)
library(ARTool)

## Load data
pre_survey <- read_csv("data/Clean_data/pre_survey.csv")
experiment_rounds <- read_csv("data/Clean_data/all_conditions.csv")
experiment_cond_A <- read_csv("data/Clean_data/ConditionA.csv")
experiment_cond_B <- read_csv("data/Clean_data/ConditionB.csv")
experiment_cond_C <- read_csv("data/Clean_data/ConditionC.csv")
post_survey <- read_csv("data/Clean_data/post_survey.csv")
complete_data <- read_csv("data/Clean_data/complete_data.csv")



########################################################################
# H1 Trust
########################################################################


###Pre-survey
#Propensity to trust (PTT) - FiGT + TS
pre_survey <- pre_survey %>%
  mutate(sumPTT = rowSums(select(., 5:11)), meanPTT = rowMeans(select(., 5:11))) %>%  #, sdFiGT = rowMads(select(5:8)))
  relocate(sumPTT, .after = Q5.4_pre) %>%
  relocate(meanPTT, .after = sumPTT) %>%
  select(-sumPTT)

#Faith in general technology Q4.2 - Q4.5
pre_survey <- pre_survey %>%
  mutate(sumFiGT = rowSums(select(., 5:8)), meanFiGT = rowMeans(select(., 5:8))) %>%  #, sdFiGT = rowMads(select(5:8)))
  relocate(sumFiGT, .after = Q4.5_pre) %>%
  relocate(meanFiGT, .after = sumFiGT) %>%
  select(-sumFiGT)

#Trusting stance Q5.2 - Q5.4
pre_survey <- pre_survey %>%
  mutate(sumTS = rowSums(select(., 11:13)), meanTS = rowMeans(select(., 11:13))) %>%
  relocate(sumTS, .after = Q5.4_pre) %>%
  relocate(meanTS, .after = sumTS) %>%
  select(-sumTS)



###Experiment rounds


#Reliability and functionality
#Trust in specific technology Q3.2 - Q3.7
experiment_rounds <- experiment_rounds %>%
  mutate(sumTiST = rowSums(select(., 6:11)), meanTiST = rowMeans(select(., 6:11))) %>%
  relocate(sumTiST, .after = Q3.7) %>%
  relocate(meanTiST, .after = sumTiST) %>%
  select(-sumTiST)






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

ggsave("figures/post_survey_edaplots/willingness_donate_again.png")


#ANOVA one-way
h2_res_aov <- aov(Q2.2_1 ~ Condition, data = willingness_level)

summary(h2_res_aov)
#             Df Sum Sq Mean Sq F value Pr(>F)
# Condition    2   2.72   1.358   0.746  0.479
# Residuals   60 109.22   1.820               
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
#experiment_rounds %>% friedman_test(Q2.2_1 ~ Condition |ParticipantID)
# does not work because "not an unreplicated complete block design"

# descriptive statistics 
aggregate(Q2.2_1 ~ Condition, 
          data = experiment_rounds,
          function(x) round(c(mean = mean(x), sd = sd(x)), 2)
)
#   Condition Q2.2_1.mean Q2.2_1.sd
# 1 Condition A        8.05      1.10
# 2 Condition B        7.59      1.53
# 3 Condition C        7.62      1.36

ggqqplot(willingness_level, "Q2.2_1", facet.by = "Condition", title = "H2 Willingness")

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
# Condition    2   8.93   4.467    1.24  0.297
# Residuals   57 205.25   3.601       

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

ggqqplot(conf_condition, "Confidence_level", facet.by = "Condition", title = "H3 Confidence in donation")
