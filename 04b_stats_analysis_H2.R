####### Analysis of experiment data for H2 ------------

## Import libraries
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(ARTool)

## Load data
pre_survey <- read_csv("data/Clean_data/pre_survey.csv")
experiment_rounds <- read_csv("data/Clean_data/binded_conditions.csv")
post_survey <- read_csv("data/Clean_data/post_survey.csv")
complete_data <- read_csv("data/Clean_data/complete_data.csv")

##Only interested in the quantitative data
quantitative_data <- complete_data %>%
  select(-Q2.3_condA, -Q2.3_condB, -Q2.3_condC, -Q4.7_post, -Q4.8_post, -Q4.9_post, -Q5.1_post, -Q5.2_post, -Q5.3_post)

####### Trust calculations ------------------------------------------------------------
# Propensity to Trust # Faith in general technology # Trusting stance
###Pre-survey and complete data set

#Propensity to trust (PTT) - FiGT + TS
#Faith in general technology Q4.2 - Q4.5
#Trusting stance Q5.2 - Q5.4

## Pre-survey
pre_survey <- pre_survey %>%
  mutate(sumPTT = rowSums(select(., 5:11)), meanPTT = rowMeans(select(., 5:11))) %>% #, sdFiGT = rowMads(select(5:8)))
  mutate(meanFiGT = rowMeans(select(., 5:8))) %>%   #sumFiGT = rowSums(select(., 5:8)), sdFiGT = rowMads(select(5:8)))
  mutate(meanTS = rowMeans(select(., 9:11))) %>%    #sumTS = rowSums(select(., 9:11)), sdFiGT = rowMads(select(9:11)))
  relocate(sumPTT, .before = Group) %>%
  relocate(meanPTT, .before = Group) %>% #relocate(sumFiGT, .after = ) %>%
  relocate(meanFiGT, .after = Q4.5_pre) %>% 
  relocate(meanTS, .after = Q5.4_pre) #relocate(sumTS, .after = meanTS) %>% # %>% select(-sumTS, -sumFiGT, -sumPTT)
  
## Complete dataset
quantitative_data <- quantitative_data %>%
  mutate(sumPTT = rowSums(select(., 6:12)), meanPTT = rowMeans(select(., 6:12))) %>%  #, sdFiGT = rowMads(select(5:8)))
  mutate(meanFiGT = rowMeans(select(., 6:9))) %>%  #sumFiGT = rowSums(select(., 6:9)), sdFiGT = rowMads(select(5:8)))
  mutate(meanTS = rowMeans(select(., 10:12))) %>% # sumTS = rowSums(select(., 10:12)), sdFiGT = rowMads(select(10:12)))
  relocate(sumPTT, .after = Q5.4_pre) %>%
  relocate(meanPTT, .after = sumPTT) %>% #relocate(sumFiGT, .after = Q4.5_pre) %>%
  relocate(meanFiGT, .after = Q4.5_pre) %>% #relocate(sumTS, .after = Q5.4_pre) %>%
  relocate(meanTS, .after = Q5.4_pre) # %>% select(-sumFiGT, -sumTS, -sumPTT)

####### Hypothesis 2 ---------------------------------------------------
### H2 Willingness (maximize consent), controlling for PTT ------
# Q2.2_1 in experiment rounds

h2_df <- experiment_rounds %>%
  select(ParticipantID, Q2.2_1, Condition)

h2_df <- ptt_df %>% full_join(h2_df)

h2_df <- drop_na(h2_df)

#### H2 plot ----
#scale is 1-9
ggplot(h2_df, aes(x = Condition, y = Q2.2_1, fill = Condition)) +
  stat_boxplot(geom ='errorbar', width = 0.2) +
  geom_boxplot(width = 0.5) +
  scale_x_discrete(labels = c("BASELINE", "ABSTRACT", "CONCRETE"), drop = FALSE) +
  ylim(0, 9) +
  labs(title = "", x = "", y = "Willingness to donate") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(text=element_text(size = 20), axis.text=element_text(size = 20))

# saved 15.11.21
# ggsave("figures/statplots/h2_plot.png", width = 9, height = 9, bg = "white")

#### H2 descriptive statistics ----
aggregate(Q2.2_1 ~ Condition, 
          data = h2_df,
          function(x) round(c(mean = mean(x), sd = sd(x)), 2)
)
#   Condition   Q2.2_1.mean Q2.2_1.sd
# 1 Condition A        8.05      1.10
# 2 Condition B        7.59      1.53
# 3 Condition C        7.62      1.36

#Test assumptions

#Assumptions of homogeneity of variance
levene_test(Q2.2_1 ~ Condition, data = h2_df)
#     df1   df2 statistic     p
#     <int> <int>     <dbl> <dbl>
# 1     2    60    0.0648 0.937
# variance is equal among groups

#### H2 ART analysis -----
h2_df.art = art(Q2.2_1 ~ factor(Condition) + (1|ParticipantID), data = h2_df)
summary(h2_df.art)
anova(h2_df.art)

# Analysis of Variance of Aligned Rank Transformed Data
# 
# Table Type: Analysis of Deviance Table (Type III Wald F tests with Kenward-Roger df) 
# Model: Mixed Effects (lmer)
# Response: art(Q2.2_1)
# 
#                           F Df Df.res  Pr(>F)  
# 1 factor(Condition) 1.4968  2 38.773 0.23649  
# ---
#   Signif. codes:   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

#### H2 Normality -----
shapiro.test(residuals(h2_df.art))
# Shapiro-Wilk normality test
# 
# data:  residuals(h2_df.art)
# W = 0.89665, p-value = 6.801e-05
qqnorm(residuals(h2_df.art)); qqline(residuals(h2_df.art))


