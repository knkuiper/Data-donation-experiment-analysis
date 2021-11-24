####### Analysis of experiment data for H1 ------------

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

####### Hypothesis 1 ---------------------------------------------------
### H1 Trust in system, controlling for PTT ------

# Reliability and functionality #Trust in specific technology Q3.2 - Q3.7
experiment_rounds <- experiment_rounds %>%
  mutate(sumTiST = rowSums(select(., 6:11)), meanTiST = rowMeans(select(., 6:11))) %>%
  relocate(sumTiST, .after = Q3.7) %>%
  relocate(meanTiST, .after = sumTiST) # %>% select(-sumTiST)

ptt_df <- pre_survey %>%
  dplyr::select(ParticipantID, sumPTT, meanPTT)

h1_df <- experiment_rounds %>%
  dplyr::select(ParticipantID, meanTiST, sumTiST, Condition)

h1_df <- ptt_df %>% full_join(h1_df)

h1_df <- drop_na(h1_df)

#### H1 Plot ----
ggplot(h1_df, aes(x = Condition, y = meanTiST, fill = Condition)) +
  stat_boxplot(geom ='errorbar', width = 0.2) +
  geom_boxplot(width = 0.5) +
  scale_x_discrete(labels = c("BASELINE", "ABSTRACT", "CONCRETE"), drop = FALSE) +
  ylim(0, 7) +
  labs(title = "", x = "", y = "Trust in specific technology") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 20), axis.text = element_text(size = 20))

# saved 15.11.21
ggsave("figures/statplots/h1_plot.png", width = 9, height = 9, bg = "white")

#### H1 Descriptive statistics  ----
aggregate(meanTiST ~ Condition, 
          data = h1_df,
          function(x) round(c(mean = mean(x), sd = sd(x)), 2)
)
#   Condition   meanTiST.mean meanTiST.sd
# 1 Condition A          5.64        0.94
# 2 Condition B          5.38        0.83
# 3 Condition C          5.32        0.77

#### Test assumptions ----

#Assumptions of homogeneity of variance
levene_test(meanTiST ~ Condition, data = h1_df)
#     df1   df2 statistic     p
#     <int> <int>     <dbl> <dbl>
# 1     2    59     0.189 0.829
# variance is equal among groups



#### H1 analysis ----

#one way anova
h1_df.aov <- aov(meanTiST ~ factor(meanPTT)+factor(Condition) + (1|ParticipantID), data = h1_df)
summary(h1_df.aov)
#                   Df Sum Sq Mean Sq F value   Pr(>F)    
# factor(meanPTT)   12 23.819  1.9849   4.839 4.08e-05 ***
# factor(Condition)  2  0.567  0.2834   0.691    0.506    
# Residuals         47 19.277  0.4102                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# h1_df.art = art(meanTiST ~ factor(meanPTT)*factor(Condition) + (1|ParticipantID), data = h1_df)
# summary(h1_df.art)
# anova(h1_df.art)

# Analysis of Variance of Aligned Rank Transformed Data
# 
# Table Type: Analysis of Deviance Table (Type III Wald F tests with Kenward-Roger df) 
# Model: Mixed Effects (lmer)
# Response: art(meanTiST)
# 
# F Df  Df.res    Pr(>F)   
# 1 factor(meanPTT)                   0.9364 12  8.9957 0.5529605   
# 2 factor(Condition)                 2.2989  2 16.0186 0.1325150   
# 3 factor(meanPTT):factor(Condition) 3.3094 22 16.0630 0.0085795 **
#   ---
#   Signif. codes:   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

# no main effect
# interaction effect meanPTT:Condition


#### H1 Normality ----
shapiro.test(residuals(h1_df.aov))
# Shapiro-Wilk normality test
# 
# data:  residuals(h1_df.aov)
# W = 0.98204, p-value = 0.498

#normal distribution

qqnorm(residuals(h1_df.aov)); qqline(residuals(h1_df.aov))
#not normal