####### Analysis of experiment data for H3 ------------

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

####### Hypothesis 3 ----------------------------------------------------
### H3 Confidence in donation, controlling for PTT ----

# Q4.3_1 How confident did you feel about donating data for version 1? - Version 1
# Q4.4_1 How confident did you feel about donating data for version 1? - Version 2
# Q4.5_1 How confident did you feel about donating data for version 1? - Version 3
# Q4.6 Which version made you feel the most confident about giving consent to donate data to the research study?

conf_conditionA <- post_survey %>%
  select(1,2) %>%
  dplyr::rename("Confidence_level" = Q4.3_1_post) %>%
  dplyr::mutate(Condition = "Condition A")

conf_conditionB <- post_survey %>%
  select(1,3) %>%
  rename("Confidence_level" = Q4.4_1_post) %>%
  mutate(Condition = "Condition B")

conf_conditionC <- post_survey %>%
  select(1,4) %>%
  rename("Confidence_level" = Q4.5_1_post) %>%
  mutate(Condition = "Condition C")

h3_df <- bind_rows(conf_conditionA, conf_conditionB, conf_conditionC)

h3_df <- ptt_df %>% full_join(h3_df)

h3_df <- drop_na(h3_df)

#### H3 plot -----
# scale 1-10
ggplot(h3_df, aes(x = Condition, y = Confidence_level, fill = Condition)) + 
  stat_boxplot(geom ='errorbar', width = 0.2) +
  geom_boxplot(width = 0.5) +
  scale_x_discrete(labels = c("BASELINE", "ABSTRACT", "CONCRETE"), drop = FALSE) +
  ylim(0, 10) +
  labs(title = "", x = "", y = "Confidence in donation") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(text=element_text(size = 20), axis.text=element_text(size = 20))
#remove outliers

# saved 15.11.21
ggsave("figures/statplots/h3_plot_outliers.png", width = 9, height = 9, bg = "white")

#### H3 descriptive statistics -----
aggregate(Confidence_level ~ Condition,
          data = h3_df,
          function(x) round(c(mean = mean(x), sd = sd(x)), 2)
)
#   Condition   Confidence_level.mean Confidence_level.sd
# 1 Condition A                  6.75                2.12
# 2 Condition B                  7.45                2.06
# 3 Condition C                  7.65                1.42

#Test assumptions

#Assumptions of homogeneity of variance
levene_test(Confidence_level ~ Condition, data = h3_df)
#   df1   df2 statistic     p
#   <int> <int>     <dbl> <dbl>
# 1     2    57     0.950 0.393
# variance is equal among groups

#### H3 ART analysis  -----
h3_df.art = art(Confidence_level ~ factor(Condition) + (1|ParticipantID), data = h3_df)
summary(h3_df.art)
anova(h3_df.art)

# Analysis of Variance of Aligned Rank Transformed Data
# 
# Table Type: Analysis of Deviance Table (Type III Wald F tests with Kenward-Roger df) 
# Model: Mixed Effects (lmer)
# Response: art(Confidence_level)
# 
# F Df Df.res   Pr(>F)  
# 1 factor(meanPTT)                   2.2517 11      8 0.129451  
# 2 factor(Condition)                 3.4589  2     16 0.056439 .
# 3 factor(meanPTT):factor(Condition) 1.1648 22     16 0.382859  
# ---
#   Signif. codes:   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

#### H3 Normality -----
shapiro.test(residuals(h3_df.art))
# Shapiro-Wilk normality test
# 
# data:  residuals(h3_df.art)
# W = 0.92586, p-value = 0.001331

qqnorm(residuals(h3_df.art)); qqline(residuals(h3_df.art))



########
###### H3 without outliers
### Removing outliers for hypothesis testing

h3_df_no_outliers <- h3_df %>%
  dplyr::filter(!(ParticipantID == 30524 & Condition == "Condition B")) %>%
  dplyr::filter(!(ParticipantID == 96216 & Condition == "Condition C")) %>%
  dplyr::filter(!(ParticipantID == 11942 & Condition == "Condition C")) %>%
  dplyr::filter(!(ParticipantID == 14609 & Condition == "Condition C")) %>%
  dplyr::filter(!(ParticipantID == 30524 & Condition == "Condition C")) %>%
  dplyr::filter(!(ParticipantID == 90593 & Condition == "Condition C"))

# scale 1-10
ggplot(h3_df_no_outliers, aes(x = Condition, y = Confidence_level, fill = Condition)) +
  stat_boxplot(geom ='errorbar', width = 0.2) +
  geom_boxplot(width = 0.5) +
  scale_x_discrete(labels = c("BASELINE", "ABSTRACT", "CONCRETE"), drop = FALSE) +
  ylim(0, 10) +
  labs(title = "", x = "", y = "Confidence in donation") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(text=element_text(size = 20), axis.text=element_text(size = 20))

# saved 15.11.21
# ggsave("figures/statplots/h3_plot_nooutliers.png", width = 9, height = 9, bg = "white")


#### H3 ART analysis  -----
h3_df_no_outliers.art = art(Confidence_level ~ factor(Condition) + (1|ParticipantID), data = h3_df_no_outliers)
summary(h3_df_no_outliers.art)
anova(h3_df_no_outliers.art)

# Analysis of Variance of Aligned Rank Transformed Data
# 
# Table Type: Analysis of Deviance Table (Type III Wald F tests with Kenward-Roger df) 
# Model: Mixed Effects (lmer)
# Response: art(Confidence_level)
# 
#                           F Df Df.res   Pr(>F)  
# 1 factor(Condition) 3.2001  2 33.243 0.053575 .
# ---
#   Signif. codes:   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

#### H3 descriptive statistics -----
aggregate(Confidence_level ~ Condition,
          data = h3_df_no_outliers,
          function(x) round(c(mean = mean(x), sd = sd(x)), 2)
)
#     Condition Confidence_level.mean Confidence_level.sd
# 1 Condition A                  6.75                2.12
# 2 Condition B                  7.79                1.44
# 3 Condition C                  7.20                0.68

#Test assumptions

#Assumptions of homogeneity of variance
levene_test(Confidence_level ~ Condition, data = h3_df_no_outliers)
  #     df1   df2 statistic       p
  #   <int> <int>     <dbl>   <dbl>
  # 1     2    51      5.80 0.00539
# variance is not equal among groups

#### H3 Normality -----
shapiro.test(residuals(h3_df_no_outliers.aov))
# Shapiro-Wilk normality test
# 
# data:  residuals(h3_df_no_outliers.aov)
# W = 0.96609, p-value = 0.1292

qqnorm(residuals(h3_df_no_outliers.aov)); qqline(residuals(h3_df_no_outliers.aov))

