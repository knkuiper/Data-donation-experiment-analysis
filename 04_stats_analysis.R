####### Analysis of experiment data ------------

## Import libraries
library(readxl)
library(dplyr)
library(tidyverse)
#library(MASS)
library(ggplot2)
library(ggpubr)
#library(viridis)
library(rstatix)
library(ARTool)

## Load data
pre_survey <- read_csv("data/Clean_data/pre_survey.csv")
experiment_rounds <- read_csv("data/Clean_data/binded_conditions.csv")
post_survey <- read_csv("data/Clean_data/post_survey.csv")
complete_data <- read_csv("data/Clean_data/complete_data.csv")
#experiment_cond_A <- read_csv("data/Clean_data/ConditionA.csv")
#experiment_cond_B <- read_csv("data/Clean_data/ConditionB.csv")
#experiment_cond_C <- read_csv("data/Clean_data/ConditionC.csv")

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
  geom_boxplot() +
  ylim(0, 7) +
  theme_minimal()

#ggsave("figures/statplots/h1_plot.png")

#### H1 Descriptive statistics  ----
aggregate(meanTiST ~ Condition, 
          data = h1_df,
          function(x) round(c(mean = mean(x), sd = sd(x)), 2)
)
#   Condition   meanTiST.mean meanTiST.sd
# 1 Condition A          5.64        0.94
# 2 Condition B          5.38        0.83
# 3 Condition C          5.32        0.77

#### H1 ART analysis ----
h1_df.art = art(meanTiST ~ factor(meanPTT)*factor(Condition) + (1|ParticipantID), data = h1_df)
anova(h1_df.art)

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

#### H1 Normality ----
shapiro.test(residuals(h1_df.art))
# Shapiro-Wilk normality test
# 
# data:  residuals(h1_df.art)
# W = 0.92171, p-value = 0.0007254
qqnorm(residuals(h1_df.art)); qqline(residuals(h1_df.art))


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
  geom_boxplot() +
  ylim(0, 9.5) +
  theme_minimal()

#ggsave("figures/statplots/h2_plot.png")

#### H2 descriptive statistics ----
aggregate(Q2.2_1 ~ Condition, 
          data = h2_df,
          function(x) round(c(mean = mean(x), sd = sd(x)), 2)
)
#   Condition   Q2.2_1.mean Q2.2_1.sd
# 1 Condition A        8.05      1.10
# 2 Condition B        7.59      1.53
# 3 Condition C        7.62      1.36

#### H2 ART analysis -----
h2_df.art = art(Q2.2_1 ~ factor(meanPTT)*factor(Condition) + (1|ParticipantID), data = h2_df)
anova(h2_df.art)

# Analysis of Variance of Aligned Rank Transformed Data
# 
# Table Type: Analysis of Deviance Table (Type III Wald F tests with Kenward-Roger df) 
# Model: Mixed Effects (lmer)
# Response: art(Q2.2_1)
# 
# F Df  Df.res    Pr(>F)   
# 1 factor(meanPTT)                   1.1107 13  8.9826 0.4487329   
# 2 factor(Condition)                 1.0151  2 16.0241 0.3845238   
# 3 factor(meanPTT):factor(Condition) 4.6397 22 16.0725 0.0013678 **
#   ---
#   Signif. codes:   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1  

#### H2 Normality -----
shapiro.test(residuals(h2_df.art))
# Shapiro-Wilk normality test
# 
# data:  residuals(h2_df.art)
# W = 0.89665, p-value = 6.801e-05
qqnorm(residuals(h2_df.art)); qqline(residuals(h2_df.art))


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
  geom_boxplot() +
  ylim(0, 10) +
  theme_minimal()
#remove outliers?

#ggsave("figures/statplots/h3_plot.png")

#### H3 descriptive statistics -----
aggregate(Confidence_level ~ Condition,
          data = h3_df,
          function(x) round(c(mean = mean(x), sd = sd(x)), 2)
)
#   Condition   Confidence_level.mean Confidence_level.sd
# 1 Condition A                  6.75                2.12
# 2 Condition B                  7.45                2.06
# 3 Condition C                  7.65                1.42

#### H3 ART analysis  -----
h3_df.art = art(Confidence_level ~ factor(meanPTT)*factor(Condition) + (1|ParticipantID), data = h3_df)
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
