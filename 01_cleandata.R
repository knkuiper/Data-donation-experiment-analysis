####### Import and clean up data sets -------------------------------

## Import libraries
library(readxl)
library(dplyr)
library(tidyverse)

## Pre survey data ---------------------------------------------------

pre_survey <- read_excel("data/Data donation pre experiment.xlsx")

# Filter out excluded participants
# Filter out nonresponse participants
# Select useful columns

excluded <- list(91093, 81153, 30358, 54779, 37020, 95171, 10493, 63781, 24724, 
                 28163, 67340, 95639, 20643, 53509, 67691, 64339, 99237, 97463)

nonresponse <- list(81921, 46588, 72432, 93615, 98612, 82350, 59668, 63680, 35211, 88229, 
                    94747, 48150, 85706, 53568, 15561, 98399, 25718, 32307, 51944, 33926)

partial_na <- list(12622)

pre_survey_filtered <- pre_survey %>% 
  slice(-1) %>%
  dplyr::select(19, 21:29, 31) %>%
  dplyr::filter(!ParticipantID %in% c(excluded), !ParticipantID %in% c(nonresponse)) %>%
  mutate(Q4.2 = recode(Q4.2, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q4.3 = recode(Q4.3, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q4.4 = recode(Q4.4, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q4.5 = recode(Q4.5, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q5.2 = recode(Q5.2, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q5.3 = recode(Q5.3, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q5.4 = recode(Q5.4, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         )

# write out new datafile
write_csv(pre_survey_filtered, file="data/Clean_data/pre_survey")

## Experiment rounds data -----------------------------------------------

### Binding dataframes
### Condition A, B, C
### Rounds 1, 2, 3
## Groups?

### ALFA -----------------------------------------------------------------

alfa_r1 <- read_excel("data/Data donation experiment round 1 Alfa.xlsx")

alfa_r1_filtered <- alfa_r1 %>% 
  slice(-1) %>%
  dplyr::select(19:29) %>%
  mutate(Round = "Round1",
         Q3.2 = recode(Q3.2, 
                     "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                     "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.3 = recode(Q3.3, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.4 = recode(Q3.4, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.5 = recode(Q3.5, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.6 = recode(Q3.6, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.7 = recode(Q3.7, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7))

alfa_r2 <- read_excel("data/Data donation experiment round 2 Alfa.xlsx")

alfa_r2_filtered <- alfa_r2 %>% 
  slice(-1) %>%
  dplyr::select(19:29) %>%
  mutate(Round = "Round2",
         Q3.2 = recode(Q3.2, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.3 = recode(Q3.3, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.4 = recode(Q3.4, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.5 = recode(Q3.5, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.6 = recode(Q3.6, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.7 = recode(Q3.7, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7))

alfa_r3 <- read_excel("data/Data donation experiment round 3 Alfa + post survey.xlsx")

alfa_r3_filtered <- alfa_r3 %>% 
  slice(-1) %>%
  dplyr::select(19:28, 39) %>%
  mutate(Round = "Round3",
         Q3.2 = recode(Q3.2, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.3 = recode(Q3.3, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.4 = recode(Q3.4, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.5 = recode(Q3.5, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.6 = recode(Q3.6, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.7 = recode(Q3.7, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7))

alfa_compl <- bind_rows(alfa_r1_filtered, alfa_r2_filtered, alfa_r3_filtered)

alfa_compl <- alfa_compl %>%
  mutate(Condition = "Condition A")

# write out new datafile
write_csv(alfa_compl, file="data/Clean_data/conditionA")

### BETA -------------------------------------------------------------------

beta_r1 <- read_excel("data/Data donation experiment round 1 Beta.xlsx")

beta_r1_filtered <- beta_r1 %>% 
  slice(-1) %>%
  dplyr::select(19:29) %>%
  mutate(Round = "Round1",
         Q3.2 = recode(Q3.2, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.3 = recode(Q3.3, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.4 = recode(Q3.4, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.5 = recode(Q3.5, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.6 = recode(Q3.6, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.7 = recode(Q3.7, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7))

beta_r2 <- read_excel("data/Data donation experiment round 2 Beta.xlsx")

beta_r2_filtered <- beta_r2 %>% 
  slice(-1) %>%
  dplyr::select(19:29) %>%
  mutate(Round = "Round2",
         Q3.2 = recode(Q3.2, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.3 = recode(Q3.3, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.4 = recode(Q3.4, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.5 = recode(Q3.5, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.6 = recode(Q3.6, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.7 = recode(Q3.7, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7))

beta_r3 <- read_excel("data/Data donation experiment round 3 Beta + post survey.xlsx")

beta_r3_filtered <- beta_r3 %>% 
  slice(-1) %>%
  dplyr::select(19:28, 39) %>%
  mutate(Round = "Round3",
         Q3.2 = recode(Q3.2, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.3 = recode(Q3.3, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.4 = recode(Q3.4, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.5 = recode(Q3.5, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.6 = recode(Q3.6, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.7 = recode(Q3.7, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7))

beta_compl <- bind_rows(beta_r1_filtered, beta_r2_filtered, beta_r3_filtered)

beta_compl <- beta_compl %>%
  mutate(Condition = "Condition B")

# write out new datafile
write_csv(beta_compl, file="data/Clean_data/conditionB")

### GAMMA -------------------------------------------------------------------

gamma_r1 <- read_excel("data/Data donation experiment round 1 Gamma.xlsx")

gamma_r1_filtered <- gamma_r1 %>% 
  slice(-1) %>%
  dplyr::select(19:29) %>%
  mutate(Round = "Round1",
         Q3.2 = recode(Q3.2, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.3 = recode(Q3.3, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.4 = recode(Q3.4, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.5 = recode(Q3.5, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.6 = recode(Q3.6, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.7 = recode(Q3.7, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7))

gamma_r2 <- read_excel("data/Data donation experiment round 2 Gamma.xlsx")

gamma_r2_filtered <- gamma_r2 %>% 
  slice(-1) %>%
  dplyr::select(19:29) %>%
  mutate(Round = "Round2",
         Q3.2 = recode(Q3.2, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.3 = recode(Q3.3, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.4 = recode(Q3.4, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.5 = recode(Q3.5, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.6 = recode(Q3.6, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.7 = recode(Q3.7, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7))

gamma_r3 <- read_excel("data/Data donation experiment round 3 Gamma + post survey.xlsx")

gamma_r3_filtered <- gamma_r3 %>% 
  slice(-1) %>%
  dplyr::select(19:28, 39) %>%
  mutate(Round = "Round3",
         Q3.2 = recode(Q3.2, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.3 = recode(Q3.3, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.4 = recode(Q3.4, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.5 = recode(Q3.5, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.6 = recode(Q3.6, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.7 = recode(Q3.7, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7))

gamma_compl <- bind_rows(gamma_r1_filtered, gamma_r2_filtered, gamma_r3_filtered)

gamma_compl <- gamma_compl %>%
  mutate(Condition = "Condition C")

# write out new datafile
write_csv(gamma_compl, file="data/Clean_data/conditionC")

### ALL DATA (every COND + ROUNDS) ------------------------------------------

all_conditions <- bind_rows(alfa_compl, beta_compl, gamma_compl)

# write out new datafile for all conditions
write_csv(all_conditions, file="data/Clean_data/all_conditions")

## POST Survey data ---------------------------------------------------------
# filter and clean only post data

alfa_post <- alfa_r3 %>%
  slice(-1) %>%
  dplyr::select(29:39) %>%
  mutate(FinalCondition = "Condition A")

beta_post <- beta_r3 %>%
  slice(-1) %>%
  dplyr::select(29:39) %>%
  mutate(FinalCondition = "Condition B")

gamma_post <- gamma_r3 %>%
  slice(-1) %>%
  dplyr::select(29:39) %>%
  mutate(FinalCondition = "Condition C")

post_survey <- bind_rows(alfa_post, beta_post, gamma_post)

# write out new datafile for post survey
write_csv(post_survey, file="data/Clean_data/post_survey")

# Functions -----------------------------------------------------------------

#write as function?

# filter_survey <- function(newdata, data) {
#   newdata <- 
#     data %>%
#       slice(-1) %>%
#         dplyr::select(18:29)
# }
# 
# filter_survey(alfa_r1_filtered, alfa_r1)
# 
# alfa_r1_filtered <- alfa_r1 %>% 
#   slice(-1) %>%
#     dplyr::select(18:29)
# 
# beta_r1_filtered <- beta_r1 %>% 
#   slice(-1) %>%
#     dplyr::select(18:29)
# 
# gamma_r1_filtered <- gamma_r1 %>% 
#   slice(-1) %>%
#     dplyr::select(18:29)