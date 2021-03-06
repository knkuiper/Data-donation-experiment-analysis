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

partial_na <- list(12622, 67484)

pre_survey <- pre_survey %>% 
  slice(-1) %>%
  dplyr::select(19, 21:29, 31, 33) %>%
  dplyr::filter(!ParticipantID %in% c(excluded), !ParticipantID %in% c(nonresponse)) %>%
  dplyr::mutate(Q4.2 = dplyr::recode(Q4.2, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q4.3 = dplyr::recode(Q4.3, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q4.4 = dplyr::recode(Q4.4, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q4.5 = dplyr::recode(Q4.5, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q5.2 = dplyr::recode(Q5.2, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q5.3 = dplyr::recode(Q5.3, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q5.4 = dplyr::recode(Q5.4, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7)) %>%
  dplyr::relocate(ParticipantID)

pre_survey <- pre_survey %>%
  setNames(paste0(names(.), "_pre")) %>%
  rename(ParticipantID = ParticipantID_pre, Group = Group_pre)

# write out new datafile
# saved 02.11.21
## write_csv(pre_survey, file="data/Clean_data/pre_survey.csv")

## Experiment rounds data -----------------------------------------------

participants_groups <- pre_survey %>%
  dplyr::select(1, 12)

### Binding dataframes
### Condition A, B, C
### Rounds 1, 2, 3
## Groups?

### ALFA -----------------------------------------------------------------

alfa_r1 <- read_excel("data/Data donation experiment round 1 Alfa.xlsx")

alfa_r1_exp <- alfa_r1 %>% 
  slice(-1) %>%
  dplyr::select(19:29) %>%
  mutate(Round = "Round1",
         Q3.2 = dplyr::recode(Q3.2, 
                     "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                     "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.3 = dplyr::recode(Q3.3, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.4 = dplyr::recode(Q3.4, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.5 = dplyr::recode(Q3.5, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.6 = dplyr::recode(Q3.6, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.7 = dplyr::recode(Q3.7, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7))

alfa_r2 <- read_excel("data/Data donation experiment round 2 Alfa.xlsx")

alfa_r2_exp <- alfa_r2 %>% 
  slice(-1) %>%
  dplyr::select(19:29) %>%
  mutate(Round = "Round2",
         Q3.2 = dplyr::recode(Q3.2, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.3 = dplyr::recode(Q3.3, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.4 = dplyr::recode(Q3.4, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.5 = dplyr::recode(Q3.5, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.6 = dplyr::recode(Q3.6, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.7 = dplyr::recode(Q3.7, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7))

alfa_r3 <- read_excel("data/Data donation experiment round 3 Alfa + post survey.xlsx")

alfa_r3_exp <- alfa_r3 %>% 
  slice(-1) %>%
  dplyr::select(19:28, 39) %>%
  mutate(Round = "Round3",
         Q3.2 = dplyr::recode(Q3.2, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.3 = dplyr::recode(Q3.3, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.4 = dplyr::recode(Q3.4, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.5 = dplyr::recode(Q3.5, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.6 = dplyr::recode(Q3.6, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.7 = dplyr::recode(Q3.7, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7))

alfa_compl <- bind_rows(alfa_r1_exp, alfa_r2_exp, alfa_r3_exp)

alfa_compl <- alfa_compl %>%
  mutate(Condition = "Condition A") %>%
  relocate(ParticipantID)

alfa_compl <- alfa_compl %>%
  inner_join(participants_groups)

alfa_cond <- alfa_compl

alfa_compl <- alfa_compl %>%
  dplyr::select(-Condition) %>%
  setNames(paste0(names(.), "_condA")) %>%
  dplyr::rename(ParticipantID = ParticipantID_condA, Group = Group_condA)

# write out new datafile
# saved 02.11.21
# write_csv(alfa_compl, file="data/Clean_data/conditionA.csv")

### BETA -------------------------------------------------------------------

beta_r1 <- read_excel("data/Data donation experiment round 1 Beta.xlsx")

beta_r1_exp <- beta_r1 %>% 
  slice(-1) %>%
  dplyr::select(19:29) %>%
  mutate(Round = "Round1",
         Q3.2 = dplyr::recode(Q3.2, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.3 = dplyr::recode(Q3.3, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.4 = dplyr::recode(Q3.4, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.5 = dplyr::recode(Q3.5, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.6 = dplyr::recode(Q3.6, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.7 = dplyr::recode(Q3.7, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7))

beta_r2 <- read_excel("data/Data donation experiment round 2 Beta.xlsx")

beta_r2_exp <- beta_r2 %>% 
  slice(-1) %>%
  dplyr::select(19:29) %>%
  mutate(Round = "Round2",
         Q3.2 = dplyr::recode(Q3.2, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.3 = dplyr::recode(Q3.3, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.4 = dplyr::recode(Q3.4, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.5 = dplyr::recode(Q3.5, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.6 = dplyr::recode(Q3.6, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.7 = dplyr::recode(Q3.7, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7))

beta_r3 <- read_excel("data/Data donation experiment round 3 Beta + post survey.xlsx")

beta_r3_exp <- beta_r3 %>% 
  slice(-1) %>%
  dplyr::select(19:28, 39) %>%
  mutate(Round = "Round3",
         Q3.2 = dplyr::recode(Q3.2, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.3 = dplyr::recode(Q3.3, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.4 = dplyr::recode(Q3.4, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.5 = dplyr::recode(Q3.5, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.6 = dplyr::recode(Q3.6, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.7 = dplyr::recode(Q3.7, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7))

beta_compl <- bind_rows(beta_r1_exp, beta_r2_exp, beta_r3_exp)

beta_compl <- beta_compl %>%
  mutate(Condition = "Condition B") %>%
  relocate(ParticipantID) 

beta_compl <- beta_compl %>%
  inner_join(participants_groups)

beta_cond <- beta_compl

beta_compl <- beta_compl %>%
  dplyr::select(-Condition) %>%
  setNames(paste0(names(.), "_condB")) %>%
  rename(ParticipantID = ParticipantID_condB, Group = Group_condB)

# write out new datafile
# saved 02.11.21
# write_csv(beta_compl, file="data/Clean_data/conditionB.csv")

### GAMMA -------------------------------------------------------------------

gamma_r1 <- read_excel("data/Data donation experiment round 1 Gamma.xlsx")

gamma_r1_exp <- gamma_r1 %>% 
  slice(-1) %>%
  dplyr::select(19:29) %>%
  mutate(Round = "Round1",
         Q3.2 = dplyr::recode(Q3.2, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.3 = dplyr::recode(Q3.3, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.4 = dplyr::recode(Q3.4, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.5 = dplyr::recode(Q3.5, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.6 = dplyr::recode(Q3.6, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.7 = dplyr::recode(Q3.7, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7))

gamma_r2 <- read_excel("data/Data donation experiment round 2 Gamma.xlsx")

gamma_r2_exp <- gamma_r2 %>% 
  slice(-1) %>%
  dplyr::select(19:29) %>%
  mutate(Round = "Round2",
         Q3.2 = dplyr::recode(Q3.2, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.3 = dplyr::recode(Q3.3, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.4 = dplyr::recode(Q3.4, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.5 = dplyr::recode(Q3.5, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.6 = dplyr::recode(Q3.6, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.7 = dplyr::recode(Q3.7, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7))

gamma_r3 <- read_excel("data/Data donation experiment round 3 Gamma + post survey.xlsx")

gamma_r3_exp <- gamma_r3 %>% 
  slice(-1) %>%
  dplyr::select(19:28, 39) %>%
  mutate(Round = "Round3",
         Q3.2 = dplyr::recode(Q3.2, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.3 = dplyr::recode(Q3.3, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.4 = dplyr::recode(Q3.4, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.5 = dplyr::recode(Q3.5, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.6 = dplyr::recode(Q3.6, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7),
         Q3.7 = dplyr::recode(Q3.7, 
                       "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7))

gamma_compl <- bind_rows(gamma_r1_exp, gamma_r2_exp, gamma_r3_exp)

gamma_compl <- gamma_compl %>%
  mutate(Condition = "Condition C") %>%
  relocate(ParticipantID)

gamma_compl <- gamma_compl %>%
  inner_join(participants_groups)

gamma_cond <- gamma_compl

gamma_compl <- gamma_compl %>%
  dplyr::select(-Condition) %>%
  setNames(paste0(names(.), "_condC")) %>%
  rename(ParticipantID = ParticipantID_condC, Group = Group_condC)

# write out new datafile
# saved 02.11.21
# write_csv(gamma_compl, file="data/Clean_data/conditionC.csv")

### ALL DATA (every COND + ROUNDS) ------------------------------------------

binded_conditions <- bind_rows(alfa_cond, beta_cond, gamma_cond)

# write out new datafile for all conditions
# saved 02.11.21
# write_csv(binded_conditions, file="data/Clean_data/binded_conditions.csv")

joined_conditions <- beta_compl %>%
  full_join(alfa_compl) %>%
  full_join(gamma_compl)

# write out new datafile for all conditions
# saved 02.11.21
# write_csv(joined_conditions, file="data/Clean_data/joined_conditions.csv")

## POST Survey data ---------------------------------------------------------
# filter and clean only post data

alfa_post <- alfa_r3 %>%
  slice(-1) %>%
  dplyr::select(29:39) #%>%
  #mutate(FinalCondition = "Condition A")

beta_post <- beta_r3 %>%
  slice(-1) %>%
  dplyr::select(29:39) #%>%
  #mutate(FinalCondition = "Condition B")

gamma_post <- gamma_r3 %>%
  slice(-1) %>%
  dplyr::select(29:39) #%>%
  #mutate(FinalCondition = "Condition C")

post_survey <- bind_rows(alfa_post, beta_post, gamma_post)

post_survey <- post_survey %>%
  relocate(ParticipantID)

post_survey <- post_survey %>%
  inner_join(participants_groups)

post_survey <- post_survey %>%
  setNames(paste0(names(.), "_post")) %>%
  rename(ParticipantID = ParticipantID_post, Group = Group_post)

# write out new datafile for post survey
# saved 02.11.21
# write_csv(post_survey, file="data/Clean_data/post_survey.csv")

## Complete experiment data -------------------------------------------------

complete_data <- pre_survey %>%
  full_join(joined_conditions) %>%
  full_join(post_survey)

complete_data <- complete_data %>%
  relocate(Group, .after = ParticipantID) %>%
  relocate(Q2.1_condA, Q2.2_1_condA, Q2.3_condA, Q2.4_condA, Q3.2_condA, Q3.3_condA, Q3.4_condA, Q3.5_condA, Q3.6_condA, Q3.7_condA, Round_condA, .before = Q2.1_condB)

# write out new datafile for all conditions
# saved 02.11.21
# write_csv(complete_data, file="data/Clean_data/complete_data.csv")

# Functions -----------------------------------------------------------------

#write as function?

# filter_survey <- function(newdata, data) {
#   newdata <- 
#     data %>%
#       slice(-1) %>%
#         dplyr::select(18:29)
# }
# 
# filter_survey(alfa_r1, alfa_r1)
# 
# alfa_r1 <- alfa_r1 %>% 
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