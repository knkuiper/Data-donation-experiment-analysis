####### Import and clean up data sets -------------------------------

## Import libraries
library(readxl)
library(dplyr)
library(tidyverse)

## Pre survey data ---------------------------------------------------

pre_survey <- read_excel("data/Pre-experiment survey.xlsx")

# Filter out excluded participants
# Select useful columns

excluded <- list(91093, 81153, 30358, 54779, 37020, 95171, 10493, 63781, 24724, 
                 28163, 67340, 95639, 20643, 53509, 67691, 64339, 99237)

pre_survey_filtered <- pre_survey %>% 
  slice(-1) %>%
  dplyr::select(18:31) %>%
  filter(!ParticipantID %in% c(excluded))

# write out new datafile
write_csv(pre_survey_filtered, file="data/Clean_data/pre_survey")


## Experiment rounds data -----------------------------------------------

### Binding dataframes
### Condition A, B, C
### Rounds 1, 2, 3
## Groups?

### ALFA -----------------------------------------------------------------

alfa_r1 <- read_excel("data/Alfa Round1.xlsx")

alfa_r1_filtered <- alfa_r1 %>% 
  slice(-1) %>%
  dplyr::select(19:29) %>%
  mutate(Round = "Round1")

alfa_r2 <- read_excel("data/Alfa Round2.xlsx")

alfa_r2_filtered <- alfa_r2 %>% 
  slice(-1) %>%
  dplyr::select(19:29) %>%
  mutate(Round = "Round2")

alfa_r3 <- read_excel("data/Alfa Round3 + post.xlsx")

alfa_r3_filtered <- alfa_r3 %>% 
  slice(-1) %>%
  dplyr::select(19:28, 39) %>%
  mutate(Round = "Round3")

alfa_compl <- bind_rows(alfa_r1_filtered, alfa_r2_filtered, alfa_r3_filtered)

alfa_compl <- alfa_compl %>%
  mutate(Condition = "Condition A")

# write out new datafile
write_csv(alfa_compl, file="data/Clean_data/conditionA")

### BETA -------------------------------------------------------------------

beta_r1 <- read_excel("data/Beta Round1.xlsx")

beta_r1_filtered <- beta_r1 %>% 
  slice(-1) %>%
  dplyr::select(19:29) %>%
  mutate(Round = "Round1")

beta_r2 <- read_excel("data/Beta Round2.xlsx")

beta_r2_filtered <- beta_r2 %>% 
  slice(-1) %>%
  dplyr::select(19:29) %>%
  mutate(Round = "Round2")

beta_r3 <- read_excel("data/Beta Round3 + post.xlsx")

beta_r3_filtered <- beta_r3 %>% 
  slice(-1) %>%
  dplyr::select(19:28, 39) %>%
  mutate(Round = "Round3")

beta_compl <- bind_rows(beta_r1_filtered, beta_r2_filtered, beta_r3_filtered)

beta_compl <- beta_compl %>%
  mutate(Condition = "Condition B")

# write out new datafile
write_csv(beta_compl, file="data/Clean_data/conditionB")

### GAMMA -------------------------------------------------------------------

gamma_r1 <- read_excel("data/Gamma Round1.xlsx")

gamma_r1_filtered <- beta_r1 %>% 
  slice(-1) %>%
  dplyr::select(19:29) %>%
  mutate(Round = "Round1")

gamma_r2 <- read_excel("data/Gamma Round2.xlsx")

gamma_r2_filtered <- gamma_r2 %>% 
  slice(-1) %>%
  dplyr::select(19:29) %>%
  mutate(Round = "Round2")

gamma_r3 <- read_excel("data/Gamma Round3 + post.xlsx")

gamma_r3_filtered <- gamma_r3 %>% 
  slice(-1) %>%
  dplyr::select(19:28, 39) %>%
  mutate(Round = "Round3")

gamma_compl <- bind_rows(gamma_r1_filtered, gamma_r2_filtered, gamma_r3_filtered)

gamma_compl <- gamma_compl %>%
  mutate(Condition = "Condition C")

# write out new datafile
write_csv(gamma_compl, file="data/Clean_data/conditionC")


### ALL DATA (every COND + ROUNDS) ------------------------------------------

all_data <- bind_rows(alfa_compl, beta_compl, gamma_compl)

# write out new datafile for all conditions
write_csv(all_data, file="data/Clean_data/all_conditions")

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

filter_survey <- function(newdata, data) {
  newdata <- 
    data %>%
      slice(-1) %>%
        dplyr::select(18:29)
}

filter_survey(alfa_r1_filtered, alfa_r1)

alfa_r1_filtered <- alfa_r1 %>% 
  slice(-1) %>%
    dplyr::select(18:29)

beta_r1_filtered <- beta_r1 %>% 
  slice(-1) %>%
    dplyr::select(18:29)

gamma_r1_filtered <- gamma_r1 %>% 
  slice(-1) %>%
    dplyr::select(18:29)