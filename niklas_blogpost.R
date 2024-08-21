# See:
# https://www.niklasjohannes.com/post/calculating-and-visualizing-error-bars-for-within-subjects-designs/

set.seed(42)

#library(Rmisc)
library(tidyverse)
library(truncnorm)

## GENERATE DATA ---------------------------------------------------------------

# number of participants
pp_n <- 30

# three conditions
conditions <- c("A", "B", "C")

# number of trials (measures per condition per participant)
trials_per_condition <- 10

# condition A
condition_a_mean <- 40
condition_a_sd <- 22

# condition B
condition_b_mean <- 45
condition_b_sd <- 17

# condition C
condition_c_mean <- 50
condition_c_sd <- 21

dat <- tibble(
  pp = factor(rep(1:(length(conditions) * trials_per_condition), each = pp_n)), 
  condition = factor(rep(conditions, pp_n * trials_per_condition))
)

# simulate error
pp_error <- tibble(
  # recreate pp identifier
  pp = factor(1:pp_n),
  
  # some bias for the means we use later
  bias_mean = rnorm(pp_n, 0, 6),
  
  # some bias for the sd we use later
  bias_sd = abs(rnorm(pp_n, 0, 3)), 
)

# some random error per trial
error <- rnorm(900, 0, 5)

dat <- left_join(dat, pp_error) %>% # add the bias variables to the data set
  add_column(., error) %>% # add random error
  group_by(pp, condition) %>% 
  mutate(
    score = case_when(
      # get 10 trials per participant and condition
      condition == "A" ~ rtruncnorm(trials_per_condition, a = 0, b = 100, 
                                    (condition_a_mean + bias_mean),
                                    (condition_a_sd + bias_sd)), 
      condition == "B" ~ rtruncnorm(trials_per_condition, a = 0, b = 100,
                                    (condition_b_mean + bias_mean),
                                    (condition_b_sd + bias_sd)),
      condition == "C" ~ rtruncnorm(trials_per_condition, a = 0, b = 100, 
                                    (condition_c_mean + bias_mean),
                                    (condition_c_sd + bias_sd)), 
      TRUE ~ NA_real_
    )
  ) %>% 
  mutate(score = score + error) %>% # add random error
  # because of error, some trials got outside boundary, clip them again here
  mutate(
    score = case_when(
      score < 0 ~ 0, 
      score > 100 ~ 100, 
      TRUE ~ score
    )
  ) %>% 
  select(-bias_mean, -bias_sd, -error) # kick out variables we don't need anymore

## "NORMAL" SE -----------------------------------------------------------------

dat %>% 
  group_by(pp, condition) %>% 
  summarise(agg_mean = mean(score), 
            agg_sd = sd(score))

# wrong - see N = 300
dat_summary <- 
  Rmisc::summarySE(
    dat,
    measurevar = "score",
    groupvars = "condition"
  )

dat_summary

## PRE-AGGREGATED --------------------------------------------------------------

# Better, but still wrong
# (Does not take into account that each participant contributes data to each 
# condition).
dat_agg <- dat %>% 
  group_by(pp, condition) %>% 
  summarise(mean_agg = mean(score))

dat_summary2 <- 
  Rmisc::summarySE(
    dat_agg,
    measurevar = "mean_agg",
    groupvars = "condition"
  )

dat_summary2

## MOREY APPROACH --------------------------------------------------------------

dat_summary3 <- 
  Rmisc::summarySEwithin(
    dat_agg,
    measurevar = "mean_agg",
    withinvars = "condition", 
    idvar = "pp"
  )

dat_summary3
