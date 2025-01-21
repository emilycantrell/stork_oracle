# This is a configuration file specifying settings for parallelizaton, feature
# choices, train-test splits, modeling options, and results output

# Set up
library(tidyverse)

# Parallelization settings
seed_job <- 0
seed_worker <- 0
workers <- 4
#n_thread_within_worker <- -1

# Feature choices
feature_set_settings <- list(
  ego_AS = c("ego_AS"), 
  ego_partner_AS = c("ego_AS", "partner_AS"), 
  ego_partner_hhchildren_AS = c("ego_AS", "partner_AS", "child_age_and_sex_by_household")
  #all_plus_LiveInPartner = c("GBAPERSOONTAB", "GBAHUISHOUDENSBUS", "prefer_official_train", "FAMILIENETWERKTAB", "live_in_partner")
  #all_plus_hhchildAS = c("GBAPERSOONTAB", "GBAHUISHOUDENSBUS", "prefer_official_train", "FAMILIENETWERKTAB", "child_age_and_sex_by_household")
  #all_records = c("GBAPERSOONTAB", "GBAHUISHOUDENSBUS", "prefer_official_train", "FAMILIENETWERKTAB")
  #augmented_records = c("GBAPERSOONTAB", "GBAHUISHOUDENSBUS", "prefer_official_train")
  #householdbus = c("GBAHUISHOUDENSBUS")
  #number_of_children = c("number_of_children")
  #household_birthdays = c("household_birthdays")
)

# Train-test splits 
sampling_files <- c("pmt_train_and_evaluation_samples_seed_1_241016.csv")
data_splits <- bind_rows(
  expand_grid(
    training_sets = c("train_sample_n_100",
                      "train_sample_n_500", 
                      "train_sample_n_1000", 
                      "train_sample_n_5000", 
                      "train_sample_n_10000", 
                      "train_sample_n_50000", 
                      "train_sample_n_100000",
                      "train_sample_n_500000",
                      "train_sample_n_1000000",
                      "train_sample_n_2000000",
                      "train_sample_n_3000000",
                      "training_set"),
    selection_sets = c("evaluation_set"), # Evaluation sets we use to select the best pipelines
    test_sets = c("evaluation_set") # Evaluation sets we use for holdout evaluations.
  )
)

# Modeling options
model_settings <- list(
  # These are default catboost settings, and these are the only model-
  # hyperparameter combination we have time to explore, but our code supports
  # tuning other models and hyperparameter options, as commented out below
  catboost = list(tibble(grid_row = pmap(expand_grid(
    learning_rate = c(NA),
    subsample = c(.8),
    depth = c(6)
  ),
  list)),
  steps = c(1000)) # how many trees in catboost?
  # catboost = list(tibble(grid_row = pmap(expand_grid(
  #   learning_rate = c(.009, .03, .09, .3, .9, NA),
  #   subsample = c(.2, .5, .8, 1),
  #   depth = c(1, 2, 4, 6, 8, 10)
  # ),
  # list)),
  # steps = c(200, 400, 600, 800, 1000)),
  # xgboost = list(tibble(grid_row = pmap(expand_grid(
  #   eta = c(.009, .03, .09, .3, .9),
  #   subsample = c(.2, .5, .8, 1),
  #   max_depth = c(1, 2, 4, 6, 8, 10)
  # ),
  # list)),
  # steps = c(200, 400, 600, 800, 1000)),
  # elastic_net = list(tibble(grid_row = pmap(bind_rows(expand_grid(
  #   alpha = c(0, .15, .3, .5, .7, .85, 1),
  #   lambda = c(NA)
  # ),
  # tibble(alpha = 1, lambda = 0)),
  # list)),
  # steps = c(1, 2, 3, 4, 5))
  )
n_grid_row <- 1 # how many hyperparameter combinations to sample from expanded
# grid?

# Performance metrics
metrics_for_all_pipelines <- c("LogLoss", "MSE", "R2_Score", "AUC")
metrics_for_selecting_pipelines <- c("LogLoss")
metrics_for_winning_pipelines <- c("F1_Score")
threshold_increment <- .01
n_bootstrap <- 100
# in confidence intervals for this submission

save_only_winning_hyperparameter_draw_results <- FALSE
results_path <- "results.csv"