# This file is designed to be run before testing the code in run_training_set, 
# run_sampling_file, and run_feature_set.R outside the CBS RA environment.

library(tidyverse)
library(data.table)

# Fake file inputs 
fake_metadata <- fread("~/Documents/GitHub/stork_oracle_cbs/fake_data_for_code_testing/manually_generated_fake_metadata.csv")
fake_sampling_file_path <- "~/Documents/GitHub/stork_oracle_cbs/fake_data_for_code_testing/pmt_train_and_evaluation_samples_seed_1.csv" 
fake_data_path <- "~/Documents/GitHub/stork_oracle_cbs/fake_data_for_code_testing/fake_data_file.csv"

# Fake jobfile inputs (in a real run, this will be read from the jobfile)
# TODO: Adjust the code to be able to handle the full training set as a training set option
data_splits <- bind_rows(
  expand_grid( # Splits for studying sample size
    training_sets = c(
      "train_sample_n_100",
      "train_sample_n_500"
    ),
    selection_sets = c("evaluation_selection_50_percent_split", "evaluation_sample_n_1000"),
    test_sets = c("evaluation_test_50_percent_split", "evaluation_sample_n_1000")
  )
)

models <- list(
  catboost = list(
    name = "catboost",
    grid = expand_grid(
      learning_rate = c(.003, .03, .3, NA),
      subsampling = c(.2, .5, .8, 1),
      depth = c(1, 2, 4, 6, 8, 10)
    ),
    steps = c(100, 200, 300, 400)
  ),
  xgboost = list(
    name = "xgboost",
    grid = expand_grid(
      eta = c(0.003, .03, .3),
      subsample = c(.2, .5, .8, 1),
      max_depth = c(1, 2, 4, 6, 8, 10),
    ),
    n_steps = 1000
  ),
  elastic_net = list(
    name = "elastic_net",
    grid = expand_grid(
      alpha = c(0, .15, .3, .5, .7, .85, 1),
      lambda = c(NA)
    ) %>%
      bind_rows(list(alpha = 1, lambda = 0)),
    n_steps = 100
  )
)

# Fake function
run_model <- function(model_name, train_set, selection_set, test_set) { 
  print(paste0("Pretend that this function ran a ", model_name, " model."))
  print("Pretend that the output is a dataframe of model results intead of a dataframe of sample sizes.")
  # Note: I neede to output some sort of dataframe to test the code, so I'm arbitrarily making a dataframe of sample sizes as an example.
  train_size <- nrow(train_set)
  selection_size <- nrow(selection_set)
  test_size <- nrow(test_set)
  fake_output_df <- cbind.data.frame(train_size, selection_size, test_size)
  return(fake_output_df)
}
