# This file creates run_training_set(). It takes a training_set (a name) and 
# filters the data according to the the column in the sampling file with the 
# same name. It also creates a training_set-specific evaluation set by taking
# the intersection of all associated selection_sets and test_sets from the
# jobfile. It then gets the models in the form of a vector of model names
# from the jobfile and generates partial results by applying run_model() to 
# each model. It takes the partial results dataframe and adds a column for the
# training_set.

#### FAKE INPUTS FOR CODE TESTING; REMOVE BEFORE UTILIZING IN A REAL RUN ###################

library(tidyverse)
library(data.table)

# Fake file inputs 
fake_metadata <- fread("~/Documents/GitHub/stork_oracle_cbs/fake_data_for_code_testing/manually_generated_fake_metadata.csv")
fake_sampling_file <- fread("~/Documents/GitHub/stork_oracle_cbs/fake_data_for_code_testing/pmt_train_and_evaluation_samples_seed_1.csv", 
                       colClasses = c(RINPERSOON = "character")) 
fake_data <- fread("~/Documents/GitHub/stork_oracle_cbs/fake_data_for_code_testing/fake_data_file.csv", 
                   colClasses = c(RINPERSOON = "character"))

# Fake jobfile inputs (in a real run, this will be read from the jobfile)
data_splits <- bind_rows(
  expand_grid( # Splits for studying sample size
    training_sets = c(
      "train_sample_n_100",
      "train_sample_n_500",
      "training_set"
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

# Fake functions
run_model <- function(model_name, train_set, selection_set, test_set) { 
  print(paste0("Pretend that this function ran a ", model_name, " model."))
  print("Pretend that the output is a dataframe of model results intead of a dataframe of sample sizes.")
  train_size <- nrow(train_set)
  selection_size <- nrow(selection_set)
  test_size <- nrow(test_set)
  fake_output_df <- cbind.data.frame(train_size, selection_size, test_size)
  return(fake_output_df)
  }

#### REAL CODE BEGINS HERE ############################################################

library(tidyverse)
library(data.table)
library(furrr)

run_training_set <- function(df, sampling_file, training_set_name) {
  # Take a training_set (a name) and filter the data according to the the column 
  # in the sampling file with the same name.
  rinpersoon_vector_for_this_training_set <- sampling_file %>%
    filter(!!sym(training_set_name) == 1) %>% 
    pull(RINPERSOON)
  training_rows <- df %>% 
    filter(RINPERSOON %in% rinpersoon_vector_for_this_training_set)
  
  # Create a training_set-specific selection set by taking the intersection of all 
  # associated selection_sets from the jobfile.
  # TODO: I interpreted this to mean "union" rather than "intersection", is that correct? 
  associated_selection_sets <- data_splits %>%
    filter(training_sets == training_set_name) %>%
    pull(selection_sets) %>%
    unique()
  rinpersoon_vector_for_associated_selection_sets <- sampling_file %>%
    filter(rowSums(select(., all_of(associated_selection_sets)) == 1) > 0) %>%
    pull(RINPERSOON)
  selection_rows <- df %>% 
    filter(RINPERSOON %in% rinpersoon_vector_for_associated_selection_sets)
  
  # Create a training_set-specific test set by taking the intersection of all 
  # associated test_sets from the jobfile.
  # TODO: The original spec said to combine selection and test sets here. I changed this given our discussion of save_only_winning_hyperparameter_draw_results == TRUE. We should discuss.
  associated_test_sets <- data_splits %>%
    filter(training_sets == training_set_name) %>%
    pull(test_sets) %>%
    unique()
  rinpersoon_vector_for_associated_test_sets <- sampling_file %>%
    filter(rowSums(select(., all_of(associated_test_sets)) == 1) > 0) %>%
    pull(RINPERSOON)
  test_rows <- df %>% 
    filter(RINPERSOON %in% rinpersoon_vector_for_associated_test_sets)

  # Get the models in the form of a vector of model names from the jobfile and 
  # generate partial results by applying run_model() to each model.
  model_names <- names(models)
  results_for_this_training_set <- future_map_dfr(model_names,
                                                  ~ run_model(.x,
                                                              train_set = training_rows,
                                                              selection_set = selection_rows,
                                                              test_set = test_rows))
  
  # Take the partial results dataframe and adds rows for the training_set.
  # TODO: Original spec said "add column" - I assume that meant "add rows"?
  # TODO: I'm not sure how to bind the results together since we are running in parallel.
  # The following code might need to be changed in order to work in a parallelized run.
  if (exists("results")) {
    results <- rbind.data.frame(results, results_for_this_training_set)
  } else {
    results <- results_for_this_training_set
  }
  
  return(results)
}
