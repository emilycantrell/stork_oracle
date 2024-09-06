# To run this code outside the CBS RA, first run "fake_inputs_for_code_testing.R"
# from the directory stork_oracle_cbs/fake_data_for_code_testing. 

# This file creates run_training_set(). It takes a training_set (a name) and 
# filters the data according to the the column in the sampling file with the 
# same name. It also creates a training_set-specific evaluation set by taking
# the intersection of all associated selection_sets and test_sets from the
# jobfile. It then gets the models in the form of a vector of model names
# from the jobfile and generates partial results by applying run_model() to 
# each model. It takes the partial results dataframe and adds a column for the
# training_set.

library(tidyverse)
library(data.table)
library(furrr)

run_training_set <- function(training_set_name, sampling_file_path, data_path) {
  # Read the sampling file and the data file 
  # Note: The files are read within the function so that they are read individually by each worker when the function is mapped
  sampling_file <- fread(sampling_file_path, colClasses = c(RINPERSOON = "character"))
  df <- fread(data_path, colClasses = c(RINPERSOON = "character"))
  
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
  # TODO: Adjust this code so that it saves the relevant info about the training set
  if (exists("results")) {
    results <- rbind.data.frame(results, results_for_this_training_set)
  } else {
    results <- results_for_this_training_set
  }
  
  return(results)
}

# Example: 
# run_training_set(training_set_name = "train_sample_n_100", sampling_file_path = fake_sampling_file_path, data_path = fake_data_path)