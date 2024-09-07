# To run this code outside of the CBS RA, first run the following files: 
# 1. fake_data_for_code_testing/fake_inputs_for_code_testing.R
# 2. run_training_set.R

# This file creates run_sampling_file(). It takes a sampling_file path and
# reads it. It also gets the training_sets in the form of a vector of 
# training_set names from the jobfile and generates partial results
# by applying run_training_set() to each training_set. It takes the partial
# results dataframe and adds a column for the sampling_file.

library(tidyverse)
library(data.table)
library(furrr)

run_sampling_file <- function(sampling_file_name, data_path) { 
  # Identify the sampling file path (it will be read later within the run_training_set function)
  # TODO: Change location_of_sampling_file to reflect the file path that will be used on OSSC
  location_of_sampling_file <- "~/Documents/GitHub/stork_oracle_cbs/fake_data_for_code_testing/"
  sampling_file_path <- paste0(location_of_sampling_file, sampling_file_name)
  
  # Get the training_sets in the form of a vector of training_set names from the jobfile and generate 
  # partial results by applying run_training_set() to each training_set.
  training_sets <- unique(data_splits$training_sets)
  results_for_this_sampling_file <- future_map_dfr(training_sets,
                                                   ~ run_training_set(.x,
                                                                      sampling_file_path = sampling_file_path,
                                                                      data_path = data_path))

  # Take the partial results dataframe and add a column for the sampling_file.
  results_for_this_sampling_file <- results_for_this_sampling_file %>%
    mutate(sampling_file = sampling_file_name)
  
  return(results_for_this_sampling_file)
  }

# Example
# run_sampling_file(sampling_file_name = "pmt_train_and_evaluation_samples_seed_1.csv", data_path = fake_data_path)
