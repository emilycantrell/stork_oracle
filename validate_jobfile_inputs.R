# This file checks that a jobfile is in the correct format

jobfile_to_check <- "jobfile_to_test_validation_script.R"

# Load libraries and read the jobfile
library(tidyverse)
library(data.table)
source(jobfile_to_check)

#### CHECK FEATURE SETS ####

# TODO: Figure out our terminology for combinations of metadata columns vs. individual metadata columns (feature sets vs. feature subsets?)

# Vector of feature columns that exist in the metadata (in the metdata these have the prefix "feature_set_")
feature_subsets_that_exist_in_metadata <- c("GBAPERSOONTAB", 
                                            "prefer_official_train", 
                                            "GBAHUISHOUDENBUS", 
                                            "sex_and_birthyear")

# Function to check the format and contents of feature_sets
check_feature_sets <- function() {
  if (!is.list(feature_sets)) stop("feature_sets is not a list.")
  
  # Check if feature_sets has at least one element
  if (length(feature_sets) == 0) stop("feature_sets list is empty.")
  
  # Initialize a list to collect invalid features for each feature set
  invalid_entries <- list()
  empty_feature_sets <- list()
  
  # Loop over each feature set
  for (set_name in names(feature_sets)) {
    # Check if the current feature set is empty (i.e., no features provided)
    if (length(feature_sets[[set_name]]) == 0) {
      empty_feature_sets[[set_name]] <- "No features provided."
    }
    
    # Find invalid features in the current feature set
    invalid_features <- setdiff(feature_sets[[set_name]], feature_subsets_that_exist_in_metadata)
    
    # If there are any invalid features, add them to the invalid_entries list
    if (length(invalid_features) > 0) {
      invalid_entries[[set_name]] <- invalid_features
    }
  }
  
  # Report all invalid features, if any
  if (length(invalid_entries) > 0) {
    for (set_name in names(invalid_entries)) {
      cat("feature_set", set_name, "contains invalid features:", paste(invalid_entries[[set_name]], collapse = ", "), "\n")
    }
  }
  
  # Report all feature sets that don't contain any features
  if (length(empty_feature_sets) > 0) {
    for (set_name in names(empty_feature_sets)) {
      cat("feature_set", set_name, "does not contain any features.\n")
    }
  }
  
  # Stop if there are invalid entries or feature sets with no features
  if (length(invalid_entries) > 0 || length(empty_feature_sets) > 0) {
    stop("Some feature sets have errors.")
  } else {
    cat("All `feature_sets` entries are valid.\n")
  }
}

#### CHECK SAMPLING FILES #### 

# Vector of valid sampling file names
sampling_files_that_exist <- paste0("pmt_train_and_evaluation_samples_seed_", 1:10, ".csv")

# Function to check that sampling_files contains valid filenames
check_sampling_files <- function() {
  # Check if sampling_files is empty
  if (length(sampling_files) == 0) stop("sampling_files is of length 0.")
  
  # Find which files are not in the valid list
  invalid_files <- setdiff(sampling_files, sampling_files_that_exist)
  
  # Report invalid files, if any
  if (length(invalid_files) > 0) {
    cat("The following sampling files are not in the list of valid file names:\n")
    cat(paste(invalid_files, collapse = "\n"), "\n")
    stop("Some sampling files are not in the valid list.")
  } else {
    cat("All `sampling_files` entries are valid.\n")
  }
}

#### CHECK DATA SPLITS ####

# TODO: Is it correct that test_sets should always be inserted, even if as an empty vector? 
# TODO: Can training_sets or selection_sets ever be an empty vector?

# Read a fake sampling file that has the same column names as the real sampling files,
# to get a list of which training and evaluation sets exist in the sampling files.
sampling_file_example <- fread("fake_data_for_code_testing/pmt_train_and_evaluation_samples_seed_1.csv")
available_training_sets <- grep("^train_", names(sampling_file_example), value = TRUE)
available_evaluation_sets <- grep("^evaluation_", names(sampling_file_example), value = TRUE)

# Function to check the structure of data_splits
check_data_splits <- function() {
  # Check if required columns are present
  if (!("training_sets" %in% colnames(data_splits))) stop("data_splits is missing 'training_sets'. Please provide at least one training set.")
  if (!("selection_sets" %in% colnames(data_splits))) stop("data_splits is missing 'selection_sets'. Please provide at least one selection set.")
  if (!("test_sets" %in% colnames(data_splits))) stop("data_splits is missing 'test_sets'. If you do not want any test sets, you should still insert an empty vector (`test_sets = c()`) in the jobfile.")
  
  # Check for NA values
  if (any(is.na(data_splits$training_sets))) stop("training_sets cannot have an entry of NA.")
  if (any(is.na(data_splits$selection_sets))) stop("selection_sets cannot have an entry of NA.")
  if (any(is.na(data_splits$test_sets))) warning("Warning: some test_sets are NA. This is permitted, but please check that the NA values are intentional.")
  
  # Collect invalid sets
  invalid_training_sets <- setdiff(data_splits$training_sets, available_training_sets)
  invalid_selection_sets <- setdiff(data_splits$selection_sets, available_evaluation_sets)
  test_sets_in_jobfile <- setdiff(data_splits$test_sets, NA) # Takes the vector of test sets without NA values
  invalid_test_sets <- setdiff(test_sets_in_jobfile, available_evaluation_sets)
  
  # Report all invalid sets
  errors_found <- FALSE
  
  if (length(invalid_training_sets) > 0) {
    cat("The following training sets are not in available_training_sets:\n")
    cat(paste(invalid_training_sets, collapse = "\n"), "\n")
    errors_found <- TRUE
  }
  
  if (length(invalid_selection_sets) > 0) {
    cat("The following selection sets are not in available_evaluation_sets:\n")
    cat(paste(invalid_selection_sets, collapse = "\n"), "\n")
    errors_found <- TRUE
  }
  
  if (length(invalid_test_sets) > 0) {
    cat("The following test sets are not in available_evaluation_sets:\n")
    cat(paste(invalid_test_sets, collapse = "\n"), "\n")
    errors_found <- TRUE
  }
  
  if (errors_found) {
    stop("Some data_splits values are not available in the sampling file.")
  } else {
    cat("All `data_splits` entries are valid.\n")
  }
}

#### CHECK MODELS ####

# TODO: Add more detailed checks of "models" to confirm the grid is formatted correctly
# TODO: is it correct that catboost should have "steps" while xgboost and elastic_net should have "n_steps"?
# TODO: If we don't provide a hyperparameter grid, will the code still run? If not, I will make a missing grid an error instead of a warning.
# TODO: Discuss how to validate the hyperparameter grids.
        # Should all possible parameters be listed as permitted values in the grid? Or only ones we've tried before? 
        # For parameters that are permitted to be used, should we validate that the values listed are permitted?
        # Note: there are a LOT of possible parameters, most of which we will never use. Therefore, 
        # if we want to validate the values inputted for each parameter, I think we should permit only
        # parameters that we are actually likely to use (and raise a warning if other parameters are included).

# Define the available model names
available_models <- c("catboost", "xgboost", "elastic_net")

# Function to check the format of "models"
check_models <- function() {
  # Check if models list is empty
  if (length(models) == 0) {
    stop("At least one model must be listed.")
  }
  
  # Get the names of the top-level elements in the models list
  model_names <- names(models)
  
  # Check if all model names are in the available list
  invalid_models <- setdiff(model_names, available_models)
  
  if (length(invalid_models) > 0) {
    cat("The following model names are not in the available list:\n")
    cat(paste(invalid_models, collapse = "\n"), "\n")
    stop("Some models listed in the jobfile are not recognized in the jobfile validation script.")
  }
  
  # Define allowed elements for each model
  allowed_elements <- list(
    catboost = c("grid", "name", "steps"),
    xgboost = c("grid", "name", "n_steps"),
    elastic_net = c("grid", "name", "n_steps")
  )
  
  # Loop through each model and perform checks
  for (model_name in model_names) {
    model <- models[[model_name]]
    
    # Check that each model has a 'name' entry matching the top-level name
    if (!("name" %in% names(model))) {
      stop(paste("Model", model_name, "is missing a 'name' entry."))
    } else if (model$name != model_name) {
      stop(paste("The 'name' entry of model", model_name, "should be:", model_name))
    }
    
    # Check that each model has a 'grid' entry
    if (!("grid" %in% names(model))) {
      warning(paste("Model", model_name, "does not have a hyperparameter grid in the jobfile. A grid is not required, but this warning is raised because we typically want to tune the models."))
    }
    
    # Special check for catboost: ensure 'steps' is included
    if (model_name == "catboost" && !("steps" %in% names(model))) {
      warning("Model 'catboost' is missing a 'steps' entry. A 'steps' entry is not required, but this warning is raised to ensure the omission is intentional.")
    }
    
    # Special check for xgboost and elastic_net: ensure 'n_steps' is included
    if (model_name %in% c("xgboost", "elastic_net") && !("n_steps" %in% names(model))) {
      warning(paste("Model", model_name, "is missing an 'n_steps' entry. An 'n_steps' entry is not required, but this warning is raised to ensure the omission is intentional."))
    }
    
    # Check for any extra or unexpected list elements
    extra_elements <- setdiff(names(model), allowed_elements[[model_name]])
    if (length(extra_elements) > 0) {
      stop(paste("Model", model_name, "has unexpected elements:", paste(extra_elements, collapse = ", ")))
    }
  }
  
  # If all checks pass
  cat("All `models` entries are valid.\n")
}

#### CHECK METRICS ####

# TODO: Make all metric names lowercase? (or whatever we do, just make sure we are consistent)
# TODO: The example jobfile said "R2_Score". Can we change the name to "r2_holdout" for clarity?
# TODO: discuss which metrics be permitted for each vector (and when to raise warnings)

check_metrics <- function() {
  # List valid metrics
  metrics_for_predicted_probabilities <- c("logloss", "mse", "r2_holdout", "auc")
  metrics_for_predicted_classes <- c("f1", "precision", "recall", "accuracy")
  valid_metrics <- c(metrics_for_predicted_probabilities, metrics_for_predicted_classes)
  
  # Check that all metrics pipelines exist and are of length > 0
  if (length(metrics_for_all_pipelines) == 0) {
    stop("metrics_for_all_pipelines is of length 0.")
  }
  
  if (length(metrics_for_selecting_pipelines) == 0) {
    stop("metrics_for_selecting_pipelines is of length 0.")
  }
  
  if (length(metrics_for_winning_pipelines) == 0) {
    stop("metrics_for_winning_pipelines is of length 0.")
  }
  
  # Function to check and list invalid metrics
  check_valid_metrics <- function(metrics, name) {
    invalid_metrics <- setdiff(metrics, valid_metrics)
    if (length(invalid_metrics) > 0) {
      stop(sprintf("%s contains invalid metric names: %s", name, paste(invalid_metrics, collapse = ", ")))
    }
  }
  
  # Check that all vectors contain only valid metric names
  check_valid_metrics(metrics_for_all_pipelines, "metrics_for_all_pipelines")
  check_valid_metrics(metrics_for_selecting_pipelines, "metrics_for_selecting_pipelines")
  check_valid_metrics(metrics_for_winning_pipelines, "metrics_for_winning_pipelines")
  
  # Issue warnings if metrics_for_all_pipelines or metrics_for_selecting_pipelines contain metrics from metrics_for_predicted_classes
  if (any(metrics_for_all_pipelines %in% metrics_for_predicted_classes)) {
    warning("metrics_for_all_pipelines contains metrics that require predicted classes. This will slow down the code because a threshold will need to be set to translate probabilities into classes for every pipeline.")
  }
  
  if (any(metrics_for_selecting_pipelines %in% metrics_for_predicted_classes)) {
    warning("metrics_for_selecting_pipelines contains metrics that require predicted classes. This will slow down the code because a threshold will need to be set to translate probabilities into classes for every pipeline.")
  }
  
  # If all checks pass
  cat("All metrics entries are valid (`metrics_for_all_pipelines`, `metrics_for_selecting_pipelines`, and `metrics_for_winning_pipelines`.)\n")
}

#### CHECK SAVE_ONLY_WINNING_HYPERPARAMETERS ####

# Function to check save_only_winning_hyperparameter_draw_results
check_save_only_winning_hyperparameter_draw_results <- function() {
  if (!is.logical(save_only_winning_hyperparameter_draw_results)) {
    stop("save_only_winning_hyperparameter_draw_results should be TRUE or FALSE.")
  }
  cat("The entry for save_only_winning_hyperparameter_draw_results is valid.\n")
}

#### CHECK N_BOOTSTRAPS ####

# Function to check n_bootstraps
check_n_bootstraps <- function() {
  if (!is.numeric(n_bootstraps) || n_bootstraps <= 0 || n_bootstraps != floor(n_bootstraps)) {
    stop("n_bootstraps should be a positive integer.")
  }
  cat("The entry for n_bootstraps is valid.\n")
}

#### RUN ALL CHECKS ####
check_feature_sets()
check_sampling_files()
check_data_splits()
check_models()
check_metrics()
check_save_only_winning_hyperparameter_draw_results()
check_n_bootstraps()

cat("All checks passed! jobfile.R appears to be correctly formatted.\n")
