# This file defines how each worker should implement the specifications laid
# out in chunking.R or by run_metric_for_selecting_pipelines_outputs in 
# run_metric_for_selecting_pipelines.R. Each worker filters the data to the
# specified training set and corresponding evaluation sets, conduct some final
# pipeline-specific preprocessing, get the fitted model, make predictions for 
# evaluation sets, and calculate performance metrics.

# The worker function has two modes.
# When the argument selection_set is NULL, we have yet to fit the model, 
# evaluate on a selection_set, or find the best hyperparameters (if we were to
# try multiple hyperparameters). This means that we get the model by actually 
# fitting the model on the training set. Then we save the model and evaluate
# it using a selection set.

# When the argument selection_set is not NULL, we would have already fitted a
# model and found the best hyperparameters using a selection_set. We would now
# get the model by loading the saved model object. Then we make predictions
# again, find the best classification threshold using the selection_set, and 
# evaluate in test_sets (we don't have test_sets for this submission).

# Set up
library(furrr)
library(tidymodels)
library(catboost)
library(xgboost)
library(glmnet)
library(tidyverse)

# The worker function
run_grid_row <- function(feature_set, sampling_file, training_set, model, grid_row, step, selection_set, metrics_for_all_pipelines, metrics_for_winning_pipelines, n_bootstrap, threshold_increment) {
  # Set up
  set.seed(seed_worker)
  bootstrap_samples <- 1:(n_bootstrap+1) # 1 additional iteration for the original un-boostrapped evaluation sample
  this_feature_set <- feature_set
  this_sampling_file <- sampling_file
  this_training_set <- training_set
  this_model <- model
  this_selection_set <- selection_set
  source("run_step.R", local = TRUE)
  
  # Select features from feature set
  names_of_features_in_the_selected_feature_set <- metadata %>%
    filter(rowSums(select(., all_of(paste0("feature_set_", feature_set_settings[[feature_set]])))) > 0) %>%
    pull(variable_name)
  feature_set_data <- data %>%
    select(RINPERSOON, all_of(names_of_features_in_the_selected_feature_set))
  
  
  # Filter rows to create training and evaluation sets using split indicators
  # from the sampling file
  # Create training_set 
  sampling_file_path <- paste0(sampling_files_path, sampling_file)
  sampling_file_content <- fread(sampling_file_path, colClasses = c(RINPERSOON = "character"))
  # This function gets corresponding rinpersoons given a particular named
  # subset of data
  get_rinpersoon <- function(sets) {
    filter(sampling_file_content, rowSums(select(sampling_file_content, all_of(sets))) > 0) %>%
      pull(RINPERSOON)
  }
  training_set_rinpersoon <- get_rinpersoon(training_set)
  training_set_data <- feature_set_data %>%
    filter(RINPERSOON %in% training_set_rinpersoon)
  # Remove columns with zero variance in the training set
  remove_zero_variance <- recipe(training_set_data) %>%
    step_zv() %>%
    prep(training_set_data, strings_as_factors = "FALSE")
  # Save the remove_zero_variance recipe
  if (is.null(selection_set)) {
    saveRDS(remove_zero_variance, "remove_zero_variance.RDS")
  }
  training_set_data <- bake(remove_zero_variance, training_set_data)
  training_set_outcomes <- outcome_data$outcome[outcome_data$RINPERSOON %in% training_set_rinpersoon]
  data_splits_related_to_training_set <- 
    filter(data_splits, training_sets == training_set)
  
  # Get all of the rows that are in the relevant evaluation sets, including 
  # selection sets and test sets
  selection_sets <- pull(data_splits_related_to_training_set, selection_sets) %>%
    unique()
  if (save_only_winning_hyperparameter_draw_results | !"test_sets" %in% colnames(data_splits_related_to_training_set)) {
    first_round_evaluation_sets <- selection_sets
  } else {
    test_sets <- pull(data_splits_related_to_training_set, test_sets)
    first_round_evaluation_sets <- unique(c(selection_sets, test_sets))
  }
  if(is.null(selection_set)) {
    evaluation_sets <- first_round_evaluation_sets
  } else {
    if (!"test_sets" %in% colnames(data_splits_related_to_training_set)) {
      evaluation_sets <- selection_set
    } else {
      test_sets <- filter(data_splits, training_sets == training_set, selection_sets == selection_set) %>%
        pull(test_sets)
      evaluation_sets <- unique(c(selection_set, test_sets))
    }
    selection_set_rinpersoon <- get_rinpersoon(selection_set)
    selection_set_outcomes <- outcome_data$outcome[outcome_data$RINPERSOON %in% selection_set_rinpersoon]
  }
  evaluation_sets_rinpersoon <- get_rinpersoon(evaluation_sets)
  evaluation_sets_data <- filter(feature_set_data, RINPERSOON %in% evaluation_sets_rinpersoon)
  evaluation_sets_data <- bake(remove_zero_variance, evaluation_sets_data)
  
  # Get models. The code is different depending on the model
  steps <- NULL
  model_fit <- NULL
  grid_row_text <- paste(names(grid_row), grid_row, sep = "=", collapse = ",")
  model_path <- "model.cbm"
  # If we were to have multiple pipelines and multiple models, we would need unique model paths
  # model_path <- paste(feature_set, sampling_file, training_set, model, grid_row_text, sep = "_")
  # Function for catboost
  get_catboost_model <- function(learning_rate, subsample, depth) {
    
    # Model-specific preprocessing. This doesn't really do anything for this submission
    recipe_for_model <- recipe(training_set_data) %>%
      step_rm(RINPERSOON, any_of(binary_one_hot_variables)) %>%
      prep(training_set_data)
    # Generate the pools
    training_set_data <- catboost.load_pool(data = bake(recipe_for_model, training_set_data),
                                             label = training_set_outcomes)
    evaluation_sets_data <<- catboost.load_pool(data = bake(recipe_for_model, evaluation_sets_data))
    
    # Fit and save the model if it's not already fit (i.e. we are not tuning 
    # the classification threshold using a selection set)
    if (is.null(selection_set)) {
      # Finalize hyperparameters
      steps <<- model_settings[[model]]$steps
      steps_internal <- max(steps)
      if (!is.na(learning_rate)) {
        learning_rate_internal <- learning_rate
      } else {
        learning_rate_internal <- NULL
      }
      # Train the model
      model_fit <<- catboost.train(training_set_data, params = list(
        loss_function = "Logloss",
        iterations = steps_internal,
        learning_rate = learning_rate_internal,
        subsample = subsample,
        depth = depth,
        logging_level = "Silent"
      ))
      # Save the model
      catboost.save_model(model_fit, model_path)
    } else {
      
      # If we have already fit and saved the best model object, just load it
      steps <<- filter(run_selection_metric_outputs, run_selection_metric_outputs$feature_set == this_feature_set, run_selection_metric_outputs$sampling_file == this_sampling_file, run_selection_metric_outputs$training_set == this_training_set, run_selection_metric_outputs$model == this_model, run_selection_metric_outputs$selection_set == this_selection_set) %>%
        pull(step) # Get the best number of steps. For this submission we don't
      # evaluate at different step options. Otherwise we would have needed to 
      # save what the best step is.
      model_fit <<- catboost.load_model(model_path)
    }
  }
  
  # We have code for xgboost and elastic_net, but they are not used here
  # get_xgboost_model <- function(eta, subsample, max_depth) {
  #   recipe_for_model <- recipe(training_set_data) %>%
  #     step_rm(RINPERSOON, any_of(categorical_variables)) %>%
  #     prep(training_set_data)
  #   # Generate the pools
  #   training_set_data <- xgb.DMatrix(data = as.matrix(bake(recipe_for_model, training_set_data)),
  #                                     label = training_set_outcomes)
  #   evaluation_sets_data <<- xgb.DMatrix(data = as.matrix(bake(recipe_for_model, evaluation_sets_data)))
  #   
  #   if (is.null(selection_set)) {
  #     # Train the model
  #     steps <<- model_settings[[model]]$steps
  #     model_fit <<- xgb.train(
  #       params = list(
  #         objective = "binary:logistic", 
  #         eta = eta,
  #         subsample = subsample,
  #         max_depth = max_depth,
  #         nthread = 1
  #       ), 
  #       data = training_set_data,
  #       nrounds = max(steps)
  #     )
  #     xgb.save(model_fit, model_path)
  #   } else {
  #     steps <<- filter(run_selection_metric_outputs, run_selection_metric_outputs$feature_set == this_feature_set, run_selection_metric_outputs$sampling_file == this_sampling_file, run_selection_metric_outputs$training_set == this_training_set, run_selection_metric_outputs$model == this_model, run_selection_metric_outputs$selection_set == this_selection_set) %>%
  #       pull(step)
  #     model_fit <<- xgb.load(model_path)
  #   }
  # }
  # 
  # get_elastic_net_model <- function(alpha, lambda) {
  #   recipe_for_model <- recipe(training_set_data) %>%
  #     step_rm(RINPERSOON, any_of(categorical_variables)) %>%
  #     step_impute_mean(any_of(continuous_variables)) %>%
  #     prep(training_set_data)
  #   training_set_data <- bake(recipe_for_model, training_set_data) %>%
  #     as.matrix()
  #   evaluation_sets_data <<- bake(recipe_for_model, evaluation_sets_data) %>%
  #     as.matrix()
  #   
  #   if (is.null(selection_set)) {
  #     if (is.na(lambda)) {
  #       lambda_internal <- NULL
  #       steps_internal <- max(model_settings[[model]]$steps)
  #       steps <<- model_settings[[model]]$steps
  #     } else {
  #       lambda_internal <- lambda
  #       steps_internal <- 1
  #       steps <<- 1
  #     }
  #     model_fit <<- glmnet(training_set_data, training_set_outcomes,
  #                          family = "binomial",
  #                          alpha = alpha,
  #                          lambda = lambda_internal,
  #                          nlambda = steps_internal)
  #     saveRDS(model_fit, model_path)
  #   } else {
  #     steps <<- filter(run_selection_metric_outputs, run_selection_metric_outputs$feature_set == this_feature_set, run_selection_metric_outputs$sampling_file == this_sampling_file, run_selection_metric_outputs$training_set == this_training_set, run_selection_metric_outputs$model == this_model, run_selection_metric_outputs$selection_set == this_selection_set) %>%
  #       pull(step)
  #     model_fit <<- readRDS(model_path)
  #   }
  # }
  
  do.call(get(paste0("get_", model, "_model")), grid_row)
  
  # For each step option as specified in the jobfile, or for the best step
  # option if we already found the best pipeline, we produce partial results
  # by applying run_step() to the step option. See run_step.R for details.
  run_grid_row_output <- map(steps, run_step)
  run_grid_row_output <- tibble(feature_set = feature_set, sampling_file = sampling_file, training_set = training_set, model = model, grid_row = list(grid_row), grid_row_text = grid_row_text, run_grid_row_output = run_grid_row_output)
  if (is.null(selection_set)) {
    run_grid_row_output
  } else {
    mutate(run_grid_row_output, selection_set = selection_set)
  }
}

# Apply run_grid_row to each task
options(future.globals.maxSize = +Inf)
plan(multisession, workers = workers)
run_grid_row_outputs <- future_pmap(grid_rows, ~run_grid_row(..., selection_set = NULL, metrics_for_all_pipelines = metrics_for_all_pipelines, metrics_for_winning_pipelines = metrics_for_winning_pipelines, n_bootstrap = n_bootstrap, threshold_increment = NULL), .options = furrr_options(seed = TRUE)) %>%
  list_rbind() %>%
  unnest(run_grid_row_output) %>%
  unnest(run_step_output)