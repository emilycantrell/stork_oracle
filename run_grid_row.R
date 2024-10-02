# This file defines how each worker should implement the specifications laid
# out in chunking.R or by run_metric_for_selecting_pipelines_outputs in 
# run_metric_for_selecting_pipelines.R. Each worker filters the data to the
# specified training set and corresponding evaluation sets, conduct some final
# pipeline-specific preprocessing, get the fitted model, make predictions for 
# evaluation sets, and calculate performance metrics.

library(tidymodels)
library(catboost)
library(xgboost)
library(glmnet)
library(tidyverse)

run_grid_row <- function(feature_set, sampling_file, training_set, model, grid_row, step, selection_set, metrics_for_all_pipelines, metrics_for_winning_pipelines, n_bootstrap, threshold_increment) {
  
  set.seed(seed_worker)
  bootstrap_samples <- 1:(n_bootstrap+1)
  this_feature_set <- feature_set
  this_sampling_file <- sampling_file
  this_training_set <- training_set
  this_model <- model
  this_selection_set <- selection_set
  source("run_step.R", local = TRUE)
  
  names_of_features_in_the_selected_feature_set <- metadata %>%
    filter(rowSums(select(., all_of(paste0("feature_set_", feature_set_settings[[feature_set]])))) > 0) %>%
    pull(variable_name)
  feature_set_data <- data %>%
    select(RINPERSOON, all_of(names_of_features_in_the_selected_feature_set))
  
  
  
  sampling_file_path <- paste0(sampling_files_path, sampling_file)
  sampling_file_content <- fread(sampling_file_path, colClasses = c(RINPERSOON = "character"))
  
  
  
  get_rinpersoon <- function(sets) {
    filter(sampling_file_content, rowSums(select(sampling_file_content, all_of(sets))) > 0) %>%
      pull(RINPERSOON)
  }
  
  training_set_rinpersoon <- get_rinpersoon(training_set)
  training_set_data <- feature_set_data %>%
    filter(RINPERSOON %in% training_set_rinpersoon)
  zv <- recipe(training_set_data) %>%
    step_zv() %>%
    prep(training_set_data, strings_as_factors = "FALSE")
  training_set_data <- bake(zv, training_set_data)
  training_set_outcomes <- outcome_data$outcome[outcome_data$RINPERSOON %in% training_set_rinpersoon]
  
  data_splits_related_to_training_set <- 
    filter(data_splits, training_sets == training_set)
  
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
  evaluation_sets_data <- bake(zv, evaluation_sets_data)
  
  preprocess_data_for_catboost <- function() {
    recipe_for_model <- recipe(training_set_data) %>%
      step_rm(RINPERSOON, any_of(binary_one_hot_variables)) %>%
      prep(training_set_data)
    
    # Generate the pools
    training_set_data <<- catboost.load_pool(data = bake(recipe_for_model, training_set_data),
                                             label = training_set_outcomes)
    evaluation_sets_data <<- catboost.load_pool(data = bake(recipe_for_model, evaluation_sets_data))
  }
  
  preprocess_data_for_xgboost <- function() {
    recipe_for_model <- recipe(training_set_data) %>%
      step_rm(RINPERSOON, any_of(categorical_variables)) %>%
      prep(training_set_data)
    
    # Generate the pools
    training_set_data <<- xgb.DMatrix(data = as.matrix(bake(recipe_for_model, training_set_data)),
                                      label = training_set_outcomes)
    evaluation_sets_data <<- xgb.DMatrix(data = as.matrix(bake(recipe_for_model, evaluation_sets_data)))
  }
  
  preprocess_data_for_elastic_net <- function() {
    recipe_for_model <- recipe(training_set_data) %>%
      step_rm(RINPERSOON, any_of(categorical_variables)) %>%
      step_impute_mean(any_of(continuous_variables)) %>%
      prep(training_set_data)
    training_set_data <<- bake(recipe_for_model, training_set_data) %>%
      as.matrix()
    evaluation_sets_data <<- bake(recipe_for_model, evaluation_sets_data) %>%
        as.matrix()
  }
  
  get(paste0("preprocess_data_for_", model))() 
  
  steps <- NULL
  model_fit <- NULL

  fit_model_for_catboost <- function(learning_rate, subsample, depth) {
    
    if (is.null(selection_set)) {
      steps <<- model_settings[[model]]$steps
    } else {
      steps <<- filter(run_selection_metric_outputs, run_selection_metric_outputs$feature_set == this_feature_set, run_selection_metric_outputs$sampling_file == this_sampling_file, run_selection_metric_outputs$training_set == this_training_set, run_selection_metric_outputs$model == this_model, run_selection_metric_outputs$selection_set == this_selection_set) %>%
        pull(step)
    }
    if (is.na(learning_rate)) {
      learning_rate_internal <- NULL
      steps_internal <- max(model_settings[[model]]$steps)
    } else {
      learning_rate_internal <- learning_rate
      steps_internal <- max(steps)
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
  }
  
  fit_model_for_xgboost <- function(eta, subsample, max_depth) {
    if (is.null(selection_set)) {
      steps <<- model_settings[[model]]$steps
    } else {
      steps <<- filter(run_selection_metric_outputs, run_selection_metric_outputs$feature_set == this_feature_set, run_selection_metric_outputs$sampling_file == this_sampling_file, run_selection_metric_outputs$training_set == this_training_set, run_selection_metric_outputs$model == this_model, run_selection_metric_outputs$selection_set == this_selection_set) %>%
        pull(step)
    }
    # Train the model
    model_fit <<- xgb.train(
      params = list(
        objective = "binary:logistic", 
        eta = eta,
        subsample = subsample,
        max_depth = max_depth,
        nthread = 1
      ), 
      data = training_set_data,
      nrounds = max(steps)
    )
  }
  
  fit_model_for_elastic_net <- function(alpha, lambda) {
    if (is.na(lambda)) {
      lambda_internal <- NULL
      steps_internal <- max(model_settings[[model]]$steps)
      if (is.null(selection_set)) {
        steps <<- model_settings[[model]]$steps
      } else {
        steps <<- filter(run_selection_metric_outputs, run_selection_metric_outputs$feature_set == this_feature_set, run_selection_metric_outputs$sampling_file == this_sampling_file, run_selection_metric_outputs$training_set == this_training_set, run_selection_metric_outputs$model == this_model, run_selection_metric_outputs$selection_set == this_selection_set) %>%
          pull(step)
      }
    } else {
      lambda_internal <- lambda
      steps_internal <- 1
      steps <<- 1
    }
    
    model_fit <<- glmnet(training_set_data, training_set_outcomes,
                    family = "binomial",
                    alpha = alpha,
                    lambda = lambda_internal,
                    nlambda = steps_internal)
  }
  
  
  do.call(get(paste0("fit_model_for_", model)), grid_row)
  
  run_grid_row_output <- map(steps, run_step)
  run_grid_row_output <- tibble(feature_set = feature_set, sampling_file = sampling_file, training_set = training_set, model = model, grid_row = list(grid_row), run_grid_row_output = run_grid_row_output)
  if (is.null(selection_set)) {
    run_grid_row_output
  } else {
    mutate(run_grid_row_output, selection_set = selection_set)
  }
}

options(future.globals.maxSize = +Inf)

plan(multisession, workers = workers)

run_grid_row_outputs <- future_pmap(grid_rows, ~run_grid_row(..., selection_set = NULL, metrics_for_all_pipelines = metrics_for_all_pipelines, metrics_for_winning_pipelines = metrics_for_winning_pipelines, n_bootstrap = n_bootstrap, threshold_increment = NULL), .options = furrr_options(seed = TRUE)) %>%
  list_rbind() %>%
  unnest(run_grid_row_output) %>%
  unnest(run_step_output)

run_grid_row_outputs %>%
  rowwise() %>%
  mutate(grid_row = paste(names(grid_row), grid_row, sep = "=", collapse = ",")) %>%
  fwrite("intermediate_results.csv")
print(Sys.time())