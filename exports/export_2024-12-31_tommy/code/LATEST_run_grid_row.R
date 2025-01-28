get_rinpersoon <- function(sets, sampling_file_content) {
  filter(sampling_file_content, rowSums(select(sampling_file_content, all_of(sets))) > 0) %>%
    pull(RINPERSOON)
}

run_grid_row <- function(feature_set, sampling_file, training_set, model, grid_row, step, model_trained, selection_set, metrics_for_all_pipelines, metrics_for_winning_pipelines, threshold_increment, parallelize_evaluation_sets_instead_of_grid_rows, workers) {
  
  
  
  print(Sys.getpid())
  set.seed(seed_worker)
  # this_feature_set <- feature_set
  # this_sampling_file <- sampling_file
  # this_training_set <- training_set
  # this_model <- model
  # this_selection_set <- selection_set
  

  names_of_features_in_the_selected_feature_set <- metadata %>%
    filter(rowSums(select(., all_of(paste0("feature_set_", feature_set_settings[[feature_set]])))) > 0) %>%
    pull(variable_name)
  feature_set_data <- data %>%
    select(RINPERSOON, all_of(names_of_features_in_the_selected_feature_set))

  sampling_file_path <- paste0(sampling_files_path, sampling_file)
  sampling_file_content <- fread(sampling_file_path, colClasses = c(RINPERSOON = "character"))
  
  training_set_rinpersoon <- get_rinpersoon(training_set, sampling_file_content)
  training_set_data <- feature_set_data %>%
    filter(RINPERSOON %in% training_set_rinpersoon)
  zv <- recipe(training_set_data) %>%
    step_zv() %>%
    prep(training_set_data, strings_as_factors = "FALSE")
  # Apply recipe for removing zero variance
  training_set_data <- bake(zv, training_set_data)
  training_set_outcomes <- outcome_data$outcome[outcome_data$RINPERSOON %in% training_set_rinpersoon]
  rm(training_set_rinpersoon)
  
  # Save the mean of the training set outcome for use in R2_Holdout calculation (see run_metric file)
  training_set_outcome_mean <- mean(training_set_outcomes)
  rm(training_set_outcomes)
  
  if (model_trained) {
    bootstrap_samples <- 1:(n_bootstrap+1)
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
      
      selection_set_rinpersoon <- get_rinpersoon(selection_set, sampling_file_content)
      selection_set_outcomes <- outcome_data$outcome[outcome_data$RINPERSOON %in% selection_set_rinpersoon]
    } 
    
    sampling_file_content <- select(sampling_file_content, RINPERSOON, all_of(evaluation_sets))
    evaluation_sets_rinpersoon <- get_rinpersoon(evaluation_sets, sampling_file_content)
    evaluation_sets_data <- filter(feature_set_data, RINPERSOON %in% evaluation_sets_rinpersoon)
    evaluation_sets_data <- bake(zv, evaluation_sets_data)
  } else {
    selection_set_rinpersoon <- NULL
    selection_set_outcomes <- NULL
  }
  rm(feature_set_data)
  
  preprocess_data_for_catboost <- function() {
    recipe_for_model <- recipe(training_set_data) %>%
      step_rm(RINPERSOON, any_of(binary_one_hot_variables)) %>%
      prep(training_set_data)
    # Generate the pools
    if (!model_trained) {
      training_set_data <<- catboost.load_pool(data = bake(recipe_for_model, training_set_data),
                                               label = training_set_outcomes)
    } else {
      evaluation_sets_data <<- catboost.load_pool(data = bake(recipe_for_model, evaluation_sets_data))
    }
  }
  
  preprocess_data_for_training_mean <- function() {
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
  
  model_fit <- NULL
  
  grid_row_text <- paste(names(grid_row), grid_row, sep = "=", collapse = ",")
  model_path <- paste(feature_set, sampling_file, training_set, model, grid_row_text, sep = "_")

  get_model_for_catboost <- function(learning_rate, subsample, depth) {
    if (!model_trained) {
      steps <- max(model_settings[[model]]$steps)
      if (is.na(learning_rate)) {
        learning_rate_internal <- NULL
      } else {
        learning_rate_internal <- learning_rate
      }
      # Train the model
      fit_start <- Sys.time()
      print(paste("Fitting started for", model_path))
      print(fit_start)
      model_fit <<- catboost.train(training_set_data, params = list(
        loss_function = "Logloss",
        iterations = steps,
        learning_rate = learning_rate_internal,
        subsample = subsample,
        depth = depth,
        # thread_count = n_thread_within_worker, # this is commented out because it raised the error: Catboost can't parse parameter "thread_count" with value: -1
        logging_level = "Silent"
      ))
      
      fit_end <- Sys.time()
      print(paste("Fitting ended for", model_path))
      print(fit_end)
      print(paste("Fitting time for", model_path))
      print(fit_end - fit_start)
      
      catboost.save_model(model_fit, model_path)
    } else {
      model_fit <<- catboost.load_model(model_path)
    }
  }
  
  get_model_for_training_mean <- function(hyperparameter) {
    if (!model_trained) {
      # Train the model
      fit_start <- Sys.time()
      print(paste("Fitting started for", model_path))
      print(fit_start)
      model_fit <<- tibble(mean = training_set_outcome_mean)
      fit_end <- Sys.time()
      print(paste("Fitting ended for", model_path))
      print(fit_end)
      print(paste("Fitting time for", model_path))
      print(fit_end - fit_start)
      
      write.csv(model_fit, model_path)
    } else {
      model_fit <<- read.csv(model_path)
    }
  }
  
  get_model_for_xgboost <- function(eta, subsample, max_depth) {
    if (is.null(selection_set)) {
      # Train the model
      steps <<- model_settings[[model]]$steps
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
      xgb.save(model_fit, model_path)
    } else {
      steps <<- filter(run_selection_metric_outputs, run_selection_metric_outputs$feature_set == this_feature_set, run_selection_metric_outputs$sampling_file == this_sampling_file, run_selection_metric_outputs$training_set == this_training_set, run_selection_metric_outputs$model == this_model, run_selection_metric_outputs$selection_set == this_selection_set) %>%
        pull(step)
      model_fit <<- xgb.load(model_path)
    }
    
  }
  
  get_model_for_elastic_net <- function(alpha, lambda) {
    if (is.null(selection_set)) {
      if (is.na(lambda)) {
        lambda_internal <- NULL
        steps_internal <- max(model_settings[[model]]$steps)
        steps <<- model_settings[[model]]$steps
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
      saveRDS(model_fit, model_path)
    } else {
      steps <<- step
      model_fit <<- readRDS(model_path)
    }
  }
  
  do.call(get(paste0("get_model_for_", model)), grid_row)
  
  if(model_trained) {
    if (is.null(selection_set)) {
      steps <- model_settings[[model]]$steps
    } else {
      steps <- step
    }
    steps_start <- Sys.time()
    print(paste("run_steps started for", model_path))
    print(steps_start)
    run_grid_row_output <- map(steps, ~run_step(.x, sampling_file_content, model, model_path, model_fit, evaluation_sets, first_round_evaluation_sets, evaluation_sets_data, evaluation_sets_rinpersoon, selection_set, selection_set_rinpersoon, selection_set_outcomes, bootstrap_samples, training_set_outcome_mean, metrics_for_all_pipelines, metrics_for_winning_pipelines, threshold_increment, parallelize_evaluation_sets_instead_of_grid_rows, workers, save_only_winning_hyperparameter_draw_results))
    steps_end <- Sys.time()
    print(paste("run_steps ended for", model_path))
    print(steps_end)
    print(paste("Time for run_steps to run for", model_path))
    print(steps_end - steps_start)
    
    
    run_grid_row_output <- tibble(feature_set = feature_set, sampling_file = sampling_file, training_set = training_set, model = model, grid_row = list(grid_row), grid_row_text = grid_row_text, run_grid_row_output = run_grid_row_output)
    if (is.null(selection_set)) {
      run_grid_row_output
    } else {
      mutate(run_grid_row_output, selection_set = selection_set)
    }
  }
}