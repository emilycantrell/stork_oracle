# For each step option as specified in the jobfile, or for the best step
# option if we already found the best pipeline, we apply run_step to make
# predictions for each relevant evaluation set. We then evaluate the 
# predictions.

# In addition, if we have already picked a best pipeline using the selection 
# set, we can now determine the best classification threshold for each 
# classification metric

run_step <- function(step, sampling_file_content, model, model_path, model_fit, evaluation_sets, first_round_evaluation_sets, evaluation_sets_data, evaluation_sets_rinpersoon, selection_set, selection_set_rinpersoon, selection_set_outcomes, bootstrap_samples, training_set_outcome_mean, metrics_for_all_pipelines, metrics_for_winning_pipelines, threshold_increment, parallelize_evaluation_sets_instead_of_grid_rows, workers, save_only_winning_hyperparameter_draw_results) {
  
  evaluation_sets_predictions <- NULL
  
  predict_for_catboost <- function() {
    evaluation_sets_predictions <<- catboost.predict(model_fit, evaluation_sets_data, prediction_type = "Probability", ntree_end = step, thread_count = 1)
  }
  
  predict_for_training_mean <- function() {
    evaluation_sets_predictions <<- rep(model_fit$mean, nrow(evaluation_sets_data))
  }
  
  predict_for_xgboost <- function() {
    evaluation_sets_predictions <<- predict(model_fit, evaluation_sets_data, iterationrange = c(1, step + 1))
  }
  
  predict_for_elastic_net <- function() {
    evaluation_sets_predictions <<- predict(model_fit, evaluation_sets_data, s = model_fit$lambda[[step]], type = "response")
  }
  
  get(paste0("predict_for_", model))()
  
  if (!is.null(selection_set)) {
    selection_set_predictions <- evaluation_sets_predictions[evaluation_sets_rinpersoon %in% selection_set_rinpersoon]
    thresholds <- seq(min(selection_set_predictions), max(selection_set_predictions) + threshold_increment, threshold_increment)
    threshold_reference <- map(thresholds, ~run_threshold(.x, selection_set_predictions, metrics_for_winning_pipelines)) %>%
      list_rbind() %>%
      unnest(run_threshold_output) %>%
      group_by(metric) %>%
      summarize(winning_threshold = sample(threshold[value == max(value)], 1))
  } else {
    threshold_reference <- NULL
    write.csv(tibble(rinpersoon = evaluation_sets_rinpersoon, prediction = evaluation_sets_predictions),
              paste0(model_path, ",step=", step, ".csv"))
  }
  
  if (parallelize_evaluation_sets_instead_of_grid_rows) {
    plan(multisession, workers = workers)
    run_step_output <- future_map(evaluation_sets, ~ run_evaluation_set(.x, sampling_file_content, first_round_evaluation_sets, evaluation_sets_predictions, evaluation_sets_rinpersoon, selection_set, bootstrap_samples, training_set_outcome_mean, metrics_for_all_pipelines, metrics_for_winning_pipelines, threshold_reference, save_only_winning_hyperparameter_draw_results), .options = furrr_options(seed = TRUE))
  } else {
    run_step_output <- map(evaluation_sets, ~ run_evaluation_set(.x, sampling_file_content, first_round_evaluation_sets, evaluation_sets_predictions, evaluation_sets_rinpersoon, selection_set, bootstrap_samples, training_set_outcome_mean, metrics_for_all_pipelines, metrics_for_winning_pipelines, threshold_reference, save_only_winning_hyperparameter_draw_results))
    
  }

  tibble(step = step, run_step_output = run_step_output)
}