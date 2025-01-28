# For each original/bootstrap sample, calculate metrics

run_bootstrap_sample <- function(bootstrap_sample, evaluation_set_predictions, evaluation_set_outcomes, bootstrapping_indices, n_evaluation_set, training_set_outcome_mean, metrics, check_threshold_reference_if_applicable, threshold_reference) {
  
  if (bootstrap_sample == 1) {
    bootstrapped_evaluation_set_predictions <- evaluation_set_predictions
    bootstrapped_evaluation_set_outcomes <- evaluation_set_outcomes
  } else {
    bootstrapped_indices <- sample(bootstrapping_indices, n_evaluation_set, replace = TRUE)
    bootstrapped_evaluation_set_predictions <- evaluation_set_predictions[bootstrapped_indices]
    bootstrapped_evaluation_set_outcomes <- evaluation_set_outcomes[bootstrapped_indices]
  }
  
  run_bootstrap_sample_output <- map(metrics, ~run_metric(.x, bootstrapped_evaluation_set_predictions, bootstrapped_evaluation_set_outcomes, training_set_outcome_mean, check_threshold_reference_if_applicable, threshold_reference))
  tibble(bootstrap_sample = bootstrap_sample, run_bootstrap_sample_output = run_bootstrap_sample_output)
}