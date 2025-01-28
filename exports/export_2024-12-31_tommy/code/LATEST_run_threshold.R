# For each possible value of classification threshold, get performance measures
# in terms of classification metrics.
run_threshold <- function(threshold, selection_set_predictions, metrics_for_winning_pipelines) {

  selection_set_label_predictions <- ifelse(selection_set_predictions >= threshold, 1, 0)
  run_threshold_output <- map(metrics_for_winning_pipelines, ~run_metric(.x, selection_set_label_predictions, NULL, selection_set_outcomes, FALSE, NULL))
  tibble(threshold = threshold, run_threshold_output = run_threshold_output)
}