# For each evaluation set, get bootstrapped performance scores.

library(tidyverse)

run_evaluation_set <- function(evaluation_set) {
  
  source("run_bootstrap_sample.R", local = TRUE)
  
  evaluation_set_rinpersoon <- get_rinpersoon(evaluation_set)
  evaluation_set_predictions <- evaluation_sets_predictions[evaluation_sets_rinpersoon %in% evaluation_set_rinpersoon]
  evaluation_set_outcomes <- outcome_data$outcome[outcome_data$RINPERSOON %in% evaluation_set_rinpersoon]
  n_evaluation_set <- length(evaluation_set_rinpersoon)
  bootstrapping_indices <- 1:n_evaluation_set
  
  if (is.null(selection_set)) {
    check_threshold_reference_if_applicable <- FALSE
    metrics <- metrics_for_all_pipelines
  } else {
    check_threshold_reference_if_applicable <- TRUE
    if (evaluation_set %in% first_round_evaluation_sets) {
      metrics <- metrics_for_winning_pipelines
      if (save_only_winning_hyperparameter_draw_results) {
        new_row <- 0
      }
    } else {
      if (save_only_winning_hyperparameter_draw_results) {
        metrics <- unique(c(metrics_for_all_pipelines, metrics_for_winning_pipelines))
        new_row <- 1
      } else {
        metrics <- metrics_for_winning_pipelines
      }
    }
  }
  
  bootstrap_samples_output <- map(bootstrap_samples, run_bootstrap_sample) %>%
    list_rbind() %>%
    unnest(run_bootstrap_sample_output)
  estimates <- filter(bootstrap_samples_output, bootstrap_sample == 1) %>%
    select(-bootstrap_sample) %>%
    rename(estimate = value)
  bootstraps <- filter(bootstrap_samples_output, bootstrap_sample != 1) %>%
    group_by(metric) %>%
    summarize(ci_lower = quantile(value, .025),
              ci_upper = quantile(value, .975))
  run_evaluation_set_output <- left_join(estimates, bootstraps, by = "metric") %>%
    pivot_wider(names_from = metric, values_from = any_of(c("estimate", "ci_lower", "ci_upper", "threshold"))) %>%
    mutate(evaluation_set = evaluation_set)
  
  if (is.null(selection_set) | !save_only_winning_hyperparameter_draw_results) {
    run_evaluation_set_output
  } else {
    mutate(run_evaluation_set_output, new_row = new_row) %>%
      select(-any_of(paste0("threshold_", metrics_for_all_pipelines)))
  }
}