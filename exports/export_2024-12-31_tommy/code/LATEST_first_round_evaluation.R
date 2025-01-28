if (parallelize_evaluation_sets_instead_of_grid_rows_in_first_round) {
  workers_for_first_round_evaluation_internal <- workers_for_first_round_evaluation
  run_grid_row_outputs <- pmap(grid_rows, ~run_grid_row(..., model_trained = TRUE, selection_set = NULL, metrics_for_all_pipelines = metrics_for_all_pipelines, metrics_for_winning_pipelines = metrics_for_winning_pipelines, threshold_increment = NULL, parallelize_evaluation_sets_instead_of_grid_rows = TRUE, workers = workers_for_first_round_evaluation_internal))
} else {
  plan(multisession, workers = workers_for_first_round_evaluation)
  workers_for_first_round_evaluation_internal <- NULL
  run_grid_row_outputs <- future_pmap(grid_rows, ~run_grid_row(..., model_trained = TRUE, selection_set = NULL, metrics_for_all_pipelines = metrics_for_all_pipelines, metrics_for_winning_pipelines = metrics_for_winning_pipelines, threshold_increment = NULL, parallelize_evaluation_sets_instead_of_grid_rows = FALSE, workers = workers_for_first_round_evaluation_internal), .options = furrr_options(seed = TRUE))
}

run_grid_row_outputs <- run_grid_row_outputs %>%
  list_rbind() %>%
  unnest(run_grid_row_output) %>%
  unnest(run_step_output)

saveRDS(run_grid_row_outputs, paste0("intermediate_", results_path, ".rds"))
