run_grid_row_outputs <- readRDS(paste0("intermediate_", results_path, ".rds"))

run_grid_row_outputs_for_selection <- rename(run_grid_row_outputs, selection_set = evaluation_set)
if (!save_only_winning_hyperparameter_draw_results) {
  run_grid_row_outputs_for_selection <- distinct(data_splits, training_sets, selection_sets) %>%
    rename(training_set = training_sets, selection_set = selection_sets) %>%
    left_join(run_grid_row_outputs_for_selection, by = c("training_set", "selection_set"), relationship = "one-to-many")
}

source("run_metric_for_selecting_pipelines.R")

set.seed(seed_selection)
run_selection_metric_outputs <- map(metrics_for_selecting_pipelines, run_metric_for_selecting_pipelines) %>%
  list_rbind() %>%
  unnest(run_selection_metric_output)

run_winning_grid_rows_outputs <- select(run_selection_metric_outputs, feature_set, sampling_file, training_set, model, grid_row, step, selection_set)

if (parallelize_evaluation_sets_instead_of_grid_rows_in_second_round) {
  workers_for_second_round_evaluation_internal <- workers_for_second_round_evaluation
  run_winning_grid_rows_outputs <- pmap(run_winning_grid_rows_outputs, ~run_grid_row(..., model_trained = TRUE, metrics_for_all_pipelines = metrics_for_all_pipelines, metrics_for_winning_pipelines = metrics_for_winning_pipelines, threshold_increment = threshold_increment, TRUE, workers = workers_for_second_round_evaluation_internal))
} else {
  plan(multisession, workers = workers_for_second_round_evaluation)
  workers_for_second_round_evaluation_internal <- NULL
  run_winning_grid_rows_outputs <- future_pmap(run_winning_grid_rows_outputs, ~run_grid_row(..., model_trained = TRUE, metrics_for_all_pipelines = metrics_for_all_pipelines, metrics_for_winning_pipelines = metrics_for_winning_pipelines, threshold_increment = threshold_increment, FALSE, workers = workers_for_second_round_evaluation_internal), .options = furrr_options(seed = TRUE))
}

run_winning_grid_rows_outputs <- run_winning_grid_rows_outputs %>%
  list_rbind() %>%
  select(-grid_row) %>%
  rename(grid_row = grid_row_text) %>%
  unnest(run_grid_row_output) %>%
  unnest(run_step_output)

run_grid_row_outputs <- select(run_grid_row_outputs, -grid_row) %>%
  rename(grid_row = grid_row_text)

if(save_only_winning_hyperparameter_draw_results) {
  old_rows <- filter(run_winning_grid_rows_outputs, new_row == 0) %>%
    select(-ends_with(metrics_for_all_pipelines))
} else {
  old_rows <- run_winning_grid_rows_outputs
}

results <- full_join(run_grid_row_outputs, old_rows, by = c("feature_set", "sampling_file", "training_set", "model", "grid_row", "step", "evaluation_set"), relationship = "many-to-many")

if(save_only_winning_hyperparameter_draw_results) {
  new_rows <- filter(run_winning_grid_rows_outputs, new_row == 1)
  results <- filter(results, !is.na(selection_set)) %>%
    bind_rows(new_rows) %>%
    select(-new_row)
} 

fwrite(results, paste0(results_path, ".csv"))
