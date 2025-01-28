options(future.globals.maxSize = +Inf)

plan(multisession, workers = workers_for_fitting_models)


future_pwalk(grid_rows, ~run_grid_row(..., model_trained = FALSE, selection_set = NULL, metrics_for_all_pipelines = metrics_for_all_pipelines, metrics_for_winning_pipelines = metrics_for_winning_pipelines, threshold_increment = NULL, parallelize_evaluation_sets_instead_of_grid_rows = NULL), .options = furrr_options(seed = TRUE))