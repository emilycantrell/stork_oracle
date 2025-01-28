# Once models are fitted and selection metrics are evaluated in selection sets,
# the function run_metric_for_selecting_pipelines(), defined here, selects the
# best pipeline. This file runs the function for each feature_set-sampling_
# file-training_set-model-selection_set combination (though for this submission 
# there is only one such submission). Then the file calls run_grid_row() again 
# on the best pipelines to get classification thresholds and prediction metrics 
# not calculated first time around. The code then saves all of the results

run_metric_for_selecting_pipelines <- function(selection_metric) {
  estimate_selection_metric <- sym(paste0("estimate_", selection_metric))
  if (selection_metric %in% c("LogLoss", "MSE")) {
    best <- min
  } else {
    best <- max
  }
  run_selection_metric_output <- group_by(run_grid_row_outputs_for_selection, feature_set, sampling_file, training_set, model, selection_set) %>%
    filter(!!estimate_selection_metric == best(!!estimate_selection_metric)) %>%
    slice_sample() %>%
    ungroup()
  
  tibble(selection_metric = selection_metric, run_selection_metric_output = run_selection_metric_output)
}