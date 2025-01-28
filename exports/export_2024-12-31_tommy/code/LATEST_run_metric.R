# This function produces evaluation metrics given predictions and actual outcomes

run_metric <- function(metric, predictions, outcomes, training_set_outcome_mean, check_threshold_reference_if_applicable, threshold_reference) {
  run_metric_LogLoss <- function() {
    output <- MLmetrics::LogLoss(predictions_internal, outcomes)
    ifelse(is.na(output), 1, output)
  }
  run_metric_MSE <- function() {
    output <- MLmetrics::MSE(predictions_internal, outcomes)
    ifelse(is.na(output), Inf, output)
    
  }
  run_metric_In_Sample_R2 <- function() {
    output <- MLmetrics::R2_Score(predictions_internal, outcomes)
    ifelse(is.na(output), -Inf, output)
  }
  run_metric_R2_Holdout <- function() { 
    sum_of_squares_predictions <- sum((outcomes - predictions_internal)^2)
    sum_of_squares_baseline <- sum((outcomes - training_set_outcome_mean)^2)
    output <- 1 - (sum_of_squares_predictions/sum_of_squares_baseline)
    ifelse(is.na(output), -Inf, output)
  }
  run_metric_AUC <- function() {
    output <- MLmetrics::AUC(predictions_internal, outcomes)
    ifelse(is.na(output), 0, output) # If scores are undefined, then we assume the performance is as bad as it could be
  }
  run_metric_F1_Score <- function() {
    p <- mean(outcomes[predictions_internal == 1])
    r <- mean(predictions_internal[outcomes == 1])
    output <- 2 * (p * r) / (p + r)
    ifelse(is.na(output), 0, output)
  }
  run_metric_Precision <- function() {
    output <- mean(outcomes[predictions_internal == 1])
    ifelse(is.na(output), 0, output)
  }
  run_metric_Recall <- function() {
    output <- mean(predictions_internal[outcomes == 1])
    ifelse(is.na(output), 0, output)
  }
  run_metric_Accuracy <- function() {
    value <- MLmetrics::Accuracy(predictions_internal, outcomes)
    ifelse(is.na(value), 0, value)
  }
  
  if(check_threshold_reference_if_applicable & metric %in% c("F1_Score", "Recall", "Precision", "Accuracy")) {
    winning_threshold <- threshold_reference$winning_threshold[threshold_reference$metric == metric]
    predictions_internal <- ifelse(predictions >= winning_threshold, 1, 0)
    tibble(metric = metric, value = get(paste0("run_metric_", metric))(), threshold = winning_threshold)
  } else {
    predictions_internal <- predictions
    tibble(metric = metric, value = get(paste0("run_metric_", metric))())
  }
  
  
}