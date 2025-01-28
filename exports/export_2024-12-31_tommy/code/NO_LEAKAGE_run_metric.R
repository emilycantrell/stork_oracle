# This function produces evaluation metrics given predictions and actual outcomes

library(MLmetrics)
library(tidyverse)

run_metric <- function(metric, predictions, outcomes, check_threshold_reference_if_applicable) {
  run_metric_LogLoss <- function() {
    LogLoss(predictions_internal, outcomes)
  }
  run_metric_MSE <- function() {
    MSE(predictions_internal, outcomes)
  }
  run_metric_R2_Score <- function() {
    output <- R2_Score(predictions_internal, outcomes)
    ifelse(is.na(output), -Inf, output)
  }
  run_metric_AUC <- function() {
    output <- AUC(predictions_internal, outcomes)
    ifelse(is.na(output), 0, output)
  }
  run_metric_F1_Score <- function() {
    output <- F1_Score(predictions_internal, outcomes, positive = "1")
    ifelse(is.na(output), 0, output)
  }
  run_metric_Precision <- function() {
    output <- Precision(predictions_internal, outcomes, positive = "1")
    ifelse(is.na(output), 0, output)
  }
  run_metric_Recall <- function() {
    output <- Recall(predictions_internal, outcomes, positive = "1")
    ifelse(is.na(output), 0, output)
  }
  run_metric_Accuracy <- function() {
    value <- Accuracy(predictions_internal, outcomes)
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