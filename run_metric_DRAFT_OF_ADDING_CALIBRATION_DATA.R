# This function produces evaluation metrics given predictions and actual outcomes

library(MLmetrics)
library(tidyverse)
library(pROC)

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
    # Note: We are using pROC for AUC because MLMetrics sometimes had an overflow error when calculating AUC.
    # Note: in pROC, the order of arguments is flipped compared to MLmetrics: labels first, predictions second
    output <- pROC::auc(outcomes, predictions_internal)
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
    # 2024-10-31: This originally said "value" instead of "output". Emily changed it to "output" but did not test the change.
    output <- Accuracy(predictions_internal, outcomes)
    ifelse(is.na(output), 0, output)
  }
  run_metric_Deciles_for_Calibration <- function() { 
    breaks_for_deciles <- unique(quantile(predictions_internal, probs = seq(0, 1, by = 0.1)))
    deciles <- cut(predictions_internal, breaks = breaks_for_deciles, include.lowest = TRUE, labels = FALSE)
    mean_predictions <- tapply(predictions_internal, deciles, mean)
    mean_outcomes <- tapply(outcomes, deciles, mean)
    count_per_decile <- tapply(outcomes, deciles, length)
    smallest_count <- min(count_per_decile) # there can be slight variation in size if n is not divisible by 10
    if(smallest_count >= 10) {
      output <- list(mean_predictions = mean_predictions, mean_outcomes = mean_outcomes, count_per_decile = count_per_decile)
    } else {
      output <- "NA: deciles are too small to export"
    }
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