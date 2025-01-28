# Please run the following in terminal:
# TODO: THIS TERMINAL INSTRUCTION IS OUTDATED, DISCUSS WITH MALTE
# Rscript run_all.R "H:/DATASETS/train.csv" "H:/DATASETS/holdout_intermediate_leaderboard.csv" "H:/DATASETS/holdout_final_leaderboard.csv" "G:/Bevolking/GBAPERSOONTAB/2020/geconverteerde data/GBAPERSOON2020TABV3.csv" "G:/Bevolking/GBAHUISHOUDENSBUS/geconverteerde data/GBAHUISHOUDENS2020BUSV1.csv" "H:/DATASETS/documentation/Codebook UPD.xlsx" "H:/pmt/stork_oracle_submission/"

# This file calls all other associated R files. This 
# file also makes it easy to change file paths through command line arguments.

print(Sys.time())
start <- Sys.time()

library(tidyverse)
library(data.table)
library(furrr)

args <- commandArgs(trailingOnly = TRUE)

sampling_files_path <- args[1] # "H:/pmt/eval/train_and_eval_samples/"
data_path <- args[2] # "H:/pmt/stork_oracle/stork_oracle_december_2024/preprocessed_files_for_quick_test_runs/data.csv"
outcome_data_path <- args[3] # "H:/pmt/stork_oracle/stork_oracle_december_2024/preprocessed_files_for_quick_test_runs/outcome_data.csv"
metadata_path <- args[4] # "H:/pmt/stork_oracle/stork_oracle_december_2024/preprocessed_files_for_quick_test_runs/metadata.csv"
data <- fread(data_path, 
              colClasses = c(RINPERSOON = "character"))
outcome_data <- fread(outcome_data_path,
                      colClasses = c(RINPERSOON = "character"))
metadata <- fread(metadata_path)
binary_one_hot_variables <- c()

source("jobfile.R")
# 
# source("filter_to_prefer_train_and_eval_set.R")
# 
# source("feature_engineering.R")
# 
# source("create_metadata.R")
# 
# source("preprocessing.R")

time_preprocessing_was_completed <- Sys.time()
print("Time preprocessing was completed:")
print(time_preprocessing_was_completed)
print("Time taken to complete preprocessing:")
print(time_preprocessing_was_completed - start)

source("chunking.R")

source("run_grid_row.R")

time_first_round_completed <- Sys.time()
print("Time first round completed:")
print(time_first_round_completed)
print("Time taken to complete first round:")
print(time_first_round_completed - time_preprocessing_was_completed)

source("run_metric_for_selecting_pipelines.R")

time_second_round_completed <- Sys.time()
print("Time second round completed:")
print(time_second_round_completed)
print("Time taken to complete second round:")
print(time_second_round_completed - time_first_round_completed)
print("Total time:")
print(time_second_round_completed - start)