# Processor	Intel(R) Xeon(R) Gold 6442Y   2.60 GHz  (2 processors) (maximum 4 workers)
# Installed RAM	128 GB
# System type	64-bit operating system, x64-based processor
# Disk Space (F: 3.25TB; G: 149GB; H: 120TB; K: 149GB; L: 28.5TB; M: 786GB; T: 3.25TB)
# Edition	Windows 11 Enterprise
# Version	23H2
# Installed on	2/14/2024
# OS build	22631.4602
# Experience Windows Feature Experience Pack 1000.22700.1055.0

library(catboost) # 1.2.5 on CBS server
library(data.table) # 1.16.2 on CBS server
library(fastDummies) # 1.7.3 on CBS server
library(furrr) # 0.3.1 on CBS server
library(glmnet) # 4.1-8 on CBS server
library(MLmetrics) # 1.1.3 on CBS server
library(tidymodels) # 1.2.0 on CBS server
library(tidyverse) # 2.0.0 on CBS server
library(xgboost) # 1.7.7.1 on CBS server
# library(pROC) # 1.18.5 on CBS server


# This file calls all other associated R files. This 
# file also makes it easy to change file paths through command line arguments.

print(Sys.time())
start <- Sys.time()

library(tidyverse) # 2.0.0 on CBS server
library(data.table) # 1.16.2 on CBS server
library(furrr) # 0.3.1 on CBS server

args <- commandArgs(trailingOnly = TRUE)

# data_files <- list(
#   prefer_official_train =  "H:/DATASETS/train.csv",
#   gbapersoontab =  "G:/Bevolking/GBAPERSOONTAB/2020/geconverteerde data/GBAPERSOON2020TABV3.csv",
#   gbahuishoudensbus =  "G:/Bevolking/GBAHUISHOUDENSBUS/geconverteerde data/GBAHUISHOUDENS2020BUSV1.csv",
#   familienetwerktab =  "G:/Bevolking/FAMILIENETWERKTAB/FAMILIENETWERK2020TABV1.csv",
#   prefer_official_holdout_features =  "H:/DATASETS/holdout_final_leaderboard.csv",
#   prefer_official_holdout_labels =  "H:/dataset prep 2024/temporary/final_leaderboard_outcome.csv"
# )
# 
# prefer_official_train_codebook_path <-  "H:/DATASETS/documentation/Codebook UPD.xlsx"
# 
# sampling_files_path <-  "H:/pmt/eval/train_and_eval_samples/"

data_files <- list(
  prefer_official_train = args[1], # "H:/DATASETS/train.csv",
  gbapersoontab = args[2], # "G:/Bevolking/GBAPERSOONTAB/2020/geconverteerde data/GBAPERSOON2020TABV3.csv",
  gbahuishoudensbus = args[3], # "G:/Bevolking/GBAHUISHOUDENSBUS/geconverteerde data/GBAHUISHOUDENS2020BUSV1.csv",
  familienetwerktab = args[4], # "G:/Bevolking/FAMILIENETWERKTAB/FAMILIENETWERK2020TABV1.csv",
  prefer_official_holdout_features = args[5], # "H:/DATASETS/holdout_final_leaderboard.csv",
  prefer_official_holdout_labels = args[6] # "H:/dataset prep 2024/temporary/final_leaderboard_outcome.csv"
)

prefer_official_train_codebook_path <- args[length(args)-1] # "H:/DATASETS/documentation/Codebook UPD.xlsx"

sampling_files_path <- args[length(args)] # "H:/pmt/eval/train_and_eval_samples/"

source("jobfile.R")

source("filter_to_prefer_train_and_eval_set.R")

source("feature_engineering.R")

source("create_metadata.R")

source("preprocessing.R")

time_preprocessing_was_completed <- Sys.time()
print("Time preprocessing was completed:")
print(time_preprocessing_was_completed)
print("Time taken to complete preprocessing:")
print(time_preprocessing_was_completed - start)

source("chunking.R")

source("run_grid_row.R")

source("training.R")

time_training_was_completed <- Sys.time()
print("Time training was completed:")
print(time_training_was_completed)
print("Time taken to complete training:")
print(time_training_was_completed - time_preprocessing_was_completed)

source("run_metric.R")

source("run_bootstrap_sample.R")

source("run_evaluation_set.R")

source("run_step.R")

source("first_round_evaluation.R")

time_first_round_evaluation_was_completed <- Sys.time()
print("Time first round evaluation was completed:")
print(time_first_round_evaluation_was_completed)
print("Time taken to complete first round evaluation:")
print(time_first_round_evaluation_was_completed - time_training_was_completed)

source("run_threshold.R")

source("run_metric_for_selecting_pipelines.R")

source("second_round_evaluation.R")

time_second_round_evaluation_was_completed <- Sys.time()
print("Time second round evaluation was completed:")
print(time_second_round_evaluation_was_completed)
print("Time taken to complete second round evaluation:")
print(time_second_round_evaluation_was_completed - time_first_round_evaluation_was_completed)
print("Total time:")
print(time_second_round_evaluation_was_completed - start)