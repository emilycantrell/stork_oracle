# Please run the following in terminal:
# Rscript run_all.R "H:/DATASETS/train.csv" "G:/Bevolking/GBAPERSOONTAB/2020/geconverteerde data/GBAPERSOON2020TABV3.csv" "G:/Bevolking/GBAHUISHOUDENSBUS/geconverteerde data/GBAHUISHOUDENS2020BUSV1.csv" "G:/Bevolking/FAMILIENETWERKTAB/FAMILIENETWERK2020TABV1.csv" "H:/DATASETS/documentation/Codebook UPD.xlsx" "H:/pmt/eval/train_and_eval_samples/"

# This file calls all other associated R files. This 
# file also makes it easy to change file paths through command line arguments.

print(Sys.time())
start <- Sys.time()

library(tidyverse)
library(data.table)
library(furrr)

args <- commandArgs(trailingOnly = TRUE)

data_files <- list(
  prefer_official_train = args[1], # "H:/DATASETS/train.csv",
  gbapersoontab = args[2], # "G:/Bevolking/GBAPERSOONTAB/2020/geconverteerde data/GBAPERSOON2020TABV3.csv",
  gbahuishoudensbus = args[3], # "G:/Bevolking/GBAHUISHOUDENSBUS/geconverteerde data/GBAHUISHOUDENS2020BUSV1.csv",
  familienetwerktab = args[4], # "G:/Bevolking/FAMILIENETWERKTAB/FAMILIENETWERK2020TABV1.csv",
  prefer_official_holdout_features = args[5], # "H:/DATASETS/holdout_final_leaderboard.csv",
  prefer_official_holdout_labels = args[6] # "H:/dataset prep 2024/temporary/final_leaderboard_outcome.csv"
)

prefer_official_train_codebook_path <- args[length(args) - 2] # "H:/DATASETS/documentation/Codebook UPD.xlsx"

feature_importance_path <- args[length(args) - 1] # "H:/pmt/STORK_ORACLE_FINAL_PREFER_SUBMISSION_FEATURE_IMPORTANCE_2024-12-15/all_plus_LiveInPartner_pmt_train_and_evaluation_samples_seed_1_241016.csv_train_sample_n_1000_catboost_learning_rate=NA,subsample=0.8,depth=6feature_importance"

sampling_files_path <- args[length(args)] # "H:/pmt/eval/train_and_eval_samples/"

source("jobfile.R")

source("filter_to_prefer_train_and_eval_set.R")

source("feature_engineering.R")

source("create_metadata.R")

source("preprocessing.R")

source("chunking.R")

source("run_grid_row.R")

source("run_metric_for_selecting_pipelines.R")

Sys.time() - start
print(Sys.time())