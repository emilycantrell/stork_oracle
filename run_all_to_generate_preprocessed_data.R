# Emily created this file to do local runs via RStudio

# This file calls all other associated R files. This 
# file also makes it easy to change file paths through command line arguments.

setwd("H:/pmt/stork_oracle/stork_oracle_december_2024")

print(Sys.time())
start <- Sys.time()

library(tidyverse)
library(data.table)
library(furrr)

args <- commandArgs(trailingOnly = TRUE)

data_files <- list(
  prefer_official_train = "H:/DATASETS/train.csv",
  gbapersoontab = "G:/Bevolking/GBAPERSOONTAB/2020/geconverteerde data/GBAPERSOON2020TABV3.csv",
  gbahuishoudensbus = "G:/Bevolking/GBAHUISHOUDENSBUS/geconverteerde data/GBAHUISHOUDENS2020BUSV1.csv",
  familienetwerktab = "G:/Bevolking/FAMILIENETWERKTAB/FAMILIENETWERK2020TABV1.csv",
  prefer_official_holdout_features = "H:/DATASETS/holdout_final_leaderboard.csv",
  prefer_official_holdout_labels = "H:/dataset prep 2024/temporary/final_leaderboard_outcome.csv"
)

prefer_official_train_codebook_path <- "H:/DATASETS/documentation/Codebook UPD.xlsx"

sampling_files_path <-  "H:/pmt/eval/train_and_eval_samples/"

source("jobfile.R")

source("filter_to_prefer_train_and_eval_set.R")

source("feature_engineering.R")

source("create_metadata.R")

source("preprocessing.R")

fwrite(data, "H:/pmt/stork_oracle/stork_oracle_december_2024/preprocessed_files_for_quick_test_runs/data_with_holdout.csv")
fwrite(metadata, "H:/pmt/stork_oracle/stork_oracle_december_2024/preprocessed_files_for_quick_test_runs/metadata.csv")
fwrite(outcome_data, "H:/pmt/stork_oracle/stork_oracle_december_2024/preprocessed_files_for_quick_test_runs/outcome_data_with_holdout.csv")

#source("chunking.R")

#source("run_grid_row.R")

#source("run_metric_for_selecting_pipelines.R")

Sys.time() - start
print(Sys.time())