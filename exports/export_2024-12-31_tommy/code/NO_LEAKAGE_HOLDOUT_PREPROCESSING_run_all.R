# This file calls all other associated R files. This 
# file also makes it easy to change file paths through command line arguments.

print(Sys.time())
start <- Sys.time()

library(tidyverse)
library(data.table)
library(furrr)

setwd("H:/pmt/stork_oracle/final_submission_no_leakage/code_for_preprocessed_holdout_data_without_leakage_variable")

args <- commandArgs(trailingOnly = TRUE)

data_files <- list(
  # When training, prefer_official_train is the train set. 
  # Here, it is the final holdout set, because we want a preprocessed version of the holdout set.
  prefer_official_train = "H:/DATASETS/holdout_final_leaderboard.csv",
  gbapersoontab = "G:/Bevolking/GBAPERSOONTAB/2020/geconverteerde data/GBAPERSOON2020TABV3.csv",
  gbahuishoudensbus = "G:/Bevolking/GBAHUISHOUDENSBUS/geconverteerde data/GBAHUISHOUDENS2020BUSV1.csv",
  familienetwerktab = "G:/Bevolking/FAMILIENETWERKTAB/FAMILIENETWERK2020TABV1.csv"
)

prefer_official_train_codebook_path <- "H:/DATASETS/documentation/Codebook UPD.xlsx"

sampling_files_path <- "H:/pmt/eval/train_and_eval_samples/"

source("jobfile.R")

source("filter_to_prefer_train_and_eval_set.R")

source("feature_engineering.R")

source("create_metadata.R")

source("preprocessing.R")

saveRDS(data, "preprocessed_holdout_data_no_leakage_2024-12-13.rds")

Sys.time() - start
print(Sys.time())