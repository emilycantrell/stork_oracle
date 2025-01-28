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
  familienetwerktab = args[4] # "G:/Bevolking/FAMILIENETWERKTAB/FAMILIENETWERK2020TABV1.csv"
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