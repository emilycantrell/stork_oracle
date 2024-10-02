# Please run the following in terminal:
# Rscript run_all.R "H:/DATASETS/train.csv" "H:/DATASETS/holdout_intermediate_leaderboard.csv" "H:/DATASETS/holdout_final_leaderboard.csv" "G:/Bevolking/GBAPERSOONTAB/2020/geconverteerde data/GBAPERSOON2020TABV3.csv" "G:/Bevolking/GBAHUISHOUDENSBUS/geconverteerde data/GBAHUISHOUDENS2020BUSV1.csv" "H:/DATASETS/documentation/Codebook UPD.xlsx" "H:/pmt/stork_oracle_submission/"

# This file calls all other R files created by Emily and Tommy, not including 
# pmt_train_and_eval_samples.R and predict_outcomes.R, which are interfaces 
# with other Princeton Megateam projects or the larger PreFer project. This 
# file also makes it easy to change file paths through command line arguments.

# Set up
print(Sys.time())
start <- Sys.time()
args <- commandArgs(trailingOnly = TRUE)
data_files <- list(
  prefer_official_train = args[1], # "H:/DATASETS/train.csv",
  intermediate = args[2], # "H:/DATASETS/holdout_intermediate_leaderboard.csv",
  final = args[3], # "H:/DATASETS/holdout_final_leaderboard.csv",
  gbapersoontab = args[4], # "G:/Bevolking/GBAPERSOONTAB/2020/geconverteerde data/GBAPERSOON2020TABV3.csv",
  gbahuishoudensbus = args[5] # "G:/Bevolking/GBAHUISHOUDENSBUS/geconverteerde data/GBAHUISHOUDENS2020BUSV1.csv"
)
prefer_official_train_codebook_path <- args[length(args)-1] # "H:/DATASETS/documentation/Codebook UPD.xlsx"
sampling_files_path <- args[length(args)] # "H:/pmt/stork_oracle_submission/"

# Settings for parallelizaton, feature choices, train-test splits, modeling 
# options, performance metrics, and results output
source("jobfile.R")

# Subset raw data to only the rows and columns we need
source("filtering_and_selecting.R")

# Create meaningfully new features at the individual level
source("feature_engineering.R")

# Distinguish continuous from categorical variables, prepare for manual feature 
# selection using what we call "feature sets", but we don't really do manual
# feature selection in this submission.
source("create_metadata.R")

# Handle missingness and, for future versions, dummy encode the data. Save
# leaderboards RDS files 
source("preprocessing.R")

# Prepare tasks for parallelized workers. For now there is just one worker.
source("chunking.R")

# This file contains a function run_grid_row() that defines what a worker does. 
# The file then runs the function for all tasks and collects results, getting a 
# model for each feature_set-sampling_file-training_set-model-hyperparameter_
# grid_row combination and producing performance metrics (Within each grid_row,
# each task loops through multiple options for an additional hyperparameter we 
# call "step"--total number of trees for boosted trees or lambda for elastic 
# net.) 
# For this submission we only have one task for one worker: fit a default
# catboost model using all available features and pmt_train_and_eval_samples_
# seed_1.csv as sampling file, using a training set of a given size.
source("run_grid_row.R")
# run_grid_row() contains a series of nested loops, with each level represented by a different function and R file
#   run_step() produces results for each possible number of total trees as specified in the jobfile (or each possible lambda value for elastic_net if we were to use that model)
#     run_evaluation_set() produces results for each applicable selection set (an evaluation set used to select the best pipeline) and test set (an evaluation set used as a holdout test set). Right now we only have one evaluation set. It's a selection set that correspond to evaluation_set in train.csv.
#       run_bootstrap_sample() produces results for each bootstrap sample of the evaluation set.
#         run_metric() produces results for each metric we want to measure

# This file contains a function run_metric_for_selecting_pipelines() that
# selects the best-performing grid_row-step at the feature_set-sampling_file-
# training_set-model level using a selection set, but we only have one grid_
# row-step to select from for now. We then run the run_grid_row() function 
# again to further find the best classification threshold.
source("run_metric_for_selecting_pipelines.R")
# When we run
# run_grid_row() again after having picked the best pipeline using a selection set
#   run_step() would lead to additional nested loop in addition to the one led by run_evaluation_set. This nested loop helps us find the best threshold
#     run_threshold() produces performance measures for each possible classification threshold
#       run_metric() produces performance measures for each metric we want to measure


# Finish up
Sys.time() - start
print(Sys.time())