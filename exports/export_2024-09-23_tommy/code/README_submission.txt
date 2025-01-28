# Please run the following three pieces of code to get...
# 1. Holdout data files that have undergone most, but not all, of the preprocessing:
#    preprocessed_holdout_intermediate.RDS and preprocessed_holdout_final.RDS
# 2. The fitted model model.cbm
# 3. The predictions pmtstork_oracle_intermediate.csv and pmtstork_oracle_final.csv
#    (in H:/PREDICTIONS)
# ...and more things!
# (except otherwise noted, all outputs are in H:/pmt/stork_oracle_submission)




#### Code Piece No. 1 ####

# This first piece creates a csv file pmt_train_and_evaluation_samples_seed_1.csv containing 
# two binary indicator columns:
# 1. evaluation_set as defined in train.csv. We need an evaluation set for tuning the
#    classification threshold (as well as hyperparameters, but we have not used
#    hyperparameter tuning for this submission)
# 2. training_set, the complement of evaluation_set in train.csv
# 3. Columns with names starting train_sample_n_. These are nested samples of the training
#    set. We may train on a sample of the training set to save time
# The original version of the code was meant to be a piece of shared infrastructure
# between Stork Oracle (Emily & Tommy) and the rest of the Princeton Megateam, so
# this has not been integrated into the rest of the code

# Please run in command line:

Rscript pmt_train_and_eval_samples.R




#### Code Piece No. 2 ####

# This second piece does almost everything, including preprocessing, model training, and
# tuning. It takes three types of input through command line arguments:
# 1. Data files including train.csv, holdout_intermediate_leaderboard.csv, 
#    holdout_final_leaderboard.csv, GBAPERSOON2020TABV3.csv, and GBAHUISHOUDENS2020BUSV1.csv
# 2. The sampling file pmt_train_and_evaluation_samples_seed_1.csv
# 3. The codebook Codebook UPD.xlsx. This last input here is not very relevant for 
#    the purpose of this submission.
# This line creates four types of outputs
# 1. Holdout data files that have undergone most, but not all, of the preprocessing:
#    preprocessed_holdout_intermediate.RDS and preprocessed_holdout_final.RDS
#    These are similar to the training dataset just before it goes into the training pipeline.
# 2. An additional preprocessing recipe remove_zero_variance.RDS to be applied to the 
#    preprocessed holdout data in how to submit_R.Rmd. It removes any columns that have
#    zero variance in the training set. Unfortunately, in our code, not all of the
#    preprocessing can be done before the data goes into the training pipeline. Since
#    we are interested in how sample size affects prediction in addition to making a
#    a good predictive model, each pipeline is partly defined by a training set of a
#    particular size. This means that we would not know which columns have zero variance
#    in the training set until after the pipeline starts, even though we are using only
#    one pipeline for the purpose of this submission.
# 3. The model object
# 4. A results.csv file. For the purpose of this submission, the results.csv file 
#    is only relevant in so far as it contains a cell indicating what classification
#    threshold to use when making predictions in "how to submit_R.Rmd"

# Please run in command line

Rscript run_all.R "H:/DATASETS/train.csv" "H:/DATASETS/holdout_intermediate_leaderboard.csv" "H:/DATASETS/holdout_final_leaderboard.csv" "G:/Bevolking/GBAPERSOONTAB/2020/geconverteerde data/GBAPERSOON2020TABV3.csv" "G:/Bevolking/GBAHUISHOUDENSBUS/geconverteerde data/GBAHUISHOUDENS2020BUSV1.csv" "H:/DATASETS/documentation/Codebook UPD.xlsx" "H:/pmt/stork_oracle_submission/"

# run_all.R calls either directly or indirectly all of the other R files in this folder.
# It starts with jobfile.R, a configuration file specifying settings for parallelizaton,
# feature choices, train-test splits, modeling options, performance metrics, and results output

# If you are looking for scripts that do most of the preprocessing to produce 
# preprocessed_intermediate_data.RDS and preprocessed_holdout_data.RDS, see filtering_and_selecting.R,
# feature_engineering.R, create_metadata.R, and preprocessing.R. You will notice that
# the preprocessing process binds train.csv and holdout data together. We did this so
# that our code could handle any variables that have categories that are unseen in
# train.csv. We isolate training data from everything else before fitting the model
# in run_grid_row.R, so we believe that there is no actual data leakage.

# If you are looking for the code that produce remove_zero_variance.RDS, you may
# want to look at chunking.R and lines 1-69 of run_grid_row.R, especially the latter

# If you are looking for scripts that train, save, and tune the model, see chunking.R,
# run_grid_row.R, run_step.R, run_evaluation_set.R, run_bootstrap_sample.R,
# run_metric.R, run_metric_for_selecting_pipelines.R, and run_threshold.R.

# More info about the run_all code is available in comments to the code files.




#### Code Piece No. 3 ####

# This third piece of code takes 6 arguments
# 1. path_to_preprocessed_holdout_data = "preprocessed_holdout_intermediate.RDS"
#    or path_to_preprocessed_holdout_data = "preprocessed_holdout_final.RDS"
# 2. path_to_remove_zero_variance = "remove_zero_variance.RDS"
# 3. model_path = "model.cbm"
# 4. path_to_results = "results.csv"
# 5. team_name = "pmt"
# 6. submission_name = "stork_oracle_intermediate"
#    or submission_name = "stork_oracle_final"
# The code produces csv files containing 0/1 predictions:
# pmtstork_oracle_intermediate.csv and pmtstork_oracle_final.csv in H:/PREDICTIONS

# Please run the code in Rmd file:

# how to submit_R.Rmd