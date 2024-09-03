# Read in arguments from the jobfile
source("jobfile") 

# Filter to the official train and eval set (to be able to do this, Malte/Flavio will need train.csv & other raw files)
# Open question: are the raw files like householdbus on OSSC already? If so, how do file paths differ from CBS environment? 
source("filter_to_prefer_train_and_eval_set.R")

# Conduct feature engineering
source("feature_engineering.R")

# Create metadata
source("create_metadata.R")

# Preprocessing 
source("preprocessing.R")

# define grids
source("hyperparameter_grids.R")

#--------------------------------

source("run_selection_metric.R")
# This file first creates a helper function to be used later: get_threshold()
# This function takes a test_metric (a name) and a set of predicted  
# probabilities for individuals with IDs and finds the best threshold for 
# that metric. It then appends the name of the metric to 
# the left of the threshold

# This file creates run_selection_metric(). It takes a selection_metric 
# (a name) and finds the model specifications that produced the best 
# selection_metric for the selection_set. It then fits the model with the best 
# number of steps and makes predictions for the 
# training_set_selection_set_specific_evaluation set, which includes the 
# selection set itself and any test set corresponding to this training-set-
# selection-set combination.

# It then applies get_threshold() to each test_metric, using the selection set 
# predictions just generated, to get a dataframe with  two columns, one for the 
# name of the test_metric and one for the threshold.
# Let's call this dataframe "thresholds".

# Finally, run_selection_metrics() augments partial results by applying
# run_evaluation_set(mode = "test") to each test_set. It takes the results
# and then append selection_metric as a column

# If save_only_winning_hyperparameter_draw_results == TRUE
# delete all other rows for this model


source("run_selection_set.R")
# This file creates run_selection_set(). It takes a selection_set (a name) and 
# augments partial results by applying run_selection_metric() to each of the 
# selection_metrics() It takes the results and then append selection_set as
# a column

#-------------------------------
# Go back to read run_models.R again before reading about run_selection_set.R!

source("run_metric.R")

# This file creates LogLoss(), MSE(), R2_Score(), AUC(), F1_Score(), and
# Accuracy(). Each of them takes a set of predictions and generates partial
# results in the form of a single score. It then appends the name of the metric
# to the left of the score

# For F1_Score() and Accuracy(), there would be an argument called "mode." If 
# mode == "all_pipelines", which is the default, the function uses 50% as the threhold
# and appends "_with_default_threhold" to the metric name.
# If mode = "winning_pipelines", the function finds the threshold by looking for the 
# threshold associated with the metrics in the dataframe threshold. The metric
# name appended to the score would be concatenated with "_with_tuned_threshold"

# This file also creates run_metric(). It takes a metric (a name) and generates
# partial results by running the metrics function with that name. If the
# argument "mode" equals "winning_pipelines", as opposed to "all_pipelines" by default for this 
# function, run the named function with mode = "winning_pipelines"

source(run_bootstrap_sample.R)
# This file creates run_bootstrap_sample(). It takes a bootstrap sample unless
# it's sample #1, where it takes the dataset as given. It also takes a vector 
# of metrics (metrics_for_all_pipelines if mode == "all_pipelines" and metrics_for_winning_pipelines if
# mode == "winning_pipelines") in the form of their names from the jobfile and generates partial 
# results by applying run_metric() to each metric.

# If mode == "test", we give run_metric() an additonal argument mode == "test"

source(run_evaluation_set.R)

# This file creates run_evaluation_set(). It takes an evaluation set. It then
# generates partial results by applying run_bootstrap_sample(mode = mode) to each
# bootstrap sample, currently only in the form of a number going from 1 to
# n_bootstraps() + 1. It takes the partial results, group it by metrics, and
# summarizes each group to get the first entry, the 2.5% percentile, and the 
# 97.5% percentile. It then pivots the data frame wider into something with 
# only one row. It then adds a column for the evaluation set. 

source(run_step.R)
# This file creates run_step_catboost(), run_step_xgboost(), and 
# run_step_elastic_net(). Each of them takes a step, makes predictions for 
# the training_set-specific evaluation set as defined in run_training_set.R. It
# then generates partial results by applying 
# run_evaluation_sets(mode = "evaluation") to each of of the evaluation set 
# names involved in generating the training_set-specific evaluation set. It 
# takes the partial results dataframe and adds a column for the step.

source(run_grid_row.R)

# This file creates run_grid_row_catboost(), run_grid_row_xgboost(), and 
# run_grid_row_elastic_net(). Each of them takes a grid_row, fits a model 
# object according to the grid_row specifications, and then checks how many 
# "steps" (trees or lambdas) are involved in the object. Each of them then  
# generates partial results by applying run_step_MODEL_NAME() to each step 
# in the steps vector as defined in the jobfile. It takes the partial results 
# dataframe and adds a column for the grid_row

source(run_models.R)
# This file creates catboost(), xgboost(), and elastic_net(). Each of them
# takes a training set and conducts model-specific preprocessing. Each of them
# then gets an expanded hyperparameter grid from the jobfile, aka grid_rows, 
# and generates partial results by applying run_grid_row_MODEL_NAME() to each 
# grid_row. It takes the partial results dataframe and adds a column for the
# model

# This file also creates run_model(). It takes a model (a name) and generates
# partial results by running the model function with that name. It then 
# augments the partial results by applying run_selection_set() to each
# selection_set involved in the partial data. We merge the aggregated output of
# run_selection_set and join it with the aggregated output of run_model().

source(run_training_set.R)
# This file creates run_training_set(). It takes a training_set (a name) and 
# filters the data according to the the column in the sampling file with the 
# same name. It also creates a training_set-specific evaluation set by taking
# the intersection of all associated selection_sets and test_sets from the
# jobfile. It then gets the models in the form of a vector of model names
# from the jobfile and generates partial results by applying run_model() to 
# each model. It takes the partial results dataframe and adds a column for the
# training_set.

source(run_sampling_file.R)
# This file creates run_sampling_file(). It takes a sampling_file path and
# reads it. It also gets the training_sets in the form of a vector of 
# training_set names from the jobfile and generates partial results
# by applying run_training_set() to each training_set. It takes the partial
# results dataframe and adds a column for the sampling_file.

source(run_feature_set.R)
# This file creates run_feature_set(). It takes a feature_set, selects the data
# according to the feature_set, and generates partial results by applying
# run_sampling_file() to each sampling_file path from the jobfile. It takes the
# partial results dataframe and adds a column for feature_set.

# Generate results by applying run_feature_set() to each feature_set.
results <- map_dfr(feature_sets, run_feature_set) 

if (save_only_winning_hyperparameter_draw_results) {
  results <- filter(results, !is.na(F1_Score_with_tuned_threshold))
}

# Save results
fwrite(results[[1]], results_path)