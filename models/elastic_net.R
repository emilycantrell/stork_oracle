# On CBS server, this is file version elastic_net_2024-08-28.R

library(data.table) # 1.15.4 on CBS server (for fread)
library(tidyverse) # 2.0.0 on CBS server
library(glmnet) # 4.1.8 on CBS server
library(MLmetrics) # 1.1.3 on CBS server (for F1_Score and other metrics)
library(caret) # 6.0.94 on CBS server (for confusionMatrix)
library(tidymodels) # 1.2.0 on CBS server
library(readxl) # 1.4.3 on CBS server

# This file fits an elastic net model

#### READ IN DATA ####

# Load outcome data 
outcome_data_path <- "H:/pmt/stork_oracle/1_filter_to_train_and_eval_set/prefer_official_train_outcomes.csv"
outcome_data <- fread(outcome_data_path, colClasses = c(RINPERSOON = "character"))

# Load official_prefer_train and address missingness (this file is already filtered to just the official PreFer train set cases)
prefer_official_train_path <- "H:/pmt/stork_oracle/1_filter_to_train_and_eval_set/prefer_official_train_prefer_official_train.csv"
prefer_official_train <- fread(prefer_official_train_path, 
                    colClasses = c(RINPERSOON = "character"))
classes <- map_chr(prefer_official_train, class)
integer64 <- names(classes)[which(classes == "integer64")]
prefer_official_train <- mutate(prefer_official_train,
         across(all_of(integer64), as.numeric),
         INPBELI = ifelse(INPBELI == 9999999999, NA, INPBELI),
         INPP100PBRUT = ifelse(INPP100PBRUT < 0, NA, INPP100PBRUT),
         INPP100PPERS = ifelse(INPP100PPERS < 0, NA, INPP100PPERS),
         INPPERSPRIM = ifelse(INPPERSPRIM == 9999999999, NA, INPPERSPRIM),
         INHAHL = ifelse(INHAHL == 99, NA, INHAHL),
         INHAHLMI = ifelse(INHAHLMI == 99, NA, INHAHLMI),
         INHARMEUR = ifelse(INHARMEUR < 0, NA, INHARMEUR),
         INHARMEURL = ifelse(INHARMEURL < 0, NA, INHARMEURL),
         INHBRUTINKH = ifelse(INHBRUTINKH == 9999999999, NA, INHBRUTINKH),
         INHUAF = ifelse(INHUAF < 0, NA, INHUAF),
         INHUAFL = ifelse(INHUAFL < 0, NA, INHUAFL),
         VEHP100WELVAART = ifelse(VEHP100WELVAART < 0, NA, VEHP100WELVAART),
         VEHP100HVERM = ifelse(VEHP100HVERM < 0, NA, VEHP100HVERM),
         VEHW1000VERH = ifelse(VEHW1000VERH == 99999999999, NA, VEHW1000VERH),
         VEHW1100BEZH = ifelse(VEHW1100BEZH == 99999999999, NA, VEHW1100BEZH),
         VEHW1110FINH = ifelse(VEHW1110FINH == 99999999999, NA, VEHW1110FINH),
         VEHW1120ONRH = ifelse(VEHW1120ONRH == 99999999999, NA, VEHW1120ONRH),
         VEHW1130ONDH = ifelse(VEHW1130ONDH == 99999999999, NA, VEHW1130ONDH),
         VEHW1140ABEH = ifelse(VEHW1140ABEH == 99999999999, NA, VEHW1140ABEH),
         VEHW1150OVEH = ifelse(VEHW1150OVEH == 99999999999, NA, VEHW1150OVEH),
         VEHW1200STOH = ifelse(VEHW1200STOH == 99999999999, NA, VEHW1200STOH),
         VEHW1210SHYH = ifelse(VEHW1210SHYH == 99999999999, NA, VEHW1210SHYH),
         VEHW1220SSTH = ifelse(VEHW1220SSTH == 99999999999, NA, VEHW1220SSTH),
         VEHW1230SOVH = ifelse(VEHW1230SOVH == 99999999999, NA, VEHW1230SOVH),
         VEHWVEREXEWH = ifelse(VEHWVEREXEWH == 99999999999, NA, VEHWVEREXEWH)
  )

# Load persoontab (this file is already filtered to just the official PreFer train set cases)
persoon_path <- "H:/pmt/stork_oracle/1_filter_to_train_and_eval_set/prefer_official_train_GBAPERSOON2020TABV3.csv"
persoontab <- fread(persoon_path, 
                    colClasses = c(RINPERSOON = "character")) 

# Load hhbus_features (this file is already filtered to just the official PreFer train set cases)
hhbus_features_path <- "H:/pmt/stork_oracle/2_feature_engineering/features_from_householdbus2020.csv"
hhbus_features <- fread(hhbus_features_path, colClasses = c(RINPERSOON = "character"))

# If we want just sex and age, set this value to TRUE 
# TODO: Set up a more generalizable way to choose specific sets of features
test_just_sex_and_age <- FALSE
if(test_just_sex_and_age == TRUE) { 
  persoontab <- persoontab %>%
    select(RINPERSOON, GBAGESLACHT, GBAGEBOORTEJAAR)
}

data <- prefer_official_train

# data <- persoontab
# Merge everything
# data <- merge(persoontab, hhbus_features, by = "RINPERSOON", all = TRUE)

# Use metadata to identify variables types 
metadata <- fread("H:/pmt/stork_oracle/3_create_metadata/variable_type_metadata.csv")
continuous_variables <- metadata %>%
  filter(variable_type == "continuous") %>%
  pull(variable_name)
categorical_variables <- metadata %>%
  filter(variable_type == "categorical") %>%
  pull(variable_name)

# Continuous missing data is encoded as NA.
# Categorical missing data is encoded as "NA"
data <- mutate(data,
               across(any_of(continuous_variables), ~ifelse(.x %in% c("-", "--", "---", "----", "0000"), NA, .x)),
               across(any_of(categorical_variables), ~ifelse(is.na(.x), "X", .x)))

#### TEMPORARY: RANDOMLY ASSIGN SPLITS ####
# This section will change once the data splitting process has been finalized

samples_path <- "H:/pmt/eval/train_and_eval_samples/pmt_train_and_evaluation_samples_seed_1.csv"
samples_data <- fread(samples_path, colClasses = c(RINPERSOON = "character"))

# Split the IDS
stork_oracle_train_ids <- samples_data$RINPERSOON[samples_data$evaluation_set == 0]
stork_oracle_validation_ids <- samples_data$RINPERSOON[samples_data$evaluation_set == 1]
stork_oracle_test_ids <- samples_data$RINPERSOON[samples_data$evaluation_set == 1]

# Apply splits to data
features_stork_oracle_train <- data %>%
  filter(RINPERSOON %in% stork_oracle_train_ids)
features_stork_oracle_validation <- data %>%
  filter(RINPERSOON %in% stork_oracle_validation_ids)
features_stork_oracle_test <- data %>%
  filter(RINPERSOON %in% stork_oracle_test_ids)

# This function gets outcomes for a subset of the data
get_outcome <- function(df) {
  left_join(df, outcome_data, by = "RINPERSOON") %>%
    pull(outcome)
}
outcome_stork_oracle_train <- get_outcome(features_stork_oracle_train)
outcome_stork_oracle_validation <- get_outcome(features_stork_oracle_validation)
outcome_stork_oracle_test <- get_outcome(features_stork_oracle_test)


#### DATA PREPARATION ####

# Function to change data type 
# Note: in the xgboost file, we make everything numeric, whereas for catboost, we make numeric & factor types
change_data_type <- function(feature_data) { 
  feature_data <- feature_data %>%
    mutate(across(any_of(continuous_variables), as.numeric)) %>%
    mutate(across(any_of(categorical_variables), as.factor)) %>%
    select(-RINPERSOON)
  return(feature_data)
}
features_stork_oracle_train <- change_data_type(features_stork_oracle_train)
features_stork_oracle_validation <- change_data_type(features_stork_oracle_validation)
features_stork_oracle_test <- change_data_type(features_stork_oracle_test)

# Mean_impute all continuous variables
imputation <- recipe(features_stork_oracle_train) %>%
  step_impute_mean(all_numeric()) %>%
  prep(features_stork_oracle_train)
features_stork_oracle_train <- bake(imputation, features_stork_oracle_train)
features_stork_oracle_validation <- bake(imputation, features_stork_oracle_validation)
features_stork_oracle_test <- bake(imputation, features_stork_oracle_test)

rm(data)
rm(features_stork_oracle_test)
rm(features_stork_oracle_validation)
rm(metadata)
rm(outcome_data)
rm(prefer_official_train)
rm(samples_data)
rm(outcome_stork_oracle_test)
rm(outcome_stork_oracle_validation)
rm(stork_oracle_test_ids)
rm(stork_oracle_train_ids)
rm(stork_oracle_validation_ids)
gc()

dummyVars(~., features_stork_oracle_train, sep = "__", fullRank = TRUE, sparse = TRUE)

features_stork_oracle_train <- sparse.model.matrix(~. , features_stork_oracle_train, sep = "__")
features_stork_oracle_validation <- sparse.model.matrix(~. , features_stork_oracle_validation, sep = "__")
features_stork_oracle_validation1 <- features_stork_oracle_validation[, c("GBAGEBOORTELAND__5002", "GBAGEBOORTELAND__5003")]
features_stork_oracle_validation1[, colnames(features_stork_oracle_train)[which(!colnames(features_stork_oracle_train) %in% colnames(features_stork_oracle_validation1))]] <- 0
# features_stork_oracle_train1 <- sparse.model.matrix(~. , features_stork_oracle_train, contrasts.arg = lapply(select(features_stork_oracle_train, all_of(colnames(features_stork_oracle_train)[sapply(features_stork_oracle_train, is.factor)])), contrasts, contrasts = FALSE))


features_stork_oracle_train1 <- features_stork_oracle_train %>%
  select(any_of(categorical_variables)) %>%
  mutate(ID = 1:nrow(features_stork_oracle_train)) %>%
  melt("ID")
features_stork_oracle_train1 <- dcast(features_stork_oracle_train1,ID ~ variable + value, sep = "_X")



  step_scale(everything()) %>%
  step_dummy(all_factor()) %>%
  


#### MODELING ####

# Create hyperparameter grid
# TODO: Adjust the contents of the grid to include other hyperparameters and values (current grid is arbitrary) 
grid <- expand_grid(
  alpha = c(1),
  lambda = NULL
) 

# Create data frame to hold tuning results
tuning_results <- grid

# Test all values in the grid
for(i in 1:nrow(grid)) {
  print(paste0("Training hyperparameter set ", i, " of ", nrow(grid)))
  # Extract the current set of hyperparameters
  # Train the model
  model <- glmnet(features_stork_oracle_train, outcome_stork_oracle_train,
                  family = "binomial",
                  alpha = grid$alpha[i],
                  lambda = grid$lambda[i])
  # Get the score on validation set from the best iteration
  # TODO: Consider using AUC or another metric for evaluation
  evaluation_scores <- assess.glmnet(model,
                                     as.matrix(features_stork_oracle_validation),
                                     outcome_stork_oracle_validation)
  tuning_results$score[i] <- as_tibble(evaluation_scores$deviance)
  tuning_results$step[i] <- as_tibble(1:length(evaluation_scores$deviance))
}

# Select the best set of hyperparameters 
best_hyperparameters <- tuning_results %>%
  unnest(c(score, step)) %>%
  filter(score == min(score)) %>% # Be careful to change max/min as needed if metric is changed
  sample_n(1) # If there is a tie, select just one row

# Train the model with the best hyperparameters
best_model <- glmnet(features_stork_oracle_train, outcome_stork_oracle_train,
                     family = "binomial",
                     alpha = best_hyperparameters$alpha,
                     lambda = best_hyperparameters$lambda)
best_model$step <- best_hyperparameters$step


# Save model
model_path <- "H:/pmt/stork_oracle/4c_elastic_net/untuned_lasso_with_prefer_official_train_2024-08-26"
saveRDS(best_model, model_path)

# Predict on validation set, so that we can use validation set to determine threshold
# Note: alternatively, we could have saved all models from the tuning process. I'm not sure whether it's better computationally to save all models or to re-run the winner.
# TODO: Test prediction_type = "Class"
pred_probabilities_validation <-predict(best_model, as.matrix(features_stork_oracle_validation), s = best_model$lambda[best_model$step], type = "response")


# Examine distribution of predictions
probs_df <- as.data.frame(pred_probabilities_validation)
probs_df %>%
  ggplot(aes(x=pred_probabilities_validation)) + 
  geom_histogram()

# Determine the best threshold for classification based on F1 score
min_threshold_to_test <- min(pred_probabilities_validation)
max_threshold_to_test <- max(pred_probabilities_validation - 0.01) # -0.01 because we need to predict at least one positive case to ensure F1 isn't NaN 
thresholds <- seq(min_threshold_to_test, max_threshold_to_test, by = c(0.01))
best_f1 <- 0 # default starting value
best_threshold <- 0.5 # default starting value
for(threshold in thresholds) { 
  pred_labels_validation <- ifelse(pred_probabilities_validation > threshold, 1, 0)
  f1 <- F1_Score(y_pred = pred_labels_validation, y_true = outcome_stork_oracle_validation, positive = "1")
  if(is.na(f1)) {
    break
  }
  if(f1 > best_f1) { 
    best_f1 <- f1
    best_threshold <- threshold
  }
}
best_f1
best_threshold

# Evaluate on the Stork Oracle test set
pred_probabilities_test <-predict(best_model, as.matrix(features_stork_oracle_test), s = best_model$lambda[best_model$step], type = "response")
pred_labels_test <- ifelse(pred_probabilities_test > best_threshold, 1, 0)
F1_Score(y_pred = pred_labels_test, y_true = outcome_stork_oracle_test, positive = "1")
Precision(y_pred = pred_labels_test, y_true = outcome_stork_oracle_test, positive = "1")
Recall(y_pred = pred_labels_test, y_true = outcome_stork_oracle_test, positive = "1")
Accuracy(y_pred = pred_labels_test, y_true = outcome_stork_oracle_test)
AUC(y_pred = pred_probabilities_test, y_true = outcome_stork_oracle_test)


# What happens if threshold is 0.5? 
best_threshold <- 0.5
pred_probabilities_test <- catboost.predict(best_model, test_pool, prediction_type = "Probability")
observed_outcomes_test <- left_join(features_stork_oracle_test, outcome_data, by = "RINPERSOON") %>%
  pull(outcome)
pred_labels_test <- ifelse(pred_probabilities_test > best_threshold, 1, 0)
F1_Score(y_pred = pred_labels_test, y_true = observed_outcomes_test, positive = "1")
Precision(y_pred = pred_labels_test, y_true = observed_outcomes_test, positive = "1")
Recall(y_pred = pred_labels_test, y_true = observed_outcomes_test, positive = "1")
Accuracy(y_pred = pred_labels_test, y_true = observed_outcomes_test)
AUC(y_pred = pred_probabilities_test, y_true = observed_outcomes_test)

# What happens if we predict the majority class as the prediction for everyone? 
pred_probabilities_test <- rep(0, length(pred_probabilities_test))
observed_outcomes_test <- left_join(features_stork_oracle_test, outcome_data, by = "RINPERSOON") %>%
  pull(outcome)
pred_labels_test <- rep(0, length(pred_probabilities_test))
F1_Score(y_pred = pred_labels_test, y_true = observed_outcomes_test, positive = "1")
Precision(y_pred = pred_labels_test, y_true = observed_outcomes_test, positive = "1")
Recall(y_pred = pred_labels_test, y_true = observed_outcomes_test, positive = "1")
Accuracy(y_pred = pred_labels_test, y_true = observed_outcomes_test)
AUC(y_pred = pred_probabilities_test, y_true = observed_outcomes_test)

# What happens if we choose the threshold to maximize accuracy? 

# Determine the best threshold for classification based on ACCURACY
observed_outcomes_validation <- left_join(features_stork_oracle_validation, outcome_data, by = "RINPERSOON") %>%
  pull(outcome)
min_threshold_to_test <- min(pred_probabilities_validation)
max_threshold_to_test <- max(pred_probabilities_validation - 0.01) # -0.01 because we need to predict at least one positive case to ensure F1 isn't NaN 
thresholds <- seq(min_threshold_to_test, max_threshold_to_test, by = c(0.01))
best_accuracy <- 0 # default starting value
best_threshold <- 0.5 # default starting value
for(threshold in thresholds) { 
  pred_labels_validation <- ifelse(pred_probabilities_validation > threshold, 1, 0)
  accuracy <- Accuracy(y_pred = pred_labels_validation, y_true = observed_outcomes_validation)
  if(accuracy > best_accuracy) { 
    best_accuracy <- accuracy
    best_threshold <- threshold
  }
}
best_accuracy
best_threshold

# Evaluate on the Stork Oracle test set
pred_probabilities_test <- catboost.predict(best_model, test_pool, prediction_type = "Probability")
observed_outcomes_test <- left_join(features_stork_oracle_test, outcome_data, by = "RINPERSOON") %>%
  pull(outcome)
pred_labels_test <- ifelse(pred_probabilities_test > best_threshold, 1, 0)
F1_Score(y_pred = pred_labels_test, y_true = observed_outcomes_test, positive = "1")
Precision(y_pred = pred_labels_test, y_true = observed_outcomes_test, positive = "1")
Recall(y_pred = pred_labels_test, y_true = observed_outcomes_test, positive = "1")
Accuracy(y_pred = pred_labels_test, y_true = observed_outcomes_test)
AUC(y_pred = pred_probabilities_test, y_true = observed_outcomes_test)





#### EXPLORATION OF RESULTS ####

# Examine observed labels vs. predicted labels
observed_vs_predicted <- cbind.data.frame(observed_outcomes_test, pred_probabilities_test, pred_labels_test)

# Compare predicted probabilities to observed outcomes
# Note: separationplot would be good for this, but we will need to request to have it installed in CBS environment
observed_vs_predicted %>%
  ggplot(aes(x = pred_probabilities_test, fill = as.factor(observed_outcomes_test))) + 
  geom_histogram(position = "identity") +
  labs(
    title = "Predicted probabilities vs. observed outcomes", 
    x = "Predicted probabilities", 
    y = "Count", 
    fill = "Observed outcomes" 
  ) + 
  theme_minimal()
 
# Compare predicted labels to observed labels: confusion matrix
confusionMatrix(as.factor(pred_labels_test), as.factor(observed_outcomes_test), positive = "1")

# Compare predicted labels to observed labels: plot
count_labels <- observed_vs_predicted %>%
  group_by(observed_outcomes_test, pred_labels_test) %>%
  count()
count_labels %>%
  ggplot(aes(x = as.factor(observed_outcomes_test), y = n, fill = as.factor(pred_labels_test))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs( 
    title = "Observed labels vs. predicted labels", 
    x = "Observed label", 
    y = "Count", 
    fill = "Predicted label"
  ) + 
  theme_minimal()

# What are the most important features? 
prefer_official_train_codebook_path <-"H:/DATASETS/documentation/Codebook UPD.xlsx"
prefer_official_train_codebook <- read_xlsx(prefer_official_train_codebook_path) %>%
  mutate(Source = sub("calculated based on ", "", Source),
         Source = sub("created based on ", "", Source)) %>%
  rename(feature = Var_name)

feature_importances <- varImp(best_model, lambda = best_model$lambda[best_model$step]) %>%
  as_tibble() %>%
  mutate(feature = case_when(
    grepl("SCONTRACTSOORT_main", best_model[["beta"]]@Dimnames[[1]]) ~ "SCONTRACTSOORT_main",
    grepl("GBABURGERLIJKESTAATNW", best_model[["beta"]]@Dimnames[[1]]) ~ "GBABURGERLIJKESTAATNW",
    T ~ sub("_X.*", "", best_model[["beta"]]@Dimnames[[1]]))) %>%
  left_join(prefer_official_train_codebook) %>%
  group_by(Source) %>%
  summarise(importance = sum(Overall))

importance_path <- "H:/pmt/stork_oracle/4c_elastic_net/untuned_lasso_with_prefer_official_train_importance_2024-08-26"
saveRDS(feature_importances, importance_path)

# Notes:
# TODO: Starter pack
# TODO: Towards a single results file
# TODO: Add more hyperparameters & values to the grid (we can randomly sample rather than trying all combos; is there something like HalvingGridSearch for R?)
# TODO: Elastic Net
# TODO: Household transition Time
# TODO: How many households?
# TODO: When we're ready for submission, train on the entire PreFer train set (including our Stork Oracle validation & test data)
# TODO: set seeds where needed
# TODO: remove useless features
# TODO: remove redundant features
# TODO: Calculate & explore other peformance metrics
# TODO: Try one-hot encoding rather than label encoding to see if it improves performance
# TODO: Do we want to group categories that apply to fewer than X people? 





