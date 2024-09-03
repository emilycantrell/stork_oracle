# On CBS server, this is file version xgboost_2024-08-27.R

library(data.table) # 1.15.4 on CBS server (for fread)
library(tidyverse) # 2.0.0 on CBS server
library(xgboost) # 1.7.7.1 on CBS server
library(MLmetrics) # 1.1.3 on CBS server (for F1_Score)
library(caret) # 6.0.94 on CBS server (for confusionMatrix)
library(pROC) # 1.18.5 on CBS server (for ROC curve)
library(tidymodels) # 1.2.0 on CBS server
library(readxl) # 1.4.3 on CBS server

# This file fits an xgboost model

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

# Identify how many ids per split
official_train_rinpersoon <- outcome_data$RINPERSOON
set.seed(111)
shuffled_ids <- sample(official_train_rinpersoon)
total_length <- length(shuffled_ids)
train_size <- floor(0.0080 * total_length)
validation_size <- floor(0.0010 * total_length)
test_size <- 0.0010 * total_length

# Split the IDS
stork_oracle_train_ids <- shuffled_ids[1:train_size]
stork_oracle_validation_ids <- shuffled_ids[(train_size + 1):(train_size + validation_size)]
stork_oracle_test_ids <- shuffled_ids[(train_size + validation_size + 1):(train_size + validation_size + test_size)]

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

# One-hot encode all factor variables
recipe <- recipe(features_stork_oracle_train) %>%
  step_dummy(all_factor(), one_hot = TRUE) %>%
  prep(features_stork_oracle_train)
features_stork_oracle_train <- bake(recipe, features_stork_oracle_train)
features_stork_oracle_validation <- bake(recipe, features_stork_oracle_validation)
features_stork_oracle_test <- bake(recipe, features_stork_oracle_test)

dmatrix_train <- xgb.DMatrix(data = as.matrix(features_stork_oracle_train), 
                             label = outcome_stork_oracle_train)
dmatrix_validation <- xgb.DMatrix(data = as.matrix(features_stork_oracle_validation), 
                                  label = outcome_stork_oracle_validation)
dmatrix_test <- xgb.DMatrix(data = as.matrix(features_stork_oracle_test), 
                            label = outcome_stork_oracle_test)

#### MODELING ####

# Create hyperparameter grid
# TODO: Adjust the contents of the grid to include other hyperparameters and values (current grid is arbitrary) 
grid <- expand_grid(
  eta = c(0.3), 
  max_depth = c(6),
  colsample_bynode = c(1)
) 

# Create data frame to hold tuning results
tuning_results <- grid

# Test all values in the grid
for(i in 1:nrow(grid)) {
  print(paste0("Training hyperparameter set ", i, " of ", nrow(grid)))
  # Extract the current set of hyperparameters
  params <- list(
    objective = "binary:logistic", 
    eta = grid$eta[i], 
    max_depth = grid$max_depth[i], 
    colsample_bynode = grid$colsample_bynode[i]
  )
  # Train the model
  set.seed(0)
  model <- xgb.train(
    params = params, 
    data = dmatrix_train,
    nrounds = 100,
    watchlist = list(validation = dmatrix_validation), 
    verbose = 1
  )
  # Save the result
  # TODO: check what best score means (is it based on the eval metric? what if we have multiple eval metrics?)
  tuning_results$score[i] <-  as_tibble(model$evaluation_log$validation_logloss)
  tuning_results$step[i] <- as_tibble(1:length(model$evaluation_log$validation_logloss))
}

# Select the best set of hyperparameters 
best_hyperparameters <- tuning_results %>%
  unnest(c(score, step)) %>%
  filter(score == min(score)) %>% # Be careful to change max/min as needed if metric is changed
  sample_n(1) # If there is a tie, select just one row

# Train the model with the best hyperparameters
best_params <- list(
  objective = "binary:logistic", 
  eta = best_hyperparameters$eta, 
  max_depth = best_hyperparameters$max_depth, 
  colsample_bynode = best_hyperparameters$colsample_bynode
)
set.seed(0)
best_model <- xgb.train(
  params = best_params, 
  data = dmatrix_train, 
  nrounds = 100,
  watchlist = list(validation = dmatrix_validation), 
  verbose = 1
)
best_model$step <- best_model$step <- best_hyperparameters$step

# Save model
model_path <- "H:/pmt/stork_oracle/4a_xgboost/untuned_xgboost_with_prefer_official_train_2024-08-26"
saveRDS(best_model, model_path)

# Predict on validation set
pred_probabilities <- predict(best_model, dmatrix_validation, iterationrange = c(1, best_model$step + 1))

# Examine distribution of predictions
probs_df <- as.data.frame(pred_probabilities)
probs_df %>%
  ggplot(aes(x=pred_probabilities)) + 
  geom_histogram()

# Determine the best threshold for classification based on F1 score
min_threshold_to_test <- min(pred_probabilities)
max_threshold_to_test <- max(pred_probabilities)
thresholds <- seq(min_threshold_to_test, max_threshold_to_test, by = c(0.01))
best_f1 <- 0 # default starting value
best_threshold <- 0.5 # default starting value
observed_outcomes <- getinfo(dmatrix_validation, "label")
for(threshold in thresholds) { 
  pred_labels <- ifelse(pred_probabilities > threshold, 1, 0)
  f1 <- F1_Score(y_pred = pred_labels, y_true = observed_outcomes, positive = "1")
  if(f1 > best_f1) { 
    best_f1 <- f1
    best_threshold <- threshold
  }
}
best_f1
best_threshold

# Evaluate on the Stork Oracle test set
pred_probabilities <- predict(best_model, dmatrix_test, iterationrange = c(1, best_model$step + 1))
observed_outcomes <- getinfo(dmatrix_test, "label")
pred_labels <- ifelse(pred_probabilities > best_threshold, 1, 0)
F1_Score(y_pred = pred_labels, y_true = observed_outcomes, positive = "1")
Precision(y_pred = pred_labels, y_true = observed_outcomes, positive = "1")
Recall(y_pred = pred_labels, y_true = observed_outcomes, positive = "1")
Accuracy(y_pred = pred_labels, y_true = observed_outcomes)
AUC(y_pred = pred_probabilities, y_true = observed_outcomes)


#### EXPLORATION OF RESULTS ####

# Examine observed labels vs. predicted labels
observed_vs_predicted <- cbind.data.frame(observed_outcomes, pred_probabilities, pred_labels)

# Compare predicted probabilities to observed outcomes
# Note: separationplot would be good for this, but we will need to request to have it installed in CBS environment
observed_vs_predicted %>%
  ggplot(aes(x = pred_probabilities, fill = as.factor(observed_outcomes))) + 
  geom_histogram(position = "identity") +
  labs(
    title = "Predicted probabilities vs. observed outcomes", 
    x = "Predicted probabilities", 
    y = "Count", 
    fill = "Observed outcomes" 
  ) + 
  theme_minimal()
 
# Compare predicted labels to observed labels: confusion matrix
confusionMatrix(as.factor(pred_labels), as.factor(observed_outcomes), positive = "1")

# Compare predicted labels to observed labels: plot
count_labels <- observed_vs_predicted %>%
  group_by(observed_outcomes, pred_labels) %>%
  count()
count_labels %>%
  ggplot(aes(x = as.factor(observed_outcomes), y = n, fill = as.factor(pred_labels))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs( 
    title = "Observed labels vs. predicted labels", 
    x = "Observed label", 
    y = "Count", 
    fill = "Predicted label"
  ) + 
  theme_minimal()

# What are the most important features? 
xgb.importance(model = best_model)

# What are the most important features? 
prefer_official_train_codebook_path <-"H:/DATASETS/documentation/Codebook UPD.xlsx"
prefer_official_train_codebook <- read_xlsx(prefer_official_train_codebook_path) %>%
  mutate(Source = sub("calculated based on ", "", Source),
         Source = sub("created based on ", "", Source)) %>%
  rename(Feature = Var_name)

feature_importances <- xgb.importance(model = best_model, trees = 0:(best_model$step - 1)) %>%
  mutate(Feature = case_when(
    grepl("SCONTRACTSOORT_main", Feature) ~ "SCONTRACTSOORT_main",
    grepl("GBABURGERLIJKESTAATNW", Feature) ~ "GBABURGERLIJKESTAATNW",
    T ~ sub("_X.*", "", Feature))) %>%
  left_join(prefer_official_train_codebook) %>%
  group_by(Source) %>%
  summarise(importance = sum(Gain))

importance_path <- "H:/pmt/stork_oracle/4a_xgboost/untuned_xgboost_with_prefer_official_train_importance_2024-08-26"
saveRDS(feature_importances, importance_path)


# ROC curve
roc_curve <- roc(observed_outcomes, pred_probabilities, levels = c("0", "1"))
plot(roc_curve)

# Notes: 
# TODO: Do we want to group categories that apply to fewer than X people?
# TODO: Calculate & explore other peformance metrics
# TODO: Try one-hot encoding rather than label encoding to see if it improves performance
# TODO: When we're ready for submission, train on the entire PreFer train set (including our Stork Oracle validation & test data)
