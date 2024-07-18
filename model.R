# This file runs a simple model to make PreFer predictions with synthetic data

# We are starting with just data from Gbapersoontab: https://portal.odissei.nl/dataset.xhtml?persistentId=doi:10.57934/0b01e4108071ba40
# The variables are gender, birthyear of self and parents, and country-of-origin of self and parents. 

# Load package versions that were available on a given date ("groundhog day")
library(groundhog)
pkgs <- c("tidyverse", "xgboost", "caret")
groundhog.day <- "2024-07-01"
groundhog.library(pkgs, groundhog.day)

# Read in feature data
persoontab <- read_csv('~/Documents/GitHub/prefer_prepare/synth/data/raw/persoon_tab.csv', col_types = cols(.default = "c")) # read everything as character due to missingness codes
features <- persoontab # later, "features" will contain persoontab data merged with other feature data

# Read in outcome file
outcome <- read_csv('~/Documents/GitHub/prefer_prepare/synth/data/raw/outcome.csv')

# Read in data splits
data_splits <- read_csv('~/Documents/GitHub/prefer_megateam_shared/data_splits/synthetic_rinpersoon_fold_assignments.csv')

#### APPLY DATA SPLITS ####

# Specify which folds to use (we will use the same folds as the books-of-life team when comparing results)
train_folds <- c(0:3)
test_folds <- c(4)

train_rinpersoons <- data_splits %>%
  filter(fold %in% train_folds) %>%
  pull(rinpersoon)

features_train <- features %>%
  filter(rinpersoon %in% train_rinpersoons)

outcome_train <- outcome %>%
  filter(rinpersoon %in% train_rinpersoons)

test_rinpersoons <- data_splits %>%
  filter(fold %in% test_folds) %>%
  pull(rinpersoon)

features_test <- features %>%
  filter(rinpersoon %in% test_rinpersoons)

outcome_test <- outcome %>%
  filter(rinpersoon %in% test_rinpersoons)
  
#### DATA PREPARATION #### 

### Determine which variables are continuous vs. categorical ###

# Identify date columns (these are continuous)
date_column_names <- grep("jaar|year|maand|month|dag|day", colnames(features), ignore.case = TRUE, value = TRUE)

# Identify continuous variables
# When we add more feature files, we will add other types of columns that should be continuous
continuous_variables <- date_column_names

# All variables that we do not identify as continuous will be considered categorical (this includes binary)
categorical_variables <- features_train %>% 
  select(-all_of(c("rinpersoon", continuous_variables))) %>% 
  names()

### Create functions for data prep ###

# Create data cleaning function
data_cleaning <- function(feature_data) { 
  # Change missingness symbol to NA
  feature_data <- feature_data %>%
    mutate_all(~ na_if(., "-"))
  # Change variables to the correct data type
  feature_data <- feature_data %>%
    mutate(across(all_of(continuous_variables), as.numeric)) %>%
    # Convert categoricals variables to factor, then numeric (cannot go straight to numeric)
    mutate(across(all_of(categorical_variables), ~ as.numeric(as.factor(.))))
  }

# Create function that generates dmatrix
generate_dmatrix <- function(feature_data, outcome) { 
  cleaned_feature_data <- data_cleaning(feature_data)
  # Merge feature data with outcome data to ensure rows are in the same order
  features_and_outcome <- full_join(cleaned_feature_data, outcome, by = join_by(rinpersoon))
  # Remove outcome and id number from feature matrix
  features_for_dmatrix <- features_and_outcome %>%
    select(-all_of(c("rinpersoon", "outcome")))
  outcome_for_dmatrix <- features_and_outcome %>%
    pull(outcome)
  # Generate dmatrix
  dmatrix <- xgb.DMatrix(data = as.matrix(features_for_dmatrix),
                         label = outcome_for_dmatrix)
  return(dmatrix)
  }

#### MODELING ####

# Basic XGBoost model
dmatrix_train <- generate_dmatrix(features_train, outcome_train)
our_model <- xgboost(data = dmatrix_train, nrounds = 100, objective = "binary:logistic")

# TODO: Add hyperparameter tuning

# Generate predictions
dmatrix_test <- generate_dmatrix(features_test, outcome_test)
predictions <- predict(our_model, dmatrix_test) 

# Calculate R^2 holdout
observed_outcomes <- getinfo(dmatrix_test, "label")
tse_model <- (observed_outcomes-predictions)^2
tse_baseline <- (observed_outcomes-mean(outcome_train$outcome))^2
r_squared_holdout <- 1-(sum(tse_model)/sum(tse_baseline))
r_squared_holdout

# TODO: Examine other metrics
