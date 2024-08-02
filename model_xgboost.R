# This file runs a simple model to make PreFer predictions with synthetic data

# We are starting with just data from Gbapersoontab: https://portal.odissei.nl/dataset.xhtml?persistentId=doi:10.57934/0b01e4108071ba40
# The variables are gender, birthyear of self and parents, and country-of-origin of self and parents. 

# Load package versions that were available on a given date ("groundhog day")
library(groundhog)
pkgs <- c("tidyverse", "xgboost", "caret")
groundhog.day <- "2023-12-31" # This date aligns with a module on the Snellius supercomputer: "R-bundle-CRAN-/2023.12-foss-2023a"
groundhog.library(pkgs, groundhog.day)

# Read in feature data
persoontab <- read_csv('~/Documents/GitHub/prefer_prepare/synth/data/raw/persoon_tab.csv', col_types = cols(.default = "c")) # read everything as character due to missingness codes
householdbus <- read_csv('~/Documents/GitHub/prefer_prepare/synth/data/raw/household_bus.csv', col_types = cols(.default = "c")) # read everything as character due to missingness codes

# Read in outcome file
outcome <- read_csv('~/Documents/GitHub/prefer_prepare/synth/data/raw/outcome.csv')

# Read in data splits
data_splits <- read_csv('~/Documents/GitHub/prefer_megateam_shared/data_splits/synthetic_rinpersoon_fold_assignments.csv')

#### DATA PREP & FEATURE ENGINEERING ####

# Function to change missingness symbols to NA
change_missingness_symbols_to_NA <- function(df) {
  df <- df %>%
    mutate_all(~ na_if(., "-")) %>%
    mutate_all(~ na_if(., "--"))
  }

persoontab <- change_missingness_symbols_to_NA(persoontab)
householdbus <- change_missingness_symbols_to_NA(householdbus)

# Create household ID for householdbus
householdbus <- householdbus %>%
  mutate(household_id = paste0(HOUSEKEEPING_NR, "_", DATE_STIRTHH)) %>%
  select(-HOUSEKEEPING_NR)

# Filter householdbus to only the most recent household per person
most_recent_household <- householdbus %>%
  group_by(rinpersoon) %>%
  arrange(desc(DATE_STIRTHH)) %>% 
  slice_head()

# Merge feature data files
features <- full_join(persoontab, 
                      most_recent_household, 
                      by = "rinpersoon")

# TODO: Utilize data from previous households in householdbus via feature engineering

#### DATA SPLITS ####

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
  
#### DMATRIX PREPARATION #### 

# Identify date columns (these are continuous but require special treatment)
date_column_names <- c("DATE_STIRTHH", "DATUMEINDEHH") # these are in date format (e.g., 2024-06-18)

# Identify continuous variables
year_month_day_column_names <- grep("jaar|year|maand|month|dag|day", colnames(features), ignore.case = TRUE, value = TRUE)
columns_with_number_in_name <- grep("aantal|number", colnames(features), ignore.case = TRUE, value = TRUE)
other_numeric_columns <- c("BIRTHEDYOUNGCHILDHH", "GBAGENERATIE") 
continuous_variables <- c(year_month_day_column_names, columns_with_number_in_name, other_numeric_columns)

# TODO: Check with codebook/Mark about meaning of GBAGENERATIE. I think this means immigrant generation (i.e., first-gen, second-gen)

# All variables that we do not identify as continuous will be considered categorical (this includes binary)
categorical_variables <- features_train %>% 
  select(-all_of(c("rinpersoon", "household_id", date_column_names, continuous_variables))) %>% 
  names()

# Create function to change data type
change_data_type <- function(feature_data) { 
  feature_data <- feature_data %>%
    mutate(across(all_of(continuous_variables), as.numeric)) %>%
    # Convert strings that represent dates to date-type, then numeric (cannot go straight to numeric)
    mutate(across(all_of(date_column_names), ~ as.numeric(as.Date(.))))%>%
    # Convert categorical variables to factor, then numeric (cannot go straight to numeric)
    mutate(across(all_of(categorical_variables), ~ as.numeric(as.factor(.))))
  }

# Create function that generates dmatrix
generate_dmatrix <- function(feature_data, outcome) { 
  cleaned_feature_data <- change_data_type(feature_data)
  # Merge feature data with outcome data to ensure rows are in the same order
  features_and_outcome <- full_join(cleaned_feature_data, outcome, by = join_by(rinpersoon))
  # Remove outcome and id number from feature matrix
  features_for_dmatrix <- features_and_outcome %>%
    select(-all_of(c("rinpersoon", "household_id", "outcome")))
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

model_to_tune <- boost_tree(
  mode = "classification",
  mtry = tune(), trees = tune(), tree_depth = tune(), learn_rate = tune()
) %>%
  set_engine("xgboost", counts = FALSE)
folds <- vfold_cv(model_df, v = 5)
grid <- expand.grid(
  mtry = c(.05, .1, .15, .2, .25, .3, .35, .4),
  trees = c(10, 50, 100, 300, 600, 900, 1200),
  tree_depth = 3:7,
  learn_rate = c(.1, .3, .5, .7, .9, 1.1)
)
best <- tune_grid(model_to_tune, recipe, folds,
                  grid = grid,
                  metrics =
                    metric_set(metric_tweak("f_meas", f_meas, event_level = "second"))
) %>%
  collect_metrics() %>%
  filter(n == 5) %>%
  arrange(desc(mean)) %>%
  head(1)
model <- boost_tree(
  mode = "classification",
  mtry = best$mtry,
  trees = best$trees,
  tree_depth = best$tree_depth,
  learn_rate = best$learn_rate
) %>%
  set_engine("xgboost", counts = FALSE)



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

