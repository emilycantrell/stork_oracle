# This file runs a simple model to make PreFer predictions with synthetic data

# Install catboost by running the following lines:
# install.packages('remotes')
# remotes::install_url('https://github.com/catboost/catboost/releases/download/v1.2.5/catboost-R-darwin-universal2-1.2.5.tgz', INSTALL_opts = c("--no-multiarch", "--no-test-load"))

# Load package versions that were available on a given date ("groundhog day")
library(groundhog)
pkgs <- c("tidyverse", "caret")
groundhog.day <- "2024-07-01"
groundhog.library(pkgs, groundhog.day)
library(catboost) # Catboost cannot be loaded with groundhog because it is not in the CRAN library; please follow installation instructions above

# Read in feature data
persoontab <- read_csv('~/Documents/GitHub/prefer_prepare/synth/data/raw/persoon_tab.csv', col_types = cols(.default = "c")) # read everything as character due to missingness codes
householdbus <- read_csv('~/Documents/GitHub/prefer_prepare/synth/data/raw/household_bus.csv', col_types = cols(.default = "c")) # read everything as character due to missingness codes

# Read in outcome file
outcome <- read_csv('~/Documents/GitHub/prefer_prepare/synth/data/e2e/outcomes_in_csv_format/template1_outcomes.csv') %>%
  rename(rinpersoon = RINPERSOON)

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

# This code will be updated for proper data splits later. For now, I'm using all synthetic data for which we have a synthetic outcome.
outcome_train <- outcome
features_train <- features %>% 
  filter(rinpersoon %in% outcome_train$rinpersoon)
  
#### MANUAL IDENTIFICATION OF DATA TYPES ####

# Identify date columns (these are continuous but require special treatment)
date_column_names <- c("DATE_STIRTHH", "DATUMEINDEHH") # these are in date format (e.g., 2024-06-18)

# Identify continuous variables
year_month_day_column_names <- grep("jaar|year|maand|month|dag|day", colnames(features), ignore.case = TRUE, value = TRUE)
columns_with_number_in_name <- grep("aantal|number", colnames(features), ignore.case = TRUE, value = TRUE)
other_numeric_columns <- c("BIRTHEDYOUNGCHILDHH", "GBAGENERATIE") 
continuous_variables <- c(year_month_day_column_names, columns_with_number_in_name, other_numeric_columns)

# TODO: Check with codebook/Mark about meaning of GBAGENERATIE. I think this means immigrant generation (i.e., first-gen, second-gen)

# All variables that we do not identify as continuous will be considered categorical (this includes binary)
categorical_variables <- setdiff(colnames(features), c("rinpersoon", "household_id", date_column_names, continuous_variables))

#### FUNCTIONS TO FORMAT DATA FOR MODELING #### 

# Function to change data type
change_data_type <- function(feature_data) { 
  cleaned_feature_data <- feature_data %>%
    mutate(across(all_of(continuous_variables), as.numeric)) %>%
    # Convert strings that represent dates to date-type, then numeric (cannot go straight to numeric)
    mutate(across(all_of(date_column_names), ~ as.numeric(as.Date(.))))%>%
    # Convert categorical variables to factor
    mutate(across(all_of(categorical_variables), as.factor))
  return(cleaned_feature_data)
}

# Function to create dataframe with cleaned feature data and outcome
generate_feature_and_outcome_dataframe <- function(feature_data, outcome) {
  cleaned_feature_data <- change_data_type(feature_data)
  # Merge feature data with outcome data to ensure rows are in the same order
  features_and_outcome <- full_join(cleaned_feature_data, outcome, by = "rinpersoon") %>%
    select(-all_of(c("rinpersoon", "household_id")))
  return(features_and_outcome)
}

# Function to generate dataframe of features
prep_features_for_model <- function(feature_data, outcome) { 
  features_and_outcome <- generate_feature_and_outcome_dataframe(feature_data, outcome)
  features_for_model <- features_and_outcome %>%
    select(-outcome)
  }

# Function to generate dataframe with the outcome
prep_outcome_for_model <- function(feature_data, outcome) { 
  features_and_outcome <- generate_feature_and_outcome_dataframe(feature_data, outcome)
  features_for_model <- features_and_outcome %>%
    mutate(outcome_as_factor = as.factor(make.names(outcome))) %>%
    pull(outcome_as_factor)
  }

#### MODELING ####

# Prepare data 
features_for_model <- prep_features_for_model(features_train, outcome_train)
outcome_for_model <- prep_outcome_for_model(features_train, outcome_train)

# Hyperparameter tuning (see https://catboost.ai/en/docs/concepts/r-usages-examples#selecting-hyperparameters)

# Grid for hyperparameter tuning
# TODO: adjust grid (these are default values from example)
hyperparameter_grid <- expand.grid(depth = c(4, 6, 8),
                                   learning_rate = 0.1,
                                   iterations = 100,
                                   l2_leaf_reg = 1e-3,
                                   rsm = 0.95,
                                   border_count = 64)

fit_control <- trainControl(method = "cv",
                            number = 4,
                            classProbs = TRUE) # TODO: decide on predicting probability vs. class

report <- train(features_for_model, 
                outcome_for_model,
                method = catboost.caret,
                logging_level = 'Verbose', 
                preProc = NULL,
                tuneGrid = hyperparameter_grid, 
                trControl = fit_control)

report$results