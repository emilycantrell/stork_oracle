library(tidyverse) # 2.0.0 on CBS server
library(readxl) # 1.4.3 on CBS server
library(fastDummies) # 1.7.3 on CBS server

# This file creates a file that assigns data type to columns

record_variable_types_in_df <- function(data_file) { 
  # Set up a data frame which will be filled in with data type in a subsequent step
  data_file <- select(data_file, -any_of(c("RINPERSOON", "household_id")))
  variable_name <- colnames(data_file)
  variable_type <- rep(NA, length(variable_name))
  variable_type_metadata <- cbind.data.frame(variable_name, variable_type)
  # Test that every column's data type was identified exactly once
  if (ncol(data_file) != length(continuous_features) + 
      length(categorical_features)) {
        stop("Each column should have its data type identified exactly once.")
      }
  
  # Add the data types to the data frame
  variable_type_metadata <- variable_type_metadata %>%
    mutate(variable_type = case_when(
      variable_name %in% continuous_features ~ "continuous", 
      variable_name %in% categorical_features ~ "categorical_or_binary_original_variable",
      TRUE ~ NA
    ))
  return(variable_type_metadata)
}

#### prefer_official_train ####
# Identify variables by shared strings in the name
continuous_features <- prefer_official_train %>%
  select(contains(c("JAAR", "MAAND", "DAG", "AANTAL", "pre2021", "_youngest", "birthday", "AANV", "n_", "average_", "s_total", "INPBELI", "P100", "INPPERSPRIM", "INHA", "BRUT", "INHUAFL", "VEH", "VZA"))) %>% # Note: month could reasonably be marked as categorical instead
  names() %>%
  c("INHUAF")

categorical_features <- prefer_official_train %>%
  select(contains(c("GEBOORTELAND", "GESLACHT", "HERKOM", "GENERATIE", "IMPUTATIECODE", "OPLNIVSOI2021AGG4H","SCONTRACTSOORT_main", "SPOLISDIENSTVERBAND_main", "GBABURGERLIJKESTAATNW", "INPEM", "INPPINK", "INPPOSHHK", "INHBBIHJ", "INHEHALGR", "INHPOPIIV", "INHSAMAOW", "INHSAMHH", "TYP"))) %>%
  names() %>%
  c("SECM", "SECM_partner")

# Record the variable names in a dataframe
variable_type_metadata_prefer_official_train <- record_variable_types_in_df(data_file = prefer_official_train)

# Add more information about feature sets
prefer_official_train_codebook_path <- prefer_official_train_codebook_path
prefer_official_train_codebook <- read_xlsx(prefer_official_train_codebook_path) %>%
  mutate(Source = sub("calculated based on ", "", Source),
         Source = sub("created based on ", "", Source)) %>%
  rename(variable_name = Var_name, feature_set = Source) %>%
  select(variable_name, feature_set)
metadata_prefer_official_train <- left_join(variable_type_metadata_prefer_official_train, prefer_official_train_codebook) %>%
  dummy_cols(select_columns = "feature_set", remove_selected_columns = TRUE) %>%
  # rename(feature_set_KINDOUDERTAB = "feature_set_KINDOUDERTAB and GBAPERSOONTAB") %>%
  mutate(# feature_set_GBAPERSOONTAB = ifelse(feature_set_KINDOUDERTAB == 1, 1, feature_set_GBAPERSOONTAB),
         feature_set_prefer_official_train = 1,
         feature_set_sex_and_birthyear = ifelse(variable_name %in% c("GBAGESLACHT", "GBAGEBOORTEJAAR"), 1, 0))

# Notes: 
# GBAGEBOORTEJAAR has a very small number of values that are not plausible 

#### PERSOONTAB ####
# Identify variables by shared strings in the name
continuous_features <- gbapersoontab_train %>%
  select(contains(c("JAAR", "MAAND", "DAG", "AANTAL"))) %>% # Note: month could reasonably be marked as categorical instead
  names()
categorical_features <- gbapersoontab_train %>%
  select(contains(c("GEBOORTELAND", "GESLACHT", "HERKOMSTGROEPERING", "GENERATIE", "IMPUTATIECODE"))) %>%
  names()

# Record the variable names in a dataframe
variable_type_metadata_gbapersoontab <- record_variable_types_in_df(data_file = gbapersoontab_train)
metadata_gbapersoontab <- variable_type_metadata_gbapersoontab %>%
  filter(!variable_name %in% variable_type_metadata_prefer_official_train$variable_name) %>%
  mutate(feature_set_GBAPERSOONTAB = 1)


#### HOUSEHOLDBUS ####
# Identify variables by shared strings in the name
continuous_features <- gbahuishoudensbus_train_focal_people_only_current_household %>%
  select(contains(c("DATUM", "JAAR", "MAAND", "AANTAL", "number"))) %>% # Note: month could reasonably be marked as categorical instead
  names()
categorical_features <- gbahuishoudensbus_train_focal_people_only_current_household %>%
  select(contains(c("TYPHH", "PLHH", "IMPUTATIECODEHH"))) %>%
  names()

# Record the variable names in a dataframe
variable_type_metadata_gbahuishoudensbus <- record_variable_types_in_df(data_file = gbahuishoudensbus_train_focal_people_only_current_household)
metadata_gbahuishoudensbus <- variable_type_metadata_gbahuishoudensbus %>%
  filter(!variable_name %in% variable_type_metadata_prefer_official_train$variable_name) %>%
  mutate(feature_set_GBAHUISHOUDENSBUS = 1)

#### live_in_partner_data ####
# This is data about married, registered, or unregistered partners who live in ego's most recent household (end of 2020)

continuous_features <- c("live_in_partner_age", "start_date_of_first_household_with_partner")
categorical_features <- c("live_in_partner_GBAGESLACHT")
variable_type_metadata_live_in_partner_data <- record_variable_types_in_df(data_file = live_in_partner_data)
metadata_live_in_partner_data <- variable_type_metadata_live_in_partner_data %>%
  mutate(feature_set_live_in_partner = 1)

#### FAMILIENETWERKTAB ####
# Note: this is named "features_from_familienetwerktab" rather than "familienetwerktab" because it has undergone significant feature engineering and does not resemble the raw file
continuous_features <- features_from_familienetwerktab %>%
  select(-RINPERSOON) %>% # all features are continuous counts of number of family members of various types
  names()
categorical_features <- c()
variable_type_metadata_features_from_familienetwerktab <- record_variable_types_in_df(data_file = features_from_familienetwerktab)
metadata_features_from_familienetwerktab <- variable_type_metadata_features_from_familienetwerktab %>%
  mutate(feature_set_FAMILIENETWERKTAB = 1)


#### child_age_and_sex_by_household #### 
continuous_features <- child_age_and_sex_by_household %>% 
  select(contains("age_household")) %>% # Note: age is a substring of GBAGESLACHT, hence the need for a longer string
  names()
categorical_features <- child_age_and_sex_by_household %>%
  select(contains("GBAGESLACHT_household")) %>% 
  names()
variable_type_metadata_child_age_and_sex_by_household <- record_variable_types_in_df(data_file = child_age_and_sex_by_household)
metadata_features_from_child_age_and_sex_by_household <- variable_type_metadata_child_age_and_sex_by_household %>%
  filter(!variable_name %in% variable_type_metadata_prefer_official_train$variable_name) %>%
  mutate(feature_set_child_age_and_sex_by_household = 1)

#### MERGE RESULTS TOGETHER ####
metadata <- list_rbind(list(metadata_gbapersoontab, 
                            metadata_gbahuishoudensbus, 
                            metadata_prefer_official_train,
                            metadata_live_in_partner_data, 
                            metadata_features_from_familienetwerktab, 
                            metadata_features_from_child_age_and_sex_by_household))
metadata[is.na(metadata)] <- 0

#### CREATE ADDITIONAL FEATURE SETS ####
features_about_number_of_children <- c("children_pre2021", "AANTALKINDHH")
metadata <- metadata %>% 
  mutate(features_about_number_of_children = ifelse(variable_name %in% features_about_number_of_children, 1, 0))
