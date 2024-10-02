library(tidyverse) # 2.0.0 on CBS server
library(readxl) # 1.4.3 on CBS server
library(fastDummies) # 1.7.3 on CBS server

# This file creates a file that assigns data type to columns

record_variable_types_in_df <- function(cbs_data_file) { 
  # Set up a data frame which will be filled in with data type in a subsequent step
  cbs_data_file <- select(cbs_data_file, -RINPERSOON)
  variable_name <- colnames(cbs_data_file)
  variable_type <- rep(NA, length(variable_name))
  variable_type_metadata <- cbind.data.frame(variable_name, variable_type)
  # Test that every column's data type was identified exactly once
  if (ncol(cbs_data_file) != length(continuous_features) + 
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

# #### prefer_official_train ####
# # Identify variables by shared strings in the name
# continuous_features <- prefer_official_train %>%
#   select(contains(c("JAAR", "MAAND", "DAG", "AANTAL", "pre2021", "_youngest", "birthday", "AANV", "n_", "average_", "s_total", "INPBELI", "P100", "INPPERSPRIM", "INHA", "BRUT", "INHUAFL", "VEH", "VZA"))) %>% # Note: month could reasonably be marked as categorical instead
#   names() # %>%
#   # c("INHUAF")
# 
# categorical_features <- prefer_official_train %>%
#   select(contains(c("GEBOORTELAND", "GESLACHT", "HERKOM", "GENERATIE", "IMPUTATIECODE", "OPLNIVSOI2021AGG4H","SCONTRACTSOORT_main", "SPOLISDIENSTVERBAND_main", "GBABURGERLIJKESTAATNW", "INPEM", "INPPINK", "INPPOSHHK", "INHBBIHJ", "INHEHALGR", "INHPOPIIV", "INHSAMAOW", "INHSAMHH", "TYP"))) %>% 
#   names() # %>%
#   # c("SECM", "SECM_partner")
# 
# # Record the variable names in a dataframe
# variable_type_metadata_prefer_official_train <- record_variable_types_in_df(cbs_data_file = prefer_official_train)
# 
# # Add more information about feature sets
# prefer_official_train_codebook_path <- prefer_official_train_codebook_path
# prefer_official_train_codebook <- read_xlsx(prefer_official_train_codebook_path) %>%
#   mutate(Source = sub("calculated based on ", "", Source),
#          Source = sub("created based on ", "", Source)) %>%
#   rename(variable_name = Var_name, feature_set = Source) %>%
#   select(variable_name, feature_set)
# metadata_prefer_official_train <- left_join(variable_type_metadata_prefer_official_train, prefer_official_train_codebook) %>%
#   dummy_cols(select_columns = "feature_set", remove_selected_columns = TRUE) %>%
#   # rename(feature_set_KINDOUDERTAB = "feature_set_KINDOUDERTAB and GBAPERSOONTAB") %>%
#   mutate(# feature_set_GBAPERSOONTAB = ifelse(feature_set_KINDOUDERTAB == 1, 1, feature_set_GBAPERSOONTAB),
#          feature_set_prefer_official_train = 1,
#          feature_set_sex_and_birthyear = ifelse(variable_name %in% c("GBAGESLACHT", "GBAGEBOORTEJAAR"), 1, 0))

# Notes: 
# GBAGEBOORTEJAAR has a very small number of values that are not plausible 

#### PERSOONTAB ####
# Identify variables by shared strings in the name
continuous_features <- gbapersoontab %>%
  select(contains(c("JAAR", "MAAND", "DAG", "AANTAL"))) %>% # Note: month could reasonably be marked as categorical instead
  names()
categorical_features <- gbapersoontab %>%
  select(contains(c("GEBOORTELAND", "GESLACHT", "HERKOMSTGROEPERING", "GENERATIE", "IMPUTATIECODE"))) %>%
  names()

# Record the variable names in a dataframe
variable_type_metadata_gbapersoontab <- record_variable_types_in_df(cbs_data_file = gbapersoontab)
metadata_gbapersoontab <- variable_type_metadata_gbapersoontab %>%
  # filter(!variable_name %in% variable_type_metadata_prefer_official_train$variable_name) %>%
  mutate(feature_set_GBAPERSOONTAB = 1)


#### HOUSEHOLDBUS ####
# Identify variables by shared strings in the name
continuous_features <- gbahuishoudensbus %>%
  select(contains(c("DATUM", "JAAR", "MAAND", "AANTAL", "number"))) %>% # Note: month could reasonably be marked as categorical instead
  names()
categorical_features <- gbahuishoudensbus %>%
  select(contains(c("TYPHH", "PLHH", "IMPUTATIECODEHH"))) %>%
  names()

# Record the variable names in a dataframe
variable_type_metadata_gbahuishoudensbus <- record_variable_types_in_df(cbs_data_file = gbahuishoudensbus)
metadata_gbahuishoudensbus <- variable_type_metadata_gbahuishoudensbus %>%
  # filter(!variable_name %in% variable_type_metadata_prefer_official_train$variable_name) %>%
  mutate(feature_set_GBAHUISHOUDENSBUS = 1)


#### SAVE OUTPUT ####
metadata <- list_rbind(list(metadata_gbapersoontab, metadata_gbahuishoudensbus)) # metadata_prefer_official_train
metadata[is.na(metadata)] <- 0