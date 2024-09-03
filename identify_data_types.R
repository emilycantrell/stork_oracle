# On CBS server, this is file version identify_data_types_2024-08-28.R

library(data.table) # 1.15.4 on CBS server
library(tidyverse) # 2.0.0 on CBS server

# This file creates a file that assigns data type to columns

#### FUNCTIONS ####
view_variable <- function(raw_data_file, column_number) { 
  raw_data_file %>% 
    group_by(across(column_number)) %>%
    count() %>%
    View()
  print(colnames(raw_data_file)[column_number]) # print so the the name can be copy-pasted
}

record_variable_types_in_df <- function(cbs_data_file) { 
  # Set up a data frame which will be filled in with data type in a subsequent step
  variable_name <- colnames(cbs_data_file)
  cbs_data_file_name <- deparse(substitute(cbs_data_file))
  data_file <- rep(cbs_data_file_name, length(variable_name))
  variable_type <- rep(NA, length(variable_name))
  variable_type_metadata <- cbind.data.frame(data_file, variable_name, variable_type)
  # Test that every column's data type was identified exactly once
  if (ncol(cbs_data_file) != length(id_variables) + length(continuous_features) + 
      length(categorical_features)) {
        stop("Each column should have its data type identified exactly once.")
      }
  
  # Add the data types to the data frame
  variable_type_metadata <- variable_type_metadata %>%
    mutate(variable_type = case_when(
      data_file == cbs_data_file_name & variable_name %in% id_variables ~ "id", 
      data_file == cbs_data_file_name & variable_name %in% continuous_features ~ "continuous", 
      data_file == cbs_data_file_name & variable_name %in% categorical_features ~ "categorical",
      TRUE ~ NA
    ))
  return(variable_type_metadata)
}

#### prefer_official_train ####

# Read data
prefer_official_train_path <- "H:/pmt/stork_oracle/1_filter_to_train_and_eval_set/prefer_official_train_prefer_official_train.csv"
prefer_official_train <- fread(prefer_official_train_path, colClasses = c(RINPERSOON = "character"))

# Examine the variables to determine data types (manually change the number in this line to review each variable)
# Note: to determine data types, I also reviewed the CBS documentation
view_variable(prefer_official_train, 1)  

# Identify variables by shared strings in the name
id_variables <- c("RINPERSOON")
continuous_features <- prefer_official_train %>%
  select(contains(c("JAAR", "MAAND", "DAG", "AANTAL", "pre2021", "_youngest", "birthday", "AANV", "n_", "average_", "s_total", "INPBELI", "P100", "INPPERSPRIM", "INHA", "BRUT", "INHUAFL", "VEH", "VZA"))) %>% # Note: month could reasonably be marked as categorical instead
  names() %>%
  c("INHUAF")

categorical_features <- prefer_official_train %>%
  select(contains(c("GEBOORTELAND", "GESLACHT", "HERKOM", "GENERATIE", "IMPUTATIECODE", "OPLNIVSOI2021AGG4H","SCONTRACTSOORT_main", "SPOLISDIENSTVERBAND_main", "GBABURGERLIJKESTAATNW", "INPEM", "INPPINK", "INPPOSHHK", "INHBBIHJ", "INHEHALGR", "INHPOPIIV", "INHSAMAOW", "INHSAMHH", "TYP"))) %>% 
  names() %>%
  c("SECM", "SECM_partner")

# Record the variable names in a dataframe
variable_type_metadata_prefer_official_train <- record_variable_types_in_df(cbs_data_file = prefer_official_train)
rm(prefer_official_train)
gc()


#### PERSOONTAB ####

# Read data
persoon_path <- "H:/pmt/stork_oracle/1_filter_to_train_and_eval_set/prefer_official_train_GBAPERSOON2020TABV3.csv"
persoontab <- fread(persoon_path, colClasses = c(RINPERSOON = "character"))

# Examine the variables to determine data types (manually change the number in this line to review each variable)
# Note: to determine data types, I also reviewed the CBS documentation
view_variable(persoontab, 1)  

# Identify variables by shared strings in the name
id_variables <- c("RINPERSOON")
continuous_features <- persoontab %>%
  select(contains(c("JAAR", "MAAND", "DAG", "AANTAL"))) %>% # Note: month could reasonably be marked as categorical instead
  names()
categorical_features <- persoontab %>%
  select(contains(c("GEBOORTELAND", "GESLACHT", "HERKOMSTGROEPERING", "GENERATIE", "IMPUTATIECODE"))) %>% 
  names()
  
# Record the variable names in a dataframe
variable_type_metadata_persoontab <- record_variable_types_in_df(cbs_data_file = persoontab)
rm(persoontab)
gc()


#### HOUSEHOLDBUS ####
hhbus_features_path <- "H:/pmt/stork_oracle/2_feature_engineering/features_from_householdbus2020.csv"
hhbus_features <- fread(hhbus_features_path, colClasses = c(RINPERSOON = "character")) %>%
  select(-RINPERSOON)

# Manually review the features (adjust the number in view_variable to indicate the column number)
view_variable(hhbus_features, 1)
# Note: in this review, I found that all missing values for hh_features variables are marked as NA

# Identify variables by shared strings in the name
id_variables <- c()
continuous_features <- hhbus_features %>%
  select(contains(c("DATUM", "JAAR", "MAAND", "AANTAL", "number"))) %>% # Note: month could reasonably be marked as categorical instead
  names()
categorical_features <- hhbus_features %>% 
  select(contains(c("TYPHH", "PLHH", "IMPUTATIECODEHH"))) %>%
  names()

# Record the variable names in a dataframe
variable_type_metadata_hhbus <- record_variable_types_in_df(cbs_data_file = hhbus_features)


#### SAVE OUTPUT ####
variable_type_metadata <- rbind.data.frame(variable_type_metadata_prefer_official_train, variable_type_metadata_persoontab, variable_type_metadata_hhbus)
setwd("H:/pmt/stork_oracle/3_create_metadata/")
fwrite(variable_type_metadata, "variable_type_metadata.csv") 