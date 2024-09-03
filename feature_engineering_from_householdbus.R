# On CBS server, this is file version feature_engineering_from_householdbus_2024-08-28.R

library(data.table) # 1.15.4 on CBS server
library(tidyverse) # 2.0.0 on CBS server

# This file creates columns of information about households, with one row for each person from the official prefer training and evaluation set

# Read hhbus
hhbus_path <- "H:/pmt/stork_oracle/1_filter_to_train_and_eval_set/prefer_official_train_GBAHUISHOUDENS2020BUSV1.csv"
hh_bus <- fread(hhbus_path, colClasses = c(RINPERSOON = "character")) %>%
  select(!all_of(c("hh_id")))

# Note: I'm using data.table syntax to wrangle the data because it is MUCH faster than dplyr. 

# Columns about current household
current_household <- hh_bus[order(-DATUMAANVANGHH), # put household start date in descending order (most recent first)  
                            .SD[1], # choose the top row per group (i.e., per person)
                            by = RINPERSOON] # the grouping variable is RINPERSOON 
setnames(current_household, 
         old = setdiff(names(current_household), "RINPERSOON"), 
         new = paste0("current_hh_", setdiff(names(current_household), "RINPERSOON")))

# Columns about previous household
# Previous household (the household immediately before the current household)
# previous_household <- hh_bus[order(-DATUMAANVANGHH), # put household start date in descending order (most recent first)  
#                             .SD[2], # choose the second-from-the-top row per group (i.e., per person)
#                             by = RINPERSOON] # the grouping variable is RINPERSOON 
# setnames(previous_household, 
#          old = setdiff(names(previous_household), "RINPERSOON"), 
#          new = paste0("previous_hh_", setdiff(names(previous_household), "RINPERSOON")))

# Columns about last household in which the person was a "child living at home"
# Note: I'm doing this in two steps (creating the intermediate households_where_ego_is_child object) because it's faster than filtering & ordering in the same step
households_where_ego_is_child <- hh_bus[PLHH == 1]
last_household_as_child <- households_where_ego_is_child[order(-DATUMAANVANGHH), # put household start date in descending order (most recent first)  
                                                         .SD[1], # choose the top row per group (i.e., per person)
                                                         by = RINPERSOON] # the grouping variable is RINPERSOON 
setnames(last_household_as_child, 
         old = setdiff(names(last_household_as_child), "RINPERSOON"), 
         new = paste0("last_hh_as_child_", setdiff(names(last_household_as_child), "RINPERSOON")))

# Number of siblings (defined as maximum number of children in household other than the ego, when ego was a "child living at home") 
number_of_siblings <- households_where_ego_is_child[, # no filtering needed
                                                    .(number_of_siblings = max(AANTALKINDHH) -1), # create "number of siblings" column (subtracting 1 to subtract ego) 
                                                    by = RINPERSOON] # the grouping variable is RINPERSOON

# Clear objects that are no longer needed
rm(hh_bus)
rm(households_where_ego_is_child)
gc()

# Merge the wrangled features
# features_from_hh_bus <- merge(current_household, previous_household, by = "RINPERSOON", all = TRUE) # "all = TRUE" does a full outer join
# rm(current_household, previous_household)
features_from_hh_bus <- merge(current_household, last_household_as_child, by = "RINPERSOON", all = TRUE)
rm(current_household, last_household_as_child)
features_from_hh_bus <- merge(features_from_hh_bus, number_of_siblings, by = "RINPERSOON", all = TRUE)

# Save the results
features_path_to_write <- "H:/pmt/stork_oracle/2_feature_engineering/features_from_householdbus2020.csv"
fwrite(features_from_hh_bus, features_path_to_write)