library(data.table) # 1.15.4 on CBS server
library(tidyverse) # 2.0.0 on CBS server

# Note: The earliest available household start date is 1994-10-01. This means:
# For people aged approx. 18 - 25 in 2020, we have info on their birth household.
# For people aged approx. 25 - 42, we have info on household(s) from some year(s) under age 18.
# For people aged approx. 43 - 45, we don't have info on their household until adulthood.

# Note: I'm using data.table syntax to wrangle the data because it is MUCH faster than dplyr.

# Current household
gbahuishoudensbus <- gbahuishoudensbus[order(-DATUMAANVANGHH), # put household start date in descending order (most recent first)
                            .SD[1], # choose the top row per group (i.e., per person)
                            by = RINPERSOON] %>% # the grouping variable is RINPERSOON
  select(-DATUMAANVANGHH)
gc()
# Previous household (the household immediately before the current household)
# previous_household <- gbahuishoudensbus[order(-DATUMAANVANGHH), # put household start date in descending order (most recent first)  
#                             .SD[2], # choose the second-from-the-top row per group (i.e., per person)
#                             by = RINPERSOON] # the grouping variable is RINPERSOON 
# setnames(previous_household, 
#          old = setdiff(names(previous_household), "RINPERSOON"), 
#          new = paste0("previous_hh_", setdiff(names(previous_household), "RINPERSOON")))
# 
# # Last household in which the person was a "child living at home"
# # Note: some people in the target group left home before householdbus started in Oct. 1994; for them, this data will be missing.
# # Note: I'm doing this in two steps (creating the intermediate households_where_ego_is_child object) because it's faster than filtering & ordering in the same step
# households_where_ego_is_child <- gbahuishoudensbus[PLHH == 1]
# last_household_as_child <- households_where_ego_is_child[order(-DATUMAANVANGHH), # put household start date in descending order (most recent first)  
#                                                          .SD[1], # choose the top row per group (i.e., per person)
#                                                          by = RINPERSOON] # the grouping variable is RINPERSOON 
# setnames(last_household_as_child, 
#          old = setdiff(names(last_household_as_child), "RINPERSOON"), 
#          new = paste0("last_hh_as_child_", setdiff(names(last_household_as_child), "RINPERSOON")))
# 
# # Number of siblings (defined as maximum number of children in household other than the ego, when ego was a "child living at home") 
# number_of_siblings <- households_where_ego_is_child[, # no filtering needed
#                                                     .(number_of_siblings = max(AANTALKINDHH) -1), # create "number of siblings" column (subtracting 1 to subtract ego) 
#                                                     by = RINPERSOON] # the grouping variable is RINPERSOON
# 
# # Merge the wrangled features
# features_from_hh_bus <- merge(current_household, previous_household, by = "RINPERSOON", all = TRUE) # "all = TRUE" does a full outer join
# features_from_hh_bus <- merge(features_from_hh_bus, last_household_as_child, by = "RINPERSOON", all = TRUE)
# features_from_hh_bus <- merge(features_from_hh_bus, number_of_siblings, by = "RINPERSOON", all = TRUE)
# 
# #### CODE FOR DEVELOPMENT PROCESS (NOT USED IN THE MAIN CODE) ####
# 
# # Smaller dataframe with just a few miscellaneous people, for faster run times while developing the main code 
# toy <- gbahuishoudensbus %>%
#   filter(RINPERSOON %in% c("000003406", "000000030", "000012440", "000001967", "000000050"))
# 
# # Code to filter to just one person, to manually examine what their household pattern looks like
# person_to_check <- "000000050"
# toy %>%
#   filter(RINPERSOON == person_to_check) %>%
#   arrange((DATUMAANVANGHH)) %>%
#   View()
# 
# # Quality checks (run these before merging & deleting objects; manually compare to the df that is filtered to just one person)
# current_household %>% filter(RINPERSOON == person_to_check)
# previous_household %>% filter(RINPERSOON == person_to_check)
# last_household_as_child %>% filter(RINPERSOON == person_to_check)
# number_of_siblings %>% filter(RINPERSOON == person_to_check)
