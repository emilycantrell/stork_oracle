library(data.table) # 1.15.4 on CBS server
library(tidyverse) # 2.0.0 on CBS server

set.seed(seed_job)

# This file filters CBS datasets to contain only the rows and columns we need, 
# so that the necessary data can be used more quickly. 

# Save RINPERSOON and outcomes (if any) for official PreFer datasets
# First we need a helper function that converts columns of type "integer64"  
# to be numeric, or we won't be able to stack the datasets together
handle_integer64 <- function(df) {
  classes <- map_chr(df, class)
  integer64 <- names(classes)[which(classes == "integer64")]
  mutate(df, across(all_of(integer64), as.numeric))
}
prefer_official_train <- fread(data_files$prefer_official_train, colClasses = c(RINPERSOON = "character")) %>%
  filter(!is.na(children_post2021)) %>%
  handle_integer64()
intermediate <- fread(data_files$intermediate, colClasses = c(RINPERSOON = "character")) %>%
  handle_integer64()
intermediate_rinpersoon <- intermediate$RINPERSOON
final <- fread(data_files$final, colClasses = c(RINPERSOON = "character")) %>%
  handle_integer64()
final_rinpersoon <- final$RINPERSOON
prefer_official <- bind_rows(prefer_official_train, intermediate, final)
rm(prefer_official_train)
rm(intermediate)
rm(final)
gc()
outcome_data <- prefer_official %>%
  select(RINPERSOON, children_post2021) %>%
  rename(outcome = children_post2021)
official_rinpersoon <- outcome_data$RINPERSOON

# Save the rest of the official PreFer dataset columns
prefer_official <- prefer_official %>%
  select(-RINPERSOONS, -children_post2021, -RINPERSOONSVERBINTENISP, -RINPERSOONVERBINTENISP, -RINPERSOONSHKW, -RINPERSOONHKW, -SOORTOBJECTNUMMER, -RINOBJECTNUMMER, -HUISHOUDNR, -evaluation_set)
prefer_official_columns <- colnames(prefer_official)
prefer_official_columns <- prefer_official_columns[!prefer_official_columns %in% c("RINPERSOON", "DATUMAANVANGHH")]


# Save PERSOONTAB, filtered to just the PreFer official dataset rows and columns that are not already in the official dataset, as well as RINPERSOON
gbapersoontab <- fread(data_files$gbapersoontab, colClasses = c(RINPERSOON = "character")) %>%
  filter(RINPERSOON %in% official_rinpersoon) %>%
  select(-RINPERSOONS, -GBAGEBOORTEDAG, -GBAGEBOORTEDAGMOEDER, -GBAGEBOORTEDAGVADER, -any_of(prefer_official_columns))
gc()

# Save householdbus (HUISHOUDENSBUS), filtered to just the PreFer official dataset rows and columns that are not already in the official dataset, as well as RINPERSOON and DATUMAANVANGHH
gbahuishoudensbus <- fread(data_files$gbahuishoudensbus, colClasses = c(RINPERSOON = "character", HUISHOUDNR = "character")) %>%
  filter(RINPERSOON %in% official_rinpersoon) %>%
  # Additional step: make unique household ID (must combine household ID with household start date to identify a unique household)
  # mutate(hh_id = paste0(HUISHOUDNR, "_", DATUMAANVANGHH)) %>%
  select(-HUISHOUDNR, -RINPERSOONS, -REFPERSOONHH, -any_of(prefer_official_columns)) # remove the raw ID so we can never use it accidentally
gc()