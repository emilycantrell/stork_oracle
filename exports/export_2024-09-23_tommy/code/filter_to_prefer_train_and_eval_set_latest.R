library(data.table) # 1.15.4 on CBS server
library(tidyverse) # 2.0.0 on CBS server

set.seed(seed_job)

# This file filters CBS datasets to contain only the official PreFer training and evaluation set, 
# so that the necessary data can be read into other code files more quickly. 

# Each section of the code is written so that it can be run without running the sections above.

# Save RINPERSOON and non-missing outcomes for official PreFer train set
prefer_official_train <- fread(data_files$prefer_official_train, colClasses = c(RINPERSOON = "character")) %>%
  filter(!is.na(children_post2021))
outcome_data <- prefer_official_train %>%
  select(RINPERSOON, children_post2021) %>%
  rename(outcome = children_post2021)
 official_train_rinpersoon <- outcome_data$RINPERSOON

# Save the rest of the official PreFer train set, filtered to just the PreFer official train set with non-missing outcomes
prefer_official_train <- prefer_official_train %>%
  select(-RINPERSOONS, -children_post2021, -RINPERSOONSVERBINTENISP, -RINPERSOONVERBINTENISP, -RINPERSOONSHKW, -RINPERSOONHKW, -SOORTOBJECTNUMMER, -RINOBJECTNUMMER, -HUISHOUDNR, -evaluation_set)

# Save PERSOONTAB, filtered to just the PreFer official train set with non-missing outcomes
gbapersoontab <- fread(data_files$gbapersoontab, colClasses = c(RINPERSOON = "character")) %>%
  filter(RINPERSOON %in% official_train_rinpersoon) %>%
  select(-RINPERSOONS, -GBAGEBOORTEDAG, -GBAGEBOORTEDAGMOEDER, -GBAGEBOORTEDAGVADER)

# Save householdbus (HUISHOUDENSBUS), filtered to just the PreFer official train set with non-missing outcomes
gbahuishoudensbus <- fread(data_files$gbahuishoudensbus, colClasses = c(RINPERSOON = "character", HUISHOUDNR = "character")) %>%
  filter(RINPERSOON %in% official_train_rinpersoon) %>%
  # Additional step: make unique household ID (must combine household ID with household start date to identify a unique household)
  # mutate(hh_id = paste0(HUISHOUDNR, "_", DATUMAANVANGHH)) %>%
  select(-HUISHOUDNR, -RINPERSOONS, -REFPERSOONHH) # remove the raw ID so we can never use it accidentally