# On CBS server, this is file version filter_to_train_set_2024-08-24.R

library(data.table) # 1.15.4 on CBS server
library(tidyverse) # 2.0.0 on CBS server

# This file filters CBS datasets to contain only the official PreFer training and evaluation set, 
# so that the necessary data can be read into other code files more quickly. 

# Each section of the code is written so that it can be run without running the sections above.

# Save RINPERSOON and non-missing outcomes for official PreFer train set
train_path <- "H:/DATASETS/train.csv"
prefer_official_train <- fread(train_path, colClasses = c(RINPERSOON = "character")) %>%
  filter(!is.na(children_post2021))
outcome_data <- prefer_official_train %>%
  select(RINPERSOON, children_post2021) %>%
  rename(outcome = children_post2021)
outcome_path_to_write <- "H:/pmt/stork_oracle/1_filter_to_train_and_eval_set/prefer_official_train_outcomes.csv"
fwrite(outcome_data, outcome_path_to_write)
rm(outcome_data)

# Save the rest of the official PreFer train set, filtered to just the PreFer official train set with non-missing outcomes
prefer_official_train <- prefer_official_train %>%
  select(-RINPERSOONS, -children_post2021, -RINPERSOONSVERBINTENISP, -RINPERSOONVERBINTENISP, -RINPERSOONSHKW, -RINPERSOONHKW, -SOORTOBJECTNUMMER, -RINOBJECTNUMMER, -HUISHOUDNR, -evaluation_set)
prefer_official_train_path_to_write <- "H:/pmt/stork_oracle/1_filter_to_train_and_eval_set/prefer_official_train_prefer_official_train.csv"
fwrite(prefer_official_train, prefer_official_train_path_to_write)
rm(prefer_official_train)
gc()

# Save PERSOONTAB, filtered to just the PreFer official train set with non-missing outcomes
outcome_path <- "H:/pmt/stork_oracle/1_filter_to_train_and_eval_set/prefer_official_train_outcomes.csv"
outcome_data <- fread(outcome_path, colClasses = c(RINPERSOON = "character"))
official_train_rinpersoon <- outcome_data$RINPERSOON
rm(outcome_data)
persoon_path <- "G:/Bevolking/GBAPERSOONTAB/2020/geconverteerde data/GBAPERSOON2020TABV3.csv"
persoontab <- fread(persoon_path, colClasses = c(RINPERSOON = "character"))
persoontab <- persoontab %>%
  filter(RINPERSOON %in% official_train_rinpersoon) %>%
  select(-RINPERSOONS, -GBAGEBOORTEDAG, -GBAGEBOORTEDAGMOEDER, -GBAGEBOORTEDAGVADER)
persoontab_path_to_write <- "H:/pmt/stork_oracle/1_filter_to_train_and_eval_set/prefer_official_train_GBAPERSOON2020TABV3.csv"
fwrite(persoontab, persoontab_path_to_write)

# Save householdbus (HUISHOUDENSBUS), filtered to just the PreFer official train set with non-missing outcomes
outcome_path <- "H:/pmt/stork_oracle/1_filter_to_train_and_eval_set/prefer_official_train_outcomes.csv"
outcome_data <- fread(outcome_path, colClasses = c(RINPERSOON = "character"))
official_train_rinpersoon <- outcome_data$RINPERSOON
rm(outcome_data)
hhbus_path <- "G:/Bevolking/GBAHUISHOUDENSBUS/geconverteerde data/GBAHUISHOUDENS2020BUSV1.csv"
hh_bus <- fread(hhbus_path, colClasses = c(RINPERSOON = "character", HUISHOUDNR = "character"))
hh_bus <- hh_bus %>%
  filter(RINPERSOON %in% official_train_rinpersoon)
# Additional step: make unique household ID (must combine household ID with household start date to identify a unique household)
hh_bus <- hh_bus %>%
  mutate(hh_id = paste0(HUISHOUDNR, "_", DATUMAANVANGHH)) %>%
  select(-HUISHOUDNR, -RINPERSOONS, -REFPERSOONHH) # remove the raw ID so we can never use it accidentally
hhbus_path_to_write <- "H:/pmt/stork_oracle/1_filter_to_train_and_eval_set/prefer_official_train_GBAHUISHOUDENS2020BUSV1.csv"
fwrite(hh_bus, hhbus_path_to_write)