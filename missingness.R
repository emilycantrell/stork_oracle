library(data.table)
library(tidyverse)

prefer_train_and_eval <- fread("H:/DATASETS/train.csv", colClasses = c(RINPERSOON = "character"))
table1 <- table(prefer_train_and_eval$children_post2021, prefer_train_and_eval$evaluation_set, useNA = "always")
table1
prefer_official_holdout_labels <- fread("H:/dataset prep 2024/temporary/final_leaderboard_outcome.csv", colClasses = c(RINPERSOON = "character"))
table2 <- table(prefer_official_holdout_labels$children_post2021, useNA = "always")

prefer_official_holdout_features <- fread("H:/DATASETS/holdout_final_leaderboard.csv", colClasses = c(RINPERSOON = "character"))
which(prefer_official_holdout_features$RINPERSOON != prefer_official_holdout_labels$RINPERSOON)
if (table2[3] == 0 & length(which(prefer_official_holdout_features$RINPERSOON != prefer_official_holdout_labels$RINPERSOON)) == 0) {
  holdout_outcome_missingness <- 0
}
sampling_file <- "H:/pmt/eval/train_and_eval_samples/pmt_train_and_evaluation_samples_seed_1_241016_with_holdout.csv" %>%
  fread(colClasses = c(RINPERSOON = "character")) %>%
  mutate(sample = case_when(training_set == 1 ~ "training_set",
                            evaluation_test_50_percent_split == 1 ~ "evaluation_test_50_percent_split",
                            evaluation_selection_50_percent_split == 1 ~ "evaluation_selection_50_percent_split",
                            official_holdout_set == 1 ~ "official_holdout_set"
                            )) %>%
  select(RINPERSOON, sample)
outcome_data <- "H:/pmt/stork_oracle/stork_oracle_december_2024/preprocessed_files_for_quick_test_runs/outcome_data.csv" %>%
  fread(colClasses = c(RINPERSOON = "character"))
data <- "H:/pmt/stork_oracle/stork_oracle_december_2024/preprocessed_files_for_quick_test_runs/data.csv" %>%
  fread(colClasses = c(RINPERSOON = "character")) %>%
  select(-GBAIMPUTATIECODE, -DATUMEINDEHH, -INHPOPIIV, -INHUAF, INHUAFL, -starts_with(c("GEB", "n_", "INPEM", "INPP100P", "INHARMEUR", "VEHP100")), -contains(c("household_child_", "_partner", "children_of_sex_")), -ends_with(c("_youngest", "_main", "PARTNER"))) %>%
  mutate(across(starts_with("GBAGEBOORTELAND"), ~if_else(.x == "_0", NA, .x)),
         across(all_of(c("INPPINK", "INPPOSHHK", "INHEHALGR", "INHUAFTYP")), ~if_else(.x == "_9", NA, .x)),
         across(starts_with("INHSAM"), ~if_else(.x == "_88", NA, .x)),
         across(all_of(c("INHBBIHJ", "VBOWoningtype")), ~ if_else(.x == "_99", NA, .x)),
         across(-RINPERSOON, ~if_else(.x %in% c("_-", "_NA", NA), 1, 0))) %>%
  full_join(sampling_file) %>%
  full_join(outcome_data) %>%
  select(-RINPERSOON) %>%
  group_by(sample, outcome) %>%
  summarize(n = n(), across(everything(), sum)) %>%
  ungroup() %>%
  mutate(
    prefer_set = c("evaluation_set", "evaluation_set", "evaluation_set", "evaluation_set", "official_holdout_set", "official_holdout_set", "training_set",  "training_set"),
    outcome_missing_from_prefer_set = c(table1[3,2], table1[3,2], table1[3,2], table1[3,2], holdout_outcome_missingness, holdout_outcome_missingness, table1[3,1], table1[3,1])) %>%
  select(sample, outcome, n, prefer_set, outcome_missing_from_prefer_set, everything())
fwrite(data, "missingness.csv")