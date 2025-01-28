# This is a configuration file specifying settings for parallelizaton, feature
# choices, train-test splits, modeling options, and results output

# Parallelization settings
seed_start <- 0
seed_worker <- 0
seed_selection <- 0
workers_for_fitting_models <- 3
workers_for_first_round_evaluation <- 3
workers_for_second_round_evaluation <- 3
parallelize_evaluation_sets_instead_of_grid_rows_in_first_round <- TRUE
parallelize_evaluation_sets_instead_of_grid_rows_in_second_round <- FALSE
#n_thread_within_worker <- -1

# Feature choices
# NB: make sure to use equal signs here, not arrows
feature_set_settings <- list(
  # AS = c("family_age_and_sex_from_prefer_submission"),
  # FS = c("family_structure"), 
  # IN = c("income_assets_benefits"), 
  # ED = c("education"), 
  # IM = c("immigration_ethnicity"), 
  # HO = c("housing"), 
  # EM = c("employment"), 
  # CH = c("childcare_proximity"), 
  # AS_FS = c("family_age_and_sex_from_prefer_submission",
  #           "family_structure"),
  # AS_FS_IN = c("family_age_and_sex_from_prefer_submission",
  #              "family_structure",
  #              "income_assets_benefits"),
  # AS_FS_IN_ED = c("family_age_and_sex_from_prefer_submission",
  #                 "family_structure",
  #                 "income_assets_benefits",
  #                 "education"),
  # AS_FS_IN_ED_IM = c("family_age_and_sex_from_prefer_submission",
  #                    "family_structure",
  #                    "income_assets_benefits",
  #                    "education",
  #                    "immigration_ethnicity"),
  # AS_FS_IN_ED_IM_HO = c("family_age_and_sex_from_prefer_submission",
  #                       "family_structure",
  #                       "income_assets_benefits",
  #                       "education",
  #                       "immigration_ethnicity",
  #                       "housing"),
  # AS_FS_IN_ED_IM_HO_EM = c("family_age_and_sex_from_prefer_submission",
  #                          "family_structure",
  #                          "income_assets_benefits",
  #                          "education",
  #                          "immigration_ethnicity",
  #                          "housing",
  #                          "employment"),
  # AS_FS_IN_ED_IM_HO_EM_CH = c("family_age_and_sex_from_prefer_submission",
  #                             "family_structure",
  #                             "income_assets_benefits",
  #                             "education",
  #                             "immigration_ethnicity",
  #                             "housing",
  #                             "employment",
  #                             "childcare_proximity"),
  # FS = c("family_structure"),
  # FS_AS_all = c("family_structure", "family_age_and_sex_from_prefer_submission", "family_age_and_sex_not_used_in_prefer_submission"),
  # FS_AS_not_prefer = c("family_structure", "family_age_and_sex_not_used_in_prefer_submission"),
  # FS_immigration = c("family_structure", "immigration_ethnicity"), 
  # FS_income = c("family_structure", "income_assets_benefits"), 
  # FS_education = c("family_structure", "education"), 
  # FS_employment = c("family_structure", "employment"),
  # FS_housing = c("family_structure", "housing"), 
  # FS_childcare = c("family_structure", "childcare_proximity"),
  # all_topics = c("family_structure", "family_age_and_sex_from_prefer_submission", "immigration_ethnicity", 
  # "income_assets_benefits", "education","employment", "housing", "childcare_proximity"),
  # AS_all = c("family_age_and_sex_from_prefer_submission", "family_age_and_sex_not_used_in_prefer_submission"),
  # AS_prefer = c("family_age_and_sex_from_prefer_submission"), 
  # AS_not_prefer = c("family_age_and_sex_not_used_in_prefer_submission"),
  # immigration = c("immigration_ethnicity"), 
  # income = c("income_assets_benefits"), 
  # education = c("education"), 
  # employment = c("employment"),
  # housing = c("housing"), 
  # childcare = c("childcare_proximity")
  # ego_AS = c("ego_AS"), 
  # ego_partner_AS = c("ego_AS", "partner_AS"), 
  # ego_partner_hhchildren_AS = c("ego_AS", "partner_AS", "child_age_and_sex_by_household")
  # all_plus_LiveInPartner = c("GBAPERSOONTAB", "GBAHUISHOUDENSBUS", "prefer_official_train", "FAMILIENETWERKTAB", "live_in_partner")
  # all_plus_hhchildAS = c("GBAPERSOONTAB", "GBAHUISHOUDENSBUS", "prefer_official_train", "FAMILIENETWERKTAB", "child_age_and_sex_by_household")
  # number_of_children = c("number_of_children"),
  # household_birthdays = c("household_birthdays"),
  # all_records = c("GBAPERSOONTAB", "GBAHUISHOUDENSBUS", "prefer_official_train", "FAMILIENETWERKTAB")
  # augmented_records = c("GBAPERSOONTAB", "GBAHUISHOUDENSBUS", "prefer_official_train"),
  # key_records = c("GBAPERSOONTAB", "GBAHUISHOUDENSBUS"),
  # familienetwerktab = c("FAMILIENETWERKTAB"),
  # hhbus = c("GBAHUISHOUDENSBUS"),
  # persoontab = c("GBAPERSOONTAB"),
  # sex_and_birthyear = c("sex_and_birthyear"),
  # prefer_official_train = c("prefer_official_train"),
  without_leakage = c("GBAPERSOONTAB", "GBAHUISHOUDENSBUS_without_leakage", "prefer_official_train", "FAMILIENETWERKTAB", "live_in_partner")
  # with_leakage = c("GBAPERSOONTAB", "GBAHUISHOUDENSBUS", "prefer_official_train", "FAMILIENETWERKTAB", "live_in_partner")
)

# Train-test splits 
sampling_files <- c("pmt_train_and_evaluation_group_samples_seed_1.csv")
data_splits <- bind_rows(
  expand_grid(
    training_sets = c("training_set"),
    selection_sets = c("evaluation_set",
                       "outcome__0__evaluation_set",
                       "outcome__1__evaluation_set",
                       "gender__male__evaluation_set",
                       "gender__female__evaluation_set",
                       "birthyear7__19751978__evaluation_set",
                       "birthyear7__19791982__evaluation_set",
                       "birthyear7__19831986__evaluation_set",
                       "birthyear7__19871990__evaluation_set",
                       "birthyear7__19911994__evaluation_set",
                       "birthyear7__19951998__evaluation_set",
                       "birthyear7__19992002__evaluation_set",
                       "birthyear3__19962002__evaluation_set",
                       "birthyear3__19861995__evaluation_set",
                       "birthyear3__19751985__evaluation_set",
                       "edu9__preprimary__evaluation_set",
                       "edu9__primary__evaluation_set",
                       "edu9__lower_secondary__evaluation_set",
                       "edu9__upper_secondary__evaluation_set",
                       "edu9__post_secondary_non_tertiary__evaluation_set",
                       "edu9__short_tertiary__evaluation_set",
                       "edu9__bachelor__evaluation_set",
                       "edu9__master__evaluation_set",
                       "edu9__doctor__evaluation_set",
                       "edu3__low__evaluation_set",
                       "edu3__middle__evaluation_set",
                       "edu3__high__evaluation_set",
                       "immigrant__no__evaluation_set",
                       "immigrant__first_gen__evaluation_set",
                       "immigrant__second_gen__evaluation_set",
                       "income__quintile1__evaluation_set",
                       "income__quintile2__evaluation_set",
                       "income__quintile3__evaluation_set",
                       "income__quintile4__evaluation_set",
                       "income__quintile5__evaluation_set",
                       "income__no__evaluation_set",
                       "income__institutional_household__evaluation_set",
                       "employment__permanent__evaluation_set",
                       "employment__temporary__evaluation_set",
                       "employment__no__evaluation_set",
                       "household__single_member__evaluation_set",
                       "household__unmarried_childless_couple__evaluation_set",
                       "household__married_childless_couple__evaluation_set",
                       "household__unmarried_parents__evaluation_set",
                       "household__married_parents__evaluation_set",
                       "household__single_parent__evaluation_set",
                       "household__institutional__evaluation_set",
                       "marriage_civil_partnership__no__evaluation_set",
                       "marriage_civil_partnership__yes__evaluation_set",
                       "children_pre5__0__evaluation_set",
                       "children_pre5__1__evaluation_set",
                       "children_pre5__2__evaluation_set",
                       "children_pre5__3__evaluation_set",
                       "children_pre5__4_plus__evaluation_set",
                       "children_pre3__2_plus__evaluation_set"
                       ), # Evaluation sets we use to select the best pipelines
    test_sets = c() # Evaluation sets we use for holdout evaluations.
  )
)

# Modeling options
model_settings <- list(
  # training_mean = list()
  catboost = list(tibble(grid_row = pmap(expand_grid(
    learning_rate = c(NA),
    subsample = c(0.8),
    depth = c(6)
  ),
  list)),
  steps = c(1)) # , how many trees in catboost?
  # catboost = expand_grid(tibble(grid_row = pmap(expand_grid(
  #   learning_rate = c(.009, .03, .09, .3, .9, NA),
  #   subsample = c(.2, .5, .8, 1),
  #   depth = c(1, 2, 4, 6, 8, 10)
  # ),
  # list)),
  # steps = c(200, 400, 600, 800, 1000)),
  # xgboost = list(tibble(grid_row = pmap(expand_grid(
  #   eta = c(.009, .03, .09, .3, .9),
  #   subsample = c(.2, .5, .8, 1),
  #   max_depth = c(1, 2, 4, 6, 8, 10)
  # ),
  # list)),
  # steps = c(200, 400, 600, 800, 1000)),
  # elastic_net = list(tibble(grid_row = pmap(bind_rows(expand_grid(
  #   alpha = c(0, .15, .3, .5, .7, .85, 1),
  #   lambda = c(NA)
  # ),
  # tibble(alpha = 1, lambda = 0)),
  # list)),
  # steps = c(1, 2, 3, 4, 5))
  )
n_grid_row <- 1 # how many hyperparameter combinations to sample from expanded
# grid?

# Performance metrics
metrics_for_all_pipelines <- c("LogLoss", "MSE", "In_Sample_R2", "R2_Holdout", "AUC")
metrics_for_selecting_pipelines <- c("LogLoss")
metrics_for_winning_pipelines <- c("F1_Score", "Accuracy")
threshold_increment <- .01
n_bootstrap <- 2

save_only_winning_hyperparameter_draw_results <- FALSE
results_path <- "results_subgroups_2024-12-30"