feature_sets <- list(
  full = c("GBAPERSOONTAB", "GBAHUISHOUDENBUS", "prefer_official_train"),
  minimal_engineering = c("GBAPERSOONTAB", "GBAHUISHOUDENBUS"),
  sex_and_birthyear = c("sex_and_birthyear")
)

sampling_files <- c("pmt_train_and_evaluation_samples_seed_1.csv")

data_splits <- bind_rows(
  expand_grid( # Splits for studying sample size
    training_sets = c(
      "train_sample_n_100",
      "train_sample_n_500",
      "train_sample_n_1000",
      "train_sample_n_2000",
      "train_sample_n_3000",
      "train_sample_n_4000",
      "train_sample_n_5000",
      "train_sample_n_10000",
      "train_sample_n_100000",
      "train_sample_n_1000000",
      "train_sample_n_2000000",
      "train_sample_n_3000000",
      "training_set"
    ),
    selection_sets = c("eval_selection"),
    test_sets = c("eval_test")
  ),
  expand_grid( # Splits for studying gender
    training_sets = c("training_set",
      "training_set_female", "training_set_male"
    ),
    selection_sets = c("evaluation_set_female","evaluation_set_male"),
    test_sets = c()
  )
)

models <- list(
  list(
    name = "catboost",
    grid = expand_grid(
      learning_rate = c(.003, .03, .3, NA),
      subsampling = c(.2, .5, .8, 1),
      depth = c(1, 2, 4, 6, 8, 10)
    ),
    steps = c(100, 200, 300, 400, ...)
  ),
  xgboost = list(
    name = "xgboost",
    grid = expand_grid(
      eta = c(0.003, .03, .3),
      subsample = c(.2, .5, .8, 1),
      max_depth = c(1, 2, 4, 6, 8, 10),
    ),
    n_steps = 1000
  ),
  elastic_net = list(
    name = "elastic_net",
    grid = expand_grid(
      alpha = c(0, .15, .3, .5, .7, .85, 1),
      lambda = c(NA)
    ) %>%
      bind_rows(list(alpha = 1, lambda = 0))
  ),
  n_steps = 100
)

metrics_for_all_pipelines <- c("LogLoss", "MSE", "R2_Score", "AUC")
metrics_for_selecting_pipelines <- c("LogLoss")
metrics_for_winning_pipelines <- c("F1_Score")

n_bootstraps <- 1000

save_only_winning_hyperparameter_draw_results <- FALSE

results_path <- "results.csv"