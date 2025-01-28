# This file creates a data frame where each row defines a feature_set-sampling_
# file-training_set-model-hyperparameter_grid_row combination.

feature_set <- names(feature_set_settings)
training_set <- unique(data_splits$training_sets)
models <-names(model_settings)
data_settings_df <- expand_grid(feature_set, sampling_files, training_set, models) %>%
  rename(sampling_file = sampling_files, model = models)
model_settings_df <-map(models, ~mutate(model_settings[[.x]][[1]], model = .x)) %>%
  list_rbind()
grid_rows <- full_join(data_settings_df, model_settings_df, by = "model", relationship = "many-to-many") %>%
  group_by(feature_set, sampling_file, training_set, model) %>%
  slice_sample(n = n_grid_row) %>%
  ungroup()

rm(feature_set)
rm(training_set)
rm(models)
rm(data_settings_df)
rm(model_settings_df)