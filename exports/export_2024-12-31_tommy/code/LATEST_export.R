feature_importance_performance <- fread("H:/pmt/stork_oracle/stork_oracle_december_2024/from_OSSC/results_feature_importance_2024-12-17.csv")
ossc_topics <- fread("H:/pmt/stork_oracle/stork_oracle_december_2024/from_OSSC/results_topic_areas_2024-12-18.csv")
topics <- fread("H:/pmt/stork_oracle/stork_oracle_december_2024/results_100k_topics_2024-12-28/results_topics.csv")
AS_topics <- fread("H:/pmt/stork_oracle/stork_oracle_december_2024/results_100k_AS_plus_topics_2024-12-28/results_AS_plus_topics.csv")
AS_FS_topics <- fread("H:/pmt/stork_oracle/stork_oracle_december_2024/results_100k_AS_FS_plus_topics_2024-12-28/results_AS_FS_plus_topics.csv")
AS_FS_IN_topics <- fread("H:/pmt/stork_oracle/stork_oracle_december_2024/results_100k_AS_FS_IN_plus_topics_2024-12-28/results_AS_FS_IN_plus_topics.csv")
AS_FS_IN_ED_topics <- fread("H:/pmt/stork_oracle/stork_oracle_december_2024/results_100k_AS_FS_IN_ED_plus_topics_2024-12-28/results_AS_FS_IN_ED_plus_topics.csv")
AS_FS_IN_ED_IM_topics <- fread("H:/pmt/stork_oracle/stork_oracle_december_2024/results_100k_AS_FS_IN_ED_IM_plus_topics_2024-12-28/results_AS_FS_IN_ED_IM_plus_topics.csv")
AS_FS_IN_ED_IM_HO_topics <- fread("H:/pmt/stork_oracle/stork_oracle_december_2024/results_100k_AS_FS_IN_ED_IM_HO_plus_topics_2024-12-28/results_AS_FS_IN_ED_IM_HO_plus_topics.csv")
baseline <- fread("H:/pmt/stork_oracle/stork_oracle_final/training_mean/results_training_mean_2024-12-26.csv")

process <- function(df) {
  select(df, -ends_with(c("Precision", "Recall")))
}

feature_importance_performance <- process(feature_importance_performance)
fwrite(feature_importance_performance, "H:/pmt/stork_oracle/hren9469_export/feature_importance_performance.csv")

ossc_topics <- process(ossc_topics) %>%
  mutate(computing_environment = "OSSC")
heavy_server_topics <- bind_rows(topics, AS_topics, AS_FS_topics, AS_FS_IN_topics, AS_FS_IN_ED_topics, AS_FS_IN_ED_IM_topics, AS_FS_IN_ED_IM_HO_topics, baseline)%>%
  mutate(computing_environment = "Heavy Server")
topics_performance <- bind_rows(ossc_topics, heavy_server_topics)
fwrite(topics_performance, "H:/pmt/stork_oracle/hren9469_export/topics_performance.csv")

missingness <- fread("H:/pmt/stork_oracle/stork_oracle_december_2024/missingness.csv")
fwrite(missingness, "H:/pmt/stork_oracle/hren9469_export/missingness.csv")

feature_importance <- fread("H:/pmt/stork_oracle/stork_oracle_get_importance_2024-12-16/without_leakage_pmt_train_and_evaluation_samples_seed_1_241016.csv_training_set_catboost_learning_rate=NA,subsample=0.8,depth=6feature_importance")
feature_importance <- select(feature_importance, -V1)
fwrite(feature_importance, "H:/pmt/stork_oracle/hren9469_export/feature_importance.csv")
