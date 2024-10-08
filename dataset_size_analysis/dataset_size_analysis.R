library(groundhog)
packages <- c("tidyverse", "scales", "rlang")
groundhog.library(packages, "2024-09-01")

results <- read_csv("/Users/ecantrell/Desktop/CBS\ Exports/sample_size_investigation.csv")

# Translate MSE to RMSE
# Note: confidence intervals were calculated via bootstrap; see run_evaluation_set.R
results <- results %>%  
  mutate(estimate_RMSE = sqrt(estimate_MSE), 
         ci_lower_RMSE = sqrt(ci_lower_MSE), 
         ci_upper_RMSE = sqrt(ci_upper_MSE))

# Set plot theme
theme_custom <- theme(
  plot.title = element_text(size = 14, face = "bold"),   
  plot.subtitle = element_text(size = 12),                
  axis.title = element_text(size = 12),                  
  legend.title = element_text(size = 12, face = "bold"),              
  legend.text = element_text(size = 10, margin = margin(b = 12)),         
  legend.position = "right"                              
)

# Create function to generate plot
plot_metric <- function(data, metric_name) {
  metric_labels <- c(
    "LogLoss" = "Log Loss",
    "MSE" = "MSE",
    "R2_Score" = expression(R^2 ~ "Score"),  
    "F1_Score" = "F1 Score",
    "RMSE" = "RMSE"
  )
  
  estimate_col <- sym(paste0("estimate_", metric_name))
  ci_lower_col <- sym(paste0("ci_lower_", metric_name))
  ci_upper_col <- sym(paste0("ci_upper_", metric_name))
  y_label <- metric_labels[[metric_name]]
  
  plot_title <- if (metric_name == "R2_Score") {
    bquote("Relationship Between Training Set Sample Size and" ~ R^2 ~ "Score")
  } else {
    paste("Relationship Between Training Set Sample Size and", metric_labels[[metric_name]])
  }
  
  plot <- data %>%
    filter(evaluation_set == "evaluation_test_50_percent_split") %>%
    filter(n_training_set > 100) %>% # TODO: Consider adding smaller sample sizes back in after tuning is appropriate for these sample sizes
    ggplot(aes(x = n_training_set, y = !!estimate_col, color = feature_set, group = feature_set)) + 
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = !!ci_lower_col, ymax = !!ci_upper_col), width = 0.2) +  # Add confidence intervals
    scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +  # Use scales to format x-axis
    theme_bw() + 
    ylab(y_label) + 
    xlab("Training Set Sample Size") + 
    ggtitle(plot_title) +
    labs(
      color = "Feature Set",  # Legend title
      subtitle = "Outcome: Fertility (birth of new child in 3-year period)"
    ) +
    scale_color_discrete(
      labels = c(
        "sex_and_birthyear" = "
Sex and Birthyear Data",
        "persoontab" = "
Demographic Data File",
        "key_records" = "
Demographic + 
Household Data Files",
        "augmented_records" = "
Demographic + 
Household + 
Customized Data Files"
      )
    ) +
    theme_custom  # Apply the custom theme
  
  print(plot)
}

# Generate the plots
plot_metric(results, "LogLoss")
plot_metric(results, "MSE")
plot_metric(results, "R2_Score")
plot_metric(results, "F1_Score")
plot_metric(results, "RMSE")

# Save plot
rmse_plot <- plot_metric(results, "RMSE")
ggsave("fertility_rmse_plot.png", rmse_plot, width = 8, height = 6)

# Plot gaps between features sets 
results %>% 
  filter(n_training_set == 1000) %>%
  filter(evaluation_set == "evaluation_test_50_percent_split") %>%
  select(feature_set, estimate_RMSE) %>%
  View()

results %>% 
  filter(n_training_set == max(n_training_set)) %>%
  filter(n_training_set > 100) %>%
  filter(evaluation_set == "evaluation_test_50_percent_split") %>%
  select(feature_set, estimate_RMSE) %>%
  View()

results %>% 
  filter(feature_set == "persoontab") %>%
  filter(n_training_set > 100) %>%
  filter(evaluation_set == "evaluation_test_50_percent_split") %>%
  select(feature_set, n_training_set, estimate_RMSE) %>%
  View()

results %>% 
  filter(feature_set == "augmented_records") %>%
  filter(n_training_set > 100) %>%
  filter(evaluation_set == "evaluation_test_50_percent_split") %>%
  select(feature_set, n_training_set, estimate_RMSE) %>%
  View()

results %>% 
  filter(feature_set == "persoontab") %>%
  filter(evaluation_set == "evaluation_test_50_percent_split") %>%
  filter(n_training_set > 100) %>%
  summarise(mean_rsme_for_demographic_data = mean(estimate_RMSE))

results %>% 
  filter(feature_set == "key_records") %>%
  filter(evaluation_set == "evaluation_test_50_percent_split") %>%
  filter(n_training_set > 100) %>%
  summarise(mean_rsme_for_demographic_and_household_data = mean(estimate_RMSE))

results %>% 
  filter(feature_set == "augmented_records") %>%
  filter(evaluation_set == "evaluation_test_50_percent_split") %>%
  filter(n_training_set > 100) %>%
  summarise(mean_rsme_for_all_features = mean(estimate_RMSE))

results %>% 
  filter(feature_set == "persoontab") %>%
  filter(evaluation_set == "evaluation_test_50_percent_split") %>%
  filter(n_training_set > 100) %>%
  select(feature_set, n_training_set, estimate_RMSE) %>% 
  View()

results %>% 
  filter(feature_set == "augmented_records") %>%
  filter(evaluation_set == "evaluation_test_50_percent_split") %>%
  filter(n_training_set > 100) %>%
  select(feature_set, n_training_set, estimate_RMSE) %>% 
  View()
