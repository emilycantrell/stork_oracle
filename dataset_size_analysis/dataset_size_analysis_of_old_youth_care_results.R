library(groundhog)
packages <- c("tidyverse", "scales")
groundhog.library(packages, "2024-09-01")

prefer_results <- read_csv("/Users/ecantrell/Desktop/CBS\ Exports/sample_size_investigation.csv")
youth_care_results <- read_csv("/Users/ecantrell/Documents/Dutch_Registry/analysis_from_USA/sample_size_results_2024-03.csv")

# Restrict to sample sizes that we are already using in the fertility results
sample_sizes_in_prefer_results <- unique(prefer_results$n_training_set)
sample_sizes_to_use <- c(sample_sizes_in_prefer_results, max(youth_care_results$train_set_sample_size))

# Add a column indicating the feature set (even though there is just one for these outcomes now)
youth_care_results <- youth_care_results %>%
  mutate(feature_set = "demographic_and_youth_care")

# Set plot theme
theme_custom <- theme(
  plot.title = element_text(size = 14, face = "bold"),   
  plot.subtitle = element_text(size = 12),                
  axis.title = element_text(size = 12),                  
  legend.title = element_text(size = 12, face = "bold"),              
  legend.text = element_text(size = 10, margin = margin(b = 12)),         
  legend.position = "right",                              
)

# PLOT FOR YOUTH CARE SERVICES USAGE
youth_care_plot <- youth_care_results %>%
  filter(outcome_name == "jeugd_gebruik") %>%
  filter(train_set_sample_size %in% sample_sizes_to_use,
         train_set_sample_size > 100) %>%
  ggplot(aes(x = train_set_sample_size, y = rmse_results, color = feature_set)) + 
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +  # Use scales to format x-axis
  geom_point() +
  geom_line() + 
  theme_bw() + 
  theme_custom + 
  ggtitle("Relationship Between Training Set Sample Size and RMSE") + 
  labs(
    color = "Feature Set",  # Legend title
    subtitle = "Outcome: Use of any Youth Care services in a 1-year period"
  ) + 
  scale_color_discrete(
    labels = c("demographic_and_youth_care" = "
Demographic + 
Prior Youth Care Data")
  ) + 
  xlab("Training Set Sample Size") + 
  ylab("RMSE") 

youth_care_plot
ggsave("youth_care_rmse_plot.png", youth_care_plot, width = 8, height = 6)


# PLOT FOR YOUTH PROTECTIVE SERVICES USAGE
protective_services_plot <- youth_care_results %>%
  filter(outcome_name == "jeugd_bescherming") %>%
  filter(train_set_sample_size %in% sample_sizes_to_use,
         train_set_sample_size > 100) %>%
  ggplot(aes(x = train_set_sample_size, y = rmse_results, color = feature_set)) + 
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +  # Use scales to format x-axis
  geom_point() +
  geom_line() + 
  theme_bw() + 
  theme_custom + 
  ggtitle("Relationship Between Training Set Sample Size and RMSE") + 
  labs(
    color = "Feature Set",  # Legend title
    subtitle = "Outcome: Use of youth protective services in a 1-year period"
  ) + 
  scale_color_discrete(
    labels = c("demographic_and_youth_care" = "
Demographic + 
Prior Youth Care Data")
  ) + 
  xlab("Training Set Sample Size") + 
  ylab("RMSE")

protective_services_plot
ggsave("protective_services_rmse_plot.png", protective_services_plot, width = 8, height = 6)


# PLOT FOR BOTH OUTCOMES
two_outcome_plot <- youth_care_results %>%
  filter(outcome_name %in% c("jeugd_gebruik", "jeugd_bescherming")) %>%
  filter(train_set_sample_size %in% sample_sizes_to_use,
         train_set_sample_size > 100) %>%
  ggplot(aes(x = train_set_sample_size, y = rmse_results, color = outcome_name)) + 
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +  # Use scales to format x-axis
  geom_point() +
  geom_line() + 
  theme_bw() + 
  theme_custom + 
  ggtitle("Relationship Between Training Set Sample Size and RMSE") + 
  labs(
    color = "Outcome",  # Legend title
  ) + 
  scale_color_discrete(
    labels = c("demographic_and_youth_care" = "
Demographic + 
Prior Youth Care Data")
  ) + 
  xlab("Training Set Sample Size") + 
  ylab("RMSE")

two_outcome_plot
# Note: it's too difficult to see the curves on this plot since the difference between 
# the predictability of each outcome is large, thus spacing out the y axis