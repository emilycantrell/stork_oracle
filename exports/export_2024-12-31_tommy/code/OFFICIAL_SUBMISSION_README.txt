STORK ORACLE FINAL SUBMISSION OVERVIEW 

This is a catboost model with default hyperparameters. The features used are: 
- train.csv: all substantive feature columns (we removed non-substantive columns like partner ID before training). 
- persoontab2020: all substantive columns.
- huishoudensbus2020: we filtered to the the current household (as of Dec. 31 2020) and included all substantive columns from that current household.
- familienetwerktab: we created a feature indicating the number of relations of each relation type (e.g., a feature indicating number of cousins), 
  and a feature counting the total number of family network relations across all relation types. We also counted number of sons, number of daughters, 
  number of stepsons, and number of stepdaughters (i.e., children & stepchildren counts differentiated by gender). 
- data on "live-in" partner: We identified people are are considered the partner of the ego in the current (Dec. 31 2020) household
  from huishoudensbus2020, regardless of whether they are married, registered, or unregistered partners. We created features indicating the age and
  sex of these partners, and the start date of the first household in which the ego and married/registered/unregistered partner lived together.

Our steps for generating predictions were as follows:
- Step 1: We generated a model on OSSC using the code in stork_oracle_with_live_in_partner_for_OSSC_2024-10-28_FINAL_PREFER_SUBMISSION.
- Step 2: We created a preprocessed dataframe for the holdout set using the code in code_to_create_preprocessed_holdout_data_for_final_submission_2024-10-28.
- Step 3: We generated predicted classes for the holdout via how_to_submit_R_stork_oracle_final_submission.
- Step 4: We generated predicted probabilities for the holdout via how_to_submit_R_stork_oracle_final_submission_predicted_probabilities.