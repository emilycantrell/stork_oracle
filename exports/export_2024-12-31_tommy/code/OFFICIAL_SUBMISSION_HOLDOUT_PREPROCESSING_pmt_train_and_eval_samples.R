library(data.table) # 1.15.4 on CBS server
library(tidyverse) # 2.0.0 on CBS server

train_path <- "H:/DATASETS/train.csv"
prefer_train_and_eval <- fread(train_path, colClasses = c(RINPERSOON = "character")) %>%
  filter(!is.na(children_post2021)) # Filter out people with missing outcomes

# NOTE: The official PreFer file, "train.csv", has a column "evaluation_set" 
# indicating 10% of cases that can be used as evaluation data. 
# Therefore, we will refer to the data from this file "prefer_train_and_eval". 
# We will use "train" to refer to the 90% of cases in train.csv that are not part of the eval set.

#### SELECT JUST THE COLUMNS WE NEED TO CREATE SAMPLES ####
prefer_train_and_eval <- prefer_train_and_eval %>%
  select(RINPERSOON, evaluation_set, GBAGESLACHT, GBAGEBOORTEJAAR, OPLNIVSOI2021AGG4HBmetNIRWO_isced) %>%
  mutate(gender_male = ifelse(GBAGESLACHT == 1, 1, 0),
         gender_female = ifelse(GBAGESLACHT == 2, 1, 0),
         birthyear_19751978 = ifelse(GBAGEBOORTEJAAR %in% 1975:1978, 1, 0),
         birthyear_19791982 = ifelse(GBAGEBOORTEJAAR %in% 1979:1982, 1, 0),
         birthyear_19831986 = ifelse(GBAGEBOORTEJAAR %in% 1983:1986, 1, 0),
         birthyear_19871990 = ifelse(GBAGEBOORTEJAAR %in% 1987:1990, 1, 0),
         birthyear_19911994 = ifelse(GBAGEBOORTEJAAR %in% 1991:1994, 1, 0),
         birthyear_19951998 = ifelse(GBAGEBOORTEJAAR %in% 1995:1998, 1, 0),
         birthyear_19992002 = ifelse(GBAGEBOORTEJAAR %in% 1999:2002, 1, 0),
         edu_preprimary = ifelse(is.na(OPLNIVSOI2021AGG4HBmetNIRWO_isced) | OPLNIVSOI2021AGG4HBmetNIRWO_isced != 0, 0, 1),
         edu_primary = ifelse(is.na(OPLNIVSOI2021AGG4HBmetNIRWO_isced) | OPLNIVSOI2021AGG4HBmetNIRWO_isced != 1, 0, 1),
         edu_lower_secondary = ifelse(is.na(OPLNIVSOI2021AGG4HBmetNIRWO_isced) | OPLNIVSOI2021AGG4HBmetNIRWO_isced != 2, 0, 1),
         edu_upper_secondary = ifelse(is.na(OPLNIVSOI2021AGG4HBmetNIRWO_isced) | OPLNIVSOI2021AGG4HBmetNIRWO_isced != 3, 0, 1),
         edu_post_secondary_non_tertiary = ifelse(is.na(OPLNIVSOI2021AGG4HBmetNIRWO_isced) | OPLNIVSOI2021AGG4HBmetNIRWO_isced != 4, 0, 1),
         edu_short_tertiary = ifelse(is.na(OPLNIVSOI2021AGG4HBmetNIRWO_isced) | OPLNIVSOI2021AGG4HBmetNIRWO_isced != 5, 0, 1),
         edu_bachelor = ifelse(is.na(OPLNIVSOI2021AGG4HBmetNIRWO_isced) | OPLNIVSOI2021AGG4HBmetNIRWO_isced != 6, 0, 1),
         edu_master = ifelse(is.na(OPLNIVSOI2021AGG4HBmetNIRWO_isced) | OPLNIVSOI2021AGG4HBmetNIRWO_isced != 7, 0, 1),
         edu_doctor = ifelse(is.na(OPLNIVSOI2021AGG4HBmetNIRWO_isced) | OPLNIVSOI2021AGG4HBmetNIRWO_isced != 8, 0, 1)
         ) %>%
  select(-GBAGESLACHT, -GBAGEBOORTEJAAR, -OPLNIVSOI2021AGG4HBmetNIRWO_isced)

#### FUNCTION TO GENERATE SAMPLES AND CREATE COLUMNS INDICATING WHO IS IN EACH SAMPLE ####
create_sample_indicators <- function(df, vector_of_sample_sizes, sample_from, group) {
  if(!is.null(group)) {
    sampling_df <- filter(df, !!sym(group) == 1)
  } else {
    sampling_df <- df
  }
  # Create prefix and filter the data to either the train or eval rows
  # and there is not need to worry about a 50% train set
  if(sample_from == "train") { 
    sample_prefix <- "train_sample_n_"
    sampling_df <- sampling_df %>% 
      filter(evaluation_set == 0)
    # n_training_set <- nrow(sampling_df)
    n_evaluation_set <- NA
    # n_evaluation_test_50_percent_split <- NA
    # vector_of_sample_sizes <- c(vector_of_sample_sizes, n_training_set)
  } else if (sample_from == "evaluation") { 
    sample_prefix <- "evaluation_sample_n_"
    sampling_df <- sampling_df %>% 
      filter(evaluation_set == 1)
    n_training_set <- NA
    n_evaluation_set <- nrow(sampling_df)
    # Calculate the size of the evaluation_test_50_percent_split set if we are in the evaluation set
    # n_evaluation_test_50_percent_split <- nrow(sampling_df) %/% 2
    vector_of_sample_sizes <- c(vector_of_sample_sizes, n_evaluation_set) # n_evaluation_test_50_percent_split
  } else { 
    stop("sample_from must be either `train` or `evaluation`")
  }

  # Put the sample sizes in decreasing order
  vector_of_sample_sizes <- sort(vector_of_sample_sizes, decreasing = TRUE)
  
  # Remove any sample sizes that are larger tha nthe sample size of the full data
  if (any(vector_of_sample_sizes > nrow(sampling_df))) {
    vector_of_sample_sizes <- vector_of_sample_sizes[vector_of_sample_sizes <= nrow(sampling_df)]
    warning("One or more sample sizes is larger than the available data, and has been automatically removed from the vector of sample sizes.")
  }
  
  # Take the largest sample, then take progressively smaller samples from within each previous sample
  # training_set_generated <- FALSE
  evaluation_set_generated <- FALSE
  # evaluation_test_50_percent_split_generated <- FALSE # This allows us to prevent bugs when, for example, n_eval_test
  # is in the pre-specified set of sample sizes
  for(n in vector_of_sample_sizes) { 
    # Create the new column name 
    # if (sample_from == "train" & n == n_training_set & !training_set_generated) {
    #   new_col <- "training_set"
    #   training_set_generated <- TRUE
    # } else if (sample_from == "evaluation" & n == n_evaluation_test_50_percent_split & !evaluation_test_50_percent_split_generated) {
    #   new_col <- "evaluation_test_50_percent_split"
    #   evaluation_test_50_percent_split_generated <- TRUE
    # } else 
      if (sample_from == "evaluation" & n == n_evaluation_set & !evaluation_set_generated) {
      new_col <- "evaluation_set"
      evaluation_set_generated <- TRUE
    } else {
      new_col <- paste0(sample_prefix, format(n, scientific = FALSE))
    }
    
    # Label group-specific columns in group-specific ways
    if (!is.null(group)) {
      new_col <- paste(group, new_col, sep = "_")
    }
    

    # Sample n rows from the previously sampled df
    sampling_df <- sampling_df %>%
      slice_sample(n = n) 
    # Create new column indicating which rows are part of the sample
    df <- df %>%
      mutate(!!new_col := ifelse(RINPERSOON %in% sampling_df$RINPERSOON, 1, 0))
  }
  
  # Generate column for eval_selection_50_percent_split if applicable
  # if (sample_from == "evaluation") {
  #   evaluation_selection_50_percent_split <- "evaluation_selection_50_percent_split"
  #   if (!is.null(group)) {
  #     evaluation_selection_50_percent_split <- paste(group, "evaluation_selection_50_percent_split", sep = "_")
  #   }
  #   df <- df %>%
  #     mutate(!!evaluation_selection_50_percent_split := ifelse(evaluation_set == 0, 0, 1 - evaluation_test_50_percent_split))
  # }
  
  return(df)
}

#### FUNCTION TO TEST THE OUTPUT OF CREATE_SAMPLE_INDICATORS ####
check_output_of_create_sample_indicators <- function(df) {
  # There should be no NAs in the file
  NA_values_exist <- any(is.na(df))
  if(NA_values_exist) { 
    stop("There are NA values in the dataframe.")
  } else { 
    print("Check passed: there are no NAs.")
  }
  
  # Other than the RINPERSOON column, all values should be either 0 or 1
  all_0_and_1 <- df %>%
    select(-RINPERSOON) %>%
    summarise(across(everything(), ~ all(. %in% c(0, 1)))) %>%
    unlist() %>%
    all()
  if(!all_0_and_1) {
    stop("Some values outside the RINPERSOON column are not 0 and 1.")
  } else {
    print("Check passed: all values other than RINPERSOON are 0/1")
  }
  
  # If the column name includes a sample size, then the sum of values matches the label 
  for(col in colnames(df)) {
    if(grepl("sample_n", col)) { 
      expected_sample_size <- as.numeric((sub(".*sample_n_", "", col)))
      actual_sum <- sum(df[[col]])
      if(actual_sum != expected_sample_size) { 
        stop(paste("Column", col, "has a sum of", actual_sum, "which is not the expected sample size."))
      } else { 
        print("Check passed: column sums match the expected sample sizes.")
      }
    }
  }
  
  # RINPERSOON values should all be unique
  all_RINPERSOON_values_are_unique <- length(unique(df$RINPERSOON)) == nrow(df)
  if(!all_RINPERSOON_values_are_unique) { 
    stop("RINPERSOON column contains duplicate values.")
  } else { 
    print("Check passed: all RINPERSOON values are unique.")
  }
  
  # # All cases in train_sample_n_* should have evaluation_set == 0 
  # train_sample_columns <- df %>%
  #   select(matches("^train_sample_n_"))
  # eval_rows <- df$evaluation_set == 1
  # train_samples_mistakenly_came_from_evaluation_rows <- any(train_sample_columns[eval_rows] == 1)
  # if(train_samples_mistakenly_came_from_evaluation_rows) { 
  #   stop("Some train samples came from the evaluation set.")
  # } else { 
  #   print("Check passed: All cases train samples come from the train set.")
  # }
  # 
  # # All cases in evaluation_sample_n_* should have evaluation_set == 1
  # eval_sample_columns <- df %>%
  #   select(matches("^evaluation_sample_n_"))
  # train_rows <- df$evaluation_set == 0
  # eval_samples_mistakenly_came_from_train_rows <- any(eval_sample_columns[train_rows] == 1)
  # if(eval_samples_mistakenly_came_from_train_rows) { 
  #   stop("Some evaluation samples came from the train set.")
  # } else { 
  #   print("Check passed: All cases in evaluation samples come from the evaluation set.")
  # }
  # 
  # # All cases in train_sample_n_[X] should have train_sample_n_[X+1] == 1 
  # # (where X + 1 is the next largest sample size)
  # train_col_names <- colnames(train_sample_columns)
  # for(i in 1:(length(train_col_names)-1)) { 
  #   larger_sample_column <- train_col_names[i]
  #   smaller_sample_column <- train_col_names[i+1]
  #   row_in_smaller_sample_but_not_in_larger_sample <- 
  #     any(df[[smaller_sample_column]] == 1 & 
  #           df[[larger_sample_column]] == 0)
  #   if(row_in_smaller_sample_but_not_in_larger_sample) { 
  #     stop("One or more rows in a smaller train sample is not nested within a larger train sample.")
  #   } else { 
  #     print("Check passed: train samples are properly nested.")  
  #   }
  # }
  # 
  # # # All cases in evaluation_sample_n_[X] should have evaluation_sample_n_[X+1] == 1 
  # # # (where X + 1 is the next largest sample size)
  # # eval_sample_columns_and_test_split_column <- df %>%
  # #   select(matches("^evaluation_sample_n_|evaluation_test_50_percent_split"))
  # # eval_sample_columns_and_test_split_column_names <- colnames(eval_sample_columns_and_test_split_column)
  # # for(i in 1:(length(eval_sample_columns_and_test_split_column_names)-1)) { 
  # #   larger_sample_column <- eval_sample_columns_and_test_split_column_names[i]
  # #   smaller_sample_column <- eval_sample_columns_and_test_split_column_names[i+1]
  # #   row_in_smaller_sample_but_not_in_larger_sample <- 
  # #     any(df[[smaller_sample_column]] == 1 & 
  # #           df[[larger_sample_column]] == 0)
  # #   if(row_in_smaller_sample_but_not_in_larger_sample) { 
  # #     stop("One or more rows in a smaller evaluation sample is not nested within a larger evaluation sample.")
  # #   } else { 
  # #     print("Check passed: evaluation samples are properly nested.")
  # #   }
  # # }
  # 
  # # Check that selection and test splits are a partition of the evaluation set,
  # # by testing that they always sum to 1 for rows where evaluation_set == 1
  # row_sum_of_selection_and_test_set_indicators_within_eval_set <- df %>%
  #   filter(evaluation_set == 1) %>%
  #   mutate(row_sum_of_selection_and_test_set_indicators = evaluation_selection_50_percent_split + evaluation_test_50_percent_split) %>%
  #   pull(row_sum_of_selection_and_test_set_indicators)
  # if(!all(row_sum_of_selection_and_test_set_indicators_within_eval_set == 1)) {
  #   stop("Selection and test columns do not sum to 1 for all rows. They are not a partition of the evaluation set.") 
  # } else {
  #   print("Check passed: Selection and test sets are a partition of the evaluation set.")
  # }
  
  # # Check that within the training set, evaluation_selection_50_percent_split and evaluation_test_50_percent_split are always 0
  # row_sum_of_selection_and_test_set_indicators_within_train_set <- df %>%
  #   filter(evaluation_set == 0) %>%
  #   mutate(row_sum_of_selection_and_test_set_indicators = evaluation_selection_50_percent_split + evaluation_test_50_percent_split) %>%
  #   pull(row_sum_of_selection_and_test_set_indicators)
  # if(!all(row_sum_of_selection_and_test_set_indicators_within_train_set == 0)) {
  #   stop("Some rows sampled for the selection or test set(s) come from the training set") 
  # } else {
  #   print("Check passed: Rows sampled for the selection and test sets never come from the training set.")
  # }
  # 
  # # Check that the sizes of the selection set and test set differ by no more than 1 row
  # # (their size could differ by 1 row if the evaluation set has an odd number of rows)
  # selection_set_size <- sum(df$evaluation_selection_50_percent_split)
  # test_set_size <- sum(df$evaluation_test_50_percent_split)
  # if(abs(selection_set_size - test_set_size) > 1) {
  #   stop("The sizes of selection set and test set differ by more than 1 row.")
  # } else {
  #   print("Check passed: The sizes of selection set and test set differ by 1 row or less.")
  # }
  
}

#### FUNCTION TO EDIT, TEST, AND SAVE THE SAMPLING DATAFRAME FOR A GIVEN SEED ####
edit_and_save_sample_df <- function(seed) { 
  set.seed(seed)
  
  # Edit and test the dataframe
  output <- create_sample_indicators(df = prefer_train_and_eval,
                                     vector_of_sample_sizes = train_sample_sizes,
                                     sample_from = "train",
                                     group = NULL)
  output <- create_sample_indicators(df = output,
                                     vector_of_sample_sizes = eval_sample_sizes,
                                     sample_from = "evaluation",
                                     group = NULL)
  
  
  output <- create_sample_indicators(df = output,
                                     vector_of_sample_sizes = c(),
                                     sample_from = "evaluation",
                                     group = "gender_male")
  output <- create_sample_indicators(df = output,
                                     vector_of_sample_sizes = c(),
                                     sample_from = "evaluation",
                                     group = "gender_female")
  output <- create_sample_indicators(df = output,
                                     vector_of_sample_sizes = c(),
                                     sample_from = "evaluation",
                                     group = "birthyear_19751978")
  output <- create_sample_indicators(df = output,
                                     vector_of_sample_sizes = c(),
                                     sample_from = "evaluation",
                                     group = "birthyear_19791982")
  output <- create_sample_indicators(df = output,
                                     vector_of_sample_sizes = c(),
                                     sample_from = "evaluation",
                                     group = "birthyear_19831986")
  output <- create_sample_indicators(df = output,
                                     vector_of_sample_sizes = c(),
                                     sample_from = "evaluation",
                                     group = "birthyear_19871990")
  output <- create_sample_indicators(df = output,
                                     vector_of_sample_sizes = c(),
                                     sample_from = "evaluation",
                                     group = "birthyear_19911994")
  output <- create_sample_indicators(df = output,
                                     vector_of_sample_sizes = c(),
                                     sample_from = "evaluation",
                                     group = "birthyear_19951998")
  output <- create_sample_indicators(df = output,
                                     vector_of_sample_sizes = c(),
                                     sample_from = "evaluation",
                                     group = "birthyear_19992002")
  output <- create_sample_indicators(df = output,
                                     vector_of_sample_sizes = c(),
                                     sample_from = "evaluation",
                                     group = "edu_preprimary")
  output <- create_sample_indicators(df = output,
                                     vector_of_sample_sizes = c(),
                                     sample_from = "evaluation",
                                     group = "edu_primary")
  output <- create_sample_indicators(df = output,
                                     vector_of_sample_sizes = c(),
                                     sample_from = "evaluation",
                                     group = "edu_lower_secondary")
  output <- create_sample_indicators(df = output,
                                     vector_of_sample_sizes = c(),
                                     sample_from = "evaluation",
                                     group = "edu_upper_secondary")
  output <- create_sample_indicators(df = output,
                                     vector_of_sample_sizes = c(),
                                     sample_from = "evaluation",
                                     group = "edu_post_secondary_non_tertiary")
  output <- create_sample_indicators(df = output,
                                     vector_of_sample_sizes = c(),
                                     sample_from = "evaluation",
                                     group = "edu_short_tertiary")
  output <- create_sample_indicators(df = output,
                                     vector_of_sample_sizes = c(),
                                     sample_from = "evaluation",
                                     group = "edu_bachelor")
  output <- create_sample_indicators(df = output,
                                     vector_of_sample_sizes = c(),
                                     sample_from = "evaluation",
                                     group = "edu_master")
  output <- create_sample_indicators(df = output,
                                     vector_of_sample_sizes = c(),
                                     sample_from = "evaluation",
                                     group = "edu_doctor")
  output <- select(output, -gender_male, -gender_female,
                   -birthyear_19751978, -birthyear_19791982, -birthyear_19831986, -birthyear_19871990, -birthyear_19911994, -birthyear_19951998, -birthyear_19992002,
                   -edu_preprimary, -edu_primary, -edu_lower_secondary, -edu_upper_secondary, -edu_post_secondary_non_tertiary, -edu_short_tertiary, -edu_bachelor, -edu_master, -edu_doctor)
  check_output_of_create_sample_indicators(output)
  
  # Save results 
  file_name <- paste0("pmt_train_and_evaluation_samples_seed_", seed, ".csv")
  pmt_samples_path_to_write <- paste0("H:/pmt/eval/train_and_eval_samples/", file_name)
  fwrite(output, pmt_samples_path_to_write)
  print(paste0("Data file with samples generated from seed ", seed, " has been saved."))
}

#### VECTORS LISTING THE DESIRED SAMPLE SIZES ####
train_sample_sizes <- c()

eval_sample_sizes <- c()

#### RUN THE FINAL FUNCTION WITH DIFFERENT SEEDS ####
seeds <- c(1)
map(seeds, edit_and_save_sample_df)
