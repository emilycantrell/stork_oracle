# This file creates a very basic fake data file with the same fake RINPERSOONs
# as the fake sampling files.

library(tidyverse)
library(data.table)

sampling_file_path <- "~/Documents/GitHub/stork_oracle_cbs/fake_data_for_code_testing/pmt_train_and_evaluation_samples_seed_1.csv"
sampling_file <- fread(sampling_file_path, colClasses = c(RINPERSOON = "character")) 

# Get the vector of fake rinpersoon
RINPERSOON <- sampling_file$RINPERSOON
rm(sampling_file)

# Generate a file with RINPERSOON as the first column, and 10 fake variable columns
fake_data_file <- data.frame(RINPERSOON = RINPERSOON)
n <- nrow(fake_data_file)
for (i in 1:25) {
  fake_data_file[[paste0("variable", i)]] <- sample(0:1, n, replace = TRUE)
}

# Save the file
fake_data_file_path_to_write <- "~/Documents/GitHub/stork_oracle_cbs/fake_data_for_code_testing/fake_data_file.csv"
fwrite(fake_data_file, fake_data_file_path_to_write)

