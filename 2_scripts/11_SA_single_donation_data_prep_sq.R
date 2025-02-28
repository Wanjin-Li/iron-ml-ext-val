# This script prepares data for the sensitivity analysis, 11_SA_rise_internal_validation_single_donor.R
# by randomly selecting a single donation from donors having >1 donations (NL only)

library(data.table)
library(rsample)



intermediate_directory <- './3_intermediate/sensitivity_analysis'  # directory to store external validation results
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}

intermediate_directory <- './3_intermediate/sensitivity_analysis/private'  # directory to store external validation results
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}

intermediate_directory <- './3_intermediate/sensitivity_analysis/private/single_donation_data'  # directory to store external validation results for sanbs
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}

intermediate_directory <- './3_intermediate/sensitivity_analysis/private/single_donation_data/sanquin'  # directory to store external validation results for sanbs
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}


# Extract original test data to randomly select a single donation

test_sq_hf <- fread("./path/private/hgb_ferr_sanquin.csv") # replace with the correct path
test_sq_h <- fread("./path/private/hgb_only_sanquin.csv")

# Randomly select a single donation from donors having more than 1 donation and keep the single donation among those having only 1 donation

single_don_test_sq_hf <- test_sq_hf[, .SD[sample(.N, 1)], by="DonorID"]
single_don_test_sq_h <- test_sq_h[, .SD[sample(.N, 1)], by="DonorID"]

# Save files
# No need to differentiate prediction outcome or data type  for external cohorts
save_dir <- "./3_intermediate/sensitivity_analysis/private/single_donation_data/"

cohort <- "sanquin/"
fwrite(single_don_test_sq_hf, paste0(save_dir, cohort, "hgb_ferr_sanquin.csv"))
fwrite(single_don_test_sq_h, paste0(save_dir, cohort, "hgb_only_sanquin.csv"))
