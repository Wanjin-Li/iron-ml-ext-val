# This script prepares data for the sensitivity analysis, 11_SA_rise_internal_validation_single_donor.R
# by randomly selecting a single donation from donors having >1 donations (RISE, SA, and US)

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

intermediate_directory <- './3_intermediate/sensitivity_analysis/private/single_donation_data/rise'  # directory to store external validation results for sanbs
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}

intermediate_directory <- './3_intermediate/sensitivity_analysis/private/single_donation_data/rise/outer_fold_test_data'  # directory to store external validation results for sanbs
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}

intermediate_directory <- './3_intermediate/sensitivity_analysis/private/single_donation_data/rise/outer_fold_test_data/pred_hgb'  # directory to store external validation results for sanbs
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}

intermediate_directory <- './3_intermediate/sensitivity_analysis/private/single_donation_data/rise/outer_fold_test_data/pred_ferr'  # directory to store external validation results for sanbs
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}

intermediate_directory <- './3_intermediate/sensitivity_analysis/private/single_donation_data/sanbs'  # directory to store external validation results for sanbs
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}

intermediate_directory <- './3_intermediate/sensitivity_analysis/private/single_donation_data/vitalant'  # directory to store external validation results for sanbs
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}

intermediate_directory <- './3_intermediate/sensitivity_analysis/private/single_donation_data/sanquin'  # directory to store external validation results for sanbs
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}



# Extract original test outer fold data to randomly select a single donation

# Ferritin prediction
test_factors_r_hf_f_outer1 <- fread("./3_intermediate/private/outer_fold_test_data/pred_ferr/test_factors_hgb_ferr_outer_fold_1.csv")
test_factors_r_hf_f_outer2 <- fread("./3_intermediate/private/outer_fold_test_data/pred_ferr/test_factors_hgb_ferr_outer_fold_2.csv")
test_factors_r_hf_f_outer3 <- fread("./3_intermediate/private/outer_fold_test_data/pred_ferr/test_factors_hgb_ferr_outer_fold_3.csv")
test_factors_r_h_f_outer1 <- fread("./3_intermediate/private/outer_fold_test_data/pred_ferr/test_factors_hgb_only_outer_fold_1.csv")
test_factors_r_h_f_outer2 <- fread("./3_intermediate/private/outer_fold_test_data/pred_ferr/test_factors_hgb_only_outer_fold_2.csv")
test_factors_r_h_f_outer3 <- fread("./3_intermediate/private/outer_fold_test_data/pred_ferr/test_factors_hgb_only_outer_fold_3.csv")

test_OH_r_hf_f_outer1 <- fread("./3_intermediate/private/outer_fold_test_data/pred_ferr/test_OH_hgb_ferr_outer_fold_1.csv")
test_OH_r_hf_f_outer2 <- fread("./3_intermediate/private/outer_fold_test_data/pred_ferr/test_OH_hgb_ferr_outer_fold_2.csv")
test_OH_r_hf_f_outer3 <- fread("./3_intermediate/private/outer_fold_test_data/pred_ferr/test_OH_hgb_ferr_outer_fold_3.csv")
test_OH_r_h_f_outer1 <- fread("./3_intermediate/private/outer_fold_test_data/pred_ferr/test_OH_hgb_only_outer_fold_1.csv")
test_OH_r_h_f_outer2 <- fread("./3_intermediate/private/outer_fold_test_data/pred_ferr/test_OH_hgb_only_outer_fold_2.csv")
test_OH_r_h_f_outer3 <- fread("./3_intermediate/private/outer_fold_test_data/pred_ferr/test_OH_hgb_only_outer_fold_3.csv")

# Hemoglobin prediction
test_factors_r_hf_h_outer1 <- fread("./3_intermediate/private/outer_fold_test_data/pred_hgb/test_factors_hgb_ferr_outer_fold_1.csv")
test_factors_r_hf_h_outer2 <- fread("./3_intermediate/private/outer_fold_test_data/pred_hgb/test_factors_hgb_ferr_outer_fold_2.csv")
test_factors_r_hf_h_outer3 <- fread("./3_intermediate/private/outer_fold_test_data/pred_hgb/test_factors_hgb_ferr_outer_fold_3.csv")
test_factors_r_h_h_outer1 <- fread("./3_intermediate/private/outer_fold_test_data/pred_hgb/test_factors_hgb_only_outer_fold_1.csv")
test_factors_r_h_h_outer2 <- fread("./3_intermediate/private/outer_fold_test_data/pred_hgb/test_factors_hgb_only_outer_fold_2.csv")
test_factors_r_h_h_outer3 <- fread("./3_intermediate/private/outer_fold_test_data/pred_hgb/test_factors_hgb_only_outer_fold_3.csv")

test_OH_r_hf_h_outer1 <- fread("./3_intermediate/private/outer_fold_test_data/pred_hgb/test_OH_hgb_ferr_outer_fold_1.csv")
test_OH_r_hf_h_outer2 <- fread("./3_intermediate/private/outer_fold_test_data/pred_hgb/test_OH_hgb_ferr_outer_fold_2.csv")
test_OH_r_hf_h_outer3 <- fread("./3_intermediate/private/outer_fold_test_data/pred_hgb/test_OH_hgb_ferr_outer_fold_3.csv")
test_OH_r_h_h_outer1 <- fread("./3_intermediate/private/outer_fold_test_data/pred_hgb/test_OH_hgb_only_outer_fold_1.csv")
test_OH_r_h_h_outer2 <- fread("./3_intermediate/private/outer_fold_test_data/pred_hgb/test_OH_hgb_only_outer_fold_2.csv")
test_OH_r_h_h_outer3 <- fread("./3_intermediate/private/outer_fold_test_data/pred_hgb/test_OH_hgb_only_outer_fold_3.csv")

# No need to differentiate prediction outcome for external cohorts
test_s_hf <- fread("./3_intermediate/private/hgb_ferr_sanbs.csv")
test_s_h <- fread("./3_intermediate/private/hgb_only_sanbs.csv")

test_v_hf <- fread("./3_intermediate/private/vitalant_updates/hgb_ferr_vitalant.csv")
test_v_h <- fread("./3_intermediate/private/vitalant_updates/hgb_only_vitalant.csv")

# test_sq_hf <- fread("./3_intermediate/private/hgb_ferr_sanquin.csv")
# test_sq_h <- fread("./3_intermediate/private/hgb_only_sanquin.csv")


# Randomly select a single donation from donors having more than 1 donation and keep the single donation among those having only 1 donation

# Predict ferritin 

single_don_test_factors_r_hf_f_outer1 <- test_factors_r_hf_f_outer1[, .SD[sample(.N, 1)], by="RandID"]
single_don_test_factors_r_hf_f_outer2 <- test_factors_r_hf_f_outer2[, .SD[sample(.N, 1)], by="RandID"]
single_don_test_factors_r_hf_f_outer3 <- test_factors_r_hf_f_outer3[, .SD[sample(.N, 1)], by="RandID"]
single_don_test_factors_r_h_f_outer1 <- test_factors_r_h_f_outer1[, .SD[sample(.N, 1)], by="RandID"]
single_don_test_factors_r_h_f_outer2 <- test_factors_r_h_f_outer2[, .SD[sample(.N, 1)], by="RandID"]
single_don_test_factors_r_h_f_outer3 <- test_factors_r_h_f_outer3[, .SD[sample(.N, 1)], by="RandID"]

single_don_test_OH_r_hf_f_outer1 <- test_OH_r_hf_f_outer1[, .SD[sample(.N, 1)], by="RandID"]
single_don_test_OH_r_hf_f_outer2 <- test_OH_r_hf_f_outer2[, .SD[sample(.N, 1)], by="RandID"]
single_don_test_OH_r_hf_f_outer3 <- test_OH_r_hf_f_outer3[, .SD[sample(.N, 1)], by="RandID"]
single_don_test_OH_r_h_f_outer1 <- test_OH_r_h_f_outer1[, .SD[sample(.N, 1)], by="RandID"]
single_don_test_OH_r_h_f_outer2 <- test_OH_r_h_f_outer2[, .SD[sample(.N, 1)], by="RandID"]
single_don_test_OH_r_h_f_outer3 <- test_OH_r_h_f_outer3[, .SD[sample(.N, 1)], by="RandID"]


# Predict hemoglobin 
single_don_test_factors_r_hf_h_outer1 <- test_factors_r_hf_h_outer1[, .SD[sample(.N, 1)], by="RandID"]
single_don_test_factors_r_hf_h_outer2 <- test_factors_r_hf_h_outer2[, .SD[sample(.N, 1)], by="RandID"]
single_don_test_factors_r_hf_h_outer3 <- test_factors_r_hf_h_outer3[, .SD[sample(.N, 1)], by="RandID"]
single_don_test_factors_r_h_h_outer1 <- test_factors_r_h_h_outer1[, .SD[sample(.N, 1)], by="RandID"]
single_don_test_factors_r_h_h_outer2 <- test_factors_r_h_h_outer2[, .SD[sample(.N, 1)], by="RandID"]
single_don_test_factors_r_h_h_outer3 <- test_factors_r_h_h_outer3[, .SD[sample(.N, 1)], by="RandID"]

single_don_test_OH_r_hf_h_outer1 <- test_OH_r_hf_h_outer1[, .SD[sample(.N, 1)], by="RandID"]
single_don_test_OH_r_hf_h_outer2 <- test_OH_r_hf_h_outer2[, .SD[sample(.N, 1)], by="RandID"]
single_don_test_OH_r_hf_h_outer3 <- test_OH_r_hf_h_outer3[, .SD[sample(.N, 1)], by="RandID"]
single_don_test_OH_r_h_h_outer1 <- test_OH_r_h_h_outer1[, .SD[sample(.N, 1)], by="RandID"]
single_don_test_OH_r_h_h_outer2 <- test_OH_r_h_h_outer2[, .SD[sample(.N, 1)], by="RandID"]
single_don_test_OH_r_h_h_outer3 <- test_OH_r_h_h_outer3[, .SD[sample(.N, 1)], by="RandID"]


# No need to differentiate prediction outcome or data type  for external cohorts
single_don_test_s_hf <- test_s_hf[, .SD[sample(.N, 1)], by="DonorID"]
single_don_test_s_h <- test_s_h[, .SD[sample(.N, 1)], by="DonorID"]

single_don_test_v_hf <- test_v_hf[, .SD[sample(.N, 1)], by="DonorID"]
single_don_test_v_h <- test_v_h[, .SD[sample(.N, 1)], by="DonorID"]

# single_don_test_sq_hf <- test_sq_hf[, .SD[sample(.N, 1)], by="DonorID"]
# single_don_test_sq_h <- test_sq_h[, .SD[sample(.N, 1)], by="DonorID"]


# Save files
save_dir <- "./3_intermediate/private/single_donation_data/"
cohort <- "rise/outer_fold_test_data/"

# Predict ferritin
fwrite(single_don_test_factors_r_hf_f_outer1, paste0(save_dir, cohort, "pred_ferr/test_factors_hgb_ferr_outer_fold_1.csv"))
fwrite(single_don_test_factors_r_hf_f_outer2, paste0(save_dir, cohort, "pred_ferr/test_factors_hgb_ferr_outer_fold_2.csv"))
fwrite(single_don_test_factors_r_hf_f_outer3, paste0(save_dir, cohort, "pred_ferr/test_factors_hgb_ferr_outer_fold_3.csv"))
fwrite(single_don_test_factors_r_h_f_outer1, paste0(save_dir, cohort, "pred_ferr/test_factors_hgb_only_outer_fold_1.csv"))
fwrite(single_don_test_factors_r_h_f_outer2, paste0(save_dir, cohort, "pred_ferr/test_factors_hgb_only_outer_fold_2.csv"))
fwrite(single_don_test_factors_r_h_f_outer3, paste0(save_dir, cohort, "pred_ferr/test_factors_hgb_only_outer_fold_3.csv"))

fwrite(single_don_test_OH_r_hf_f_outer1, paste0(save_dir, cohort, "pred_ferr/test_OH_hgb_ferr_outer_fold_1.csv"))
fwrite(single_don_test_OH_r_hf_f_outer2, paste0(save_dir, cohort, "pred_ferr/test_OH_hgb_ferr_outer_fold_2.csv"))
fwrite(single_don_test_OH_r_hf_f_outer3, paste0(save_dir, cohort, "pred_ferr/test_OH_hgb_ferr_outer_fold_3.csv"))
fwrite(single_don_test_OH_r_h_f_outer1, paste0(save_dir, cohort, "pred_ferr/test_OH_hgb_only_outer_fold_1.csv"))
fwrite(single_don_test_OH_r_h_f_outer2, paste0(save_dir, cohort, "pred_ferr/test_OH_hgb_only_outer_fold_2.csv"))
fwrite(single_don_test_OH_r_h_f_outer3, paste0(save_dir, cohort, "pred_ferr/test_OH_hgb_only_outer_fold_3.csv"))


# Predict hemoglobin
fwrite(single_don_test_factors_r_hf_h_outer1, paste0(save_dir, cohort, "pred_hgb/test_factors_hgb_ferr_outer_fold_1.csv"))
fwrite(single_don_test_factors_r_hf_h_outer2, paste0(save_dir, cohort, "pred_hgb/test_factors_hgb_ferr_outer_fold_2.csv"))
fwrite(single_don_test_factors_r_hf_h_outer3, paste0(save_dir, cohort, "pred_hgb/test_factors_hgb_ferr_outer_fold_3.csv"))
fwrite(single_don_test_factors_r_h_h_outer1, paste0(save_dir, cohort, "pred_hgb/test_factors_hgb_only_outer_fold_1.csv"))
fwrite(single_don_test_factors_r_h_h_outer2, paste0(save_dir, cohort, "pred_hgb/test_factors_hgb_only_outer_fold_2.csv"))
fwrite(single_don_test_factors_r_h_h_outer3, paste0(save_dir, cohort, "pred_hgb/test_factors_hgb_only_outer_fold_3.csv"))

fwrite(single_don_test_OH_r_hf_h_outer1, paste0(save_dir, cohort, "pred_hgb/test_OH_hgb_ferr_outer_fold_1.csv"))
fwrite(single_don_test_OH_r_hf_h_outer2, paste0(save_dir, cohort, "pred_hgb/test_OH_hgb_ferr_outer_fold_2.csv"))
fwrite(single_don_test_OH_r_hf_h_outer3, paste0(save_dir, cohort, "pred_hgb/test_OH_hgb_ferr_outer_fold_3.csv"))
fwrite(single_don_test_OH_r_h_h_outer1, paste0(save_dir, cohort, "pred_hgb/test_OH_hgb_only_outer_fold_1.csv"))
fwrite(single_don_test_OH_r_h_h_outer2, paste0(save_dir, cohort, "pred_hgb/test_OH_hgb_only_outer_fold_2.csv"))
fwrite(single_don_test_OH_r_h_h_outer3, paste0(save_dir, cohort, "pred_hgb/test_OH_hgb_only_outer_fold_3.csv"))

# No need to differentiate prediction outcome or data type  for external cohorts
save_dir <- "./3_intermediate/sensitivity_analysis/private/single_donation_data/"
cohort <- "sanbs/"

fwrite(single_don_test_s_hf, paste0(save_dir, cohort, "hgb_ferr_sanbs.csv"))
fwrite(single_don_test_s_h, paste0(save_dir, cohort, "hgb_only_sanbs.csv"))

cohort <- "vitalant/"
fwrite(single_don_test_v_hf, paste0(save_dir, cohort, "hgb_ferr_vitalant.csv"))
fwrite(single_don_test_v_h, paste0(save_dir, cohort, "hgb_only_vitalant.csv"))


cohort <- "sanquin/"
fwrite(single_don_test_sq_hf, paste0(save_dir, cohort, "hgb_ferr_sanquin.csv"))
fwrite(single_don_test_sq_h, paste0(save_dir, cohort, "hgb_only_sanquin.csv"))



