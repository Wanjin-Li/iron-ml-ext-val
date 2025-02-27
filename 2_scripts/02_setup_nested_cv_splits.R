# This file is to create nested cross-validation object 

# GENERATE NESTED CV OBJECTS -----------
#. Will construct 3 outer CV folds in which the model 
#. selection will be repeated to obtain three estimates of the RMSPE of 
#. our model selection procedure that are not at risk of overfitting

# RUNNING NESTED CV VERSION OF MODEL SELECTION PROCEDURE ----
#. This is identical to the original model selection procedure except
#. repeated 3 times, one in each of the outer CV folds with 2/3 of the data.

# Create data splits for nested cross-validation 

library(data.table)
library(rsample)
source("./2_scripts/utility_functions.R")
source("./2_scripts/02_helper_functions.R")

# Create directories ----
intermediate_folder <- './3_intermediate/rsplits/nested_model'
if (!dir.exists(intermediate_folder)) {
  dir.create(intermediate_folder, recursive = TRUE)
}

intermediate_folder <- './3_intermediate/private/outer_fold'
if (!dir.exists(intermediate_folder)) {
  dir.create(intermediate_folder, recursive = TRUE)
}

intermediate_directory <- './3_intermediate/rsplits/nested_model/outer_fold'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory, recursive = TRUE)
}

intermediate_directory <- './3_intermediate/rsplits/nested_model/outer_fold/pred_hgb'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory, recursive = TRUE)
}

intermediate_directory <- './3_intermediate/rsplits/nested_model/outer_fold/pred_ferr'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory, recursive = TRUE)
}

intermediate_directory <- './3_intermediate/rsplits/nested_model/inner_fold'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory, recursive = TRUE)
}

intermediate_directory <- './3_intermediate/rsplits/nested_model/inner_fold/pred_hgb'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory, recursive = TRUE)
}

intermediate_directory <- './3_intermediate/rsplits/nested_model/inner_fold/pred_ferr'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory, recursive = TRUE)
}


intermediate_directory <- './3_intermediate/model_dev_data/nested_model/outer_fold'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory, recursive = TRUE)
}

intermediate_directory <- './3_intermediate/model_dev_data/nested_model/outer_fold/pred_hgb'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory, recursive = TRUE)
}

intermediate_directory <- './3_intermediate/model_dev_data/nested_model/outer_fold/pred_ferr'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory, recursive = TRUE)
}

intermediate_directory <- './3_intermediate/model_dev_data/nested_model/inner_fold'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory, recursive = TRUE)
}

intermediate_directory <- './3_intermediate/model_dev_data/nested_model/inner_fold/pred_hgb'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory, recursive = TRUE)
}

intermediate_directory <- './3_intermediate/model_dev_data/nested_model/inner_fold/pred_ferr'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory, recursive = TRUE)
}


# OUTER FOLD DATA SPLIT ----

## UTILITY FUNCTION  ----
outer_fold_cv_split <- function(training_data, data_version){
  # get unique donors 
  dt_hgb_unique_donors <- get_unique_donors(training_data)
  
  # remove extraneous fields
  training_data <- remove_extra_cols(training_data)
  
  # 2 prediction tasks: predict hgb, predict ferritin
  dt.md.hgb.outer <- subset(training_data, select = -c(fu_ferritin, fu_log_ferritin))  # predict hgb so remove ferr outcomes
  dt.md.ferr.outer <- subset(training_data, select = -c(fu_hgb, fu_ferritin))  # predict log ferritin so remove hgb outcome and ferr outcome
  
  ## Perform OH for XGB 
  # Create dummy one-hot variables for categoricals
  dt.OH.hgb.outer <- create_OH_var("RandID", dt.md.hgb.outer, "fu_hgb")
  dt.OH.ferr.outer <- create_OH_var("RandID", dt.md.ferr.outer, "fu_log_ferritin")
  
  ## Perform Factor
  dt.factor.hgb.outer <- create_factor_var(dt.md.hgb.outer)
  dt.factor.ferr.outer <- create_factor_var(dt.md.ferr.outer)
  
  ## Do 3-fold cv split on donors on the entire training set 
  set.seed(123)
  donor_rsplit_factor_outer <- vfold_cv(data = dt_hgb_unique_donors, v = 3)
  donor_rsplit_OH_outer <- copy(donor_rsplit_factor_outer)

  ## Replace donors in rsplit_res with actual donations; 
  ## note DonorID is kept which needs to be removed when training 
  rsplit_OH_hgb_outer <- replace_rsplit_data_with_id(donor_rsplit_OH_outer, dt.OH.hgb.outer)
  rsplit_OH_ferr_outer <- replace_rsplit_data_with_id(donor_rsplit_OH_outer, dt.OH.ferr.outer)
  
  rsplit_factor_hgb_outer <- replace_rsplit_data_with_id(donor_rsplit_factor_outer, dt.factor.hgb.outer)
  rsplit_factor_ferr_outer <- replace_rsplit_data_with_id(donor_rsplit_factor_outer, dt.factor.ferr.outer)

  ## Save dataframes 
  base_path <- "./3_intermediate/rsplits/nested_model/outer_fold/"

  # save unique donor splits for inner fold splits
  saveRDS(donor_rsplit_OH_outer, paste0(base_path, "donor_rsplit_OH_", data_version, "_outer_folds.rds"))
  saveRDS(donor_rsplit_factor_outer, paste0(base_path, "donor_rsplit_factors_", data_version, "_outer_folds.rds"))

  # save one hot encoding dataframes
  saveRDS(rsplit_OH_hgb_outer, paste0(base_path, "pred_hgb/rsplit_OH_", data_version, "_outer_folds.rds"))
  saveRDS(rsplit_OH_ferr_outer, paste0(base_path, "pred_ferr/rsplit_OH_", data_version, "_outer_folds.rds"))

  # save character as factor dataframes
  saveRDS(rsplit_factor_hgb_outer, paste0(base_path, "pred_hgb/rsplit_factors_", data_version, "_outer_folds.rds"))
  saveRDS(rsplit_factor_ferr_outer, paste0(base_path, "pred_ferr/rsplit_factors_", data_version, "_outer_folds.rds"))

  # Save the full training sets as CSV
  base_path2 <- "./3_intermediate/model_dev_data/nested_model/outer_fold/"

  fwrite(dt.factor.hgb.outer, paste0(base_path2, "pred_hgb/mdset_factors_", data_version, "_outer_folds.csv"))
  fwrite(dt.factor.ferr.outer, paste0(base_path2, "pred_ferr/mdset_factors_", data_version, "_outer_folds.csv"))
  fwrite(dt.OH.hgb.outer, paste0(base_path2, "pred_hgb/mdset_OH_", data_version, "_outer_folds.csv"))
  fwrite(dt.OH.ferr.outer, paste0(base_path2, "pred_ferr/mdset_OH_", data_version, "_outer_folds.csv"))
  
}

## PREDICTION USING HGB ONLY DATASET  ----
hgb_training_data <- fread("./3_intermediate/private/hgb_only_rise.csv")

outer_fold_cv_split(hgb_training_data, "hgb_only")

#________________________________________________________________________________
dt_hgb_unique_donors <- get_unique_donors(hgb_training_data)

# remove extraneous fields
training_data <- remove_extra_cols(hgb_training_data)

# 2 prediction tasks: predict hgb, predict ferritin
dt.md.hgb.outer <- subset(training_data, select = -c(fu_ferritin, fu_log_ferritin))  # predict hgb so remove ferr outcomes
dt.md.ferr.outer <- subset(training_data, select = -c(fu_hgb, fu_ferritin))  # predict log ferritin so remove hgb outcome and ferr outcome

### Perform OH for XGB -----
# Create dummy one-hot variables for categoricals
dt.OH.hgb.outer <- create_OH_var("RandID", dt.md.hgb.outer, "fu_hgb")
dt.OH.ferr.outer <- create_OH_var("RandID", dt.md.ferr.outer, "fu_log_ferritin")

### Perform Factor -----
dt.factor.hgb.outer <- create_factor_var(dt.md.hgb.outer)
dt.factor.ferr.outer <- create_factor_var(dt.md.ferr.outer)

# make OH version and factor version of splits the same
donor_rsplit_OH_hgb_only_outer <- readRDS("./3_intermediate/rsplits/nested_model/outer_fold/donor_rsplit_factors_hgb_only_outer_folds.rds")
rsplit_OH_hgb_outer <- replace_rsplit_data_with_id(donor_rsplit_OH_hgb_only_outer, dt.OH.hgb.outer)
rsplit_OH_ferr_outer <- replace_rsplit_data_with_id(donor_rsplit_OH_hgb_only_outer, dt.OH.ferr.outer)

# save unique donor splits for inner fold splits
base_path <- "./3_intermediate/rsplits/nested_model/outer_fold/"
data_version <- "hgb_only"

saveRDS(donor_rsplit_OH_hgb_only_outer, paste0(base_path, "donor_rsplit_OH_", data_version, "_outer_folds.rds"))

# save one hot encoding dataframes
saveRDS(rsplit_OH_hgb_outer, paste0(base_path, "pred_hgb/rsplit_OH_", data_version, "_outer_folds.rds"))
saveRDS(rsplit_OH_ferr_outer, paste0(base_path, "pred_ferr/rsplit_OH_", data_version, "_outer_folds.rds"))

# Save the full training sets as CSV
base_path2 <- "./3_intermediate/model_dev_data/nested_model/outer_fold/"
fwrite(dt.OH.hgb.outer, paste0(base_path2, "pred_hgb/mdset_OH_", data_version, "_outer_folds.csv"))
fwrite(dt.OH.ferr.outer, paste0(base_path2, "pred_ferr/mdset_OH_", data_version, "_outer_folds.csv"))
#________________________________________________________________________________



## PREDICTION USING HGB AND FERRITIN DATASET ----
hgb_ferr_training_data <- fread("./3_intermediate/private/hgb_ferr_rise.csv")

outer_fold_cv_split(hgb_ferr_training_data, "hgb_ferr")

#________________________________________________________________________________
dt_hgb_unique_donors <- get_unique_donors(hgb_ferr_training_data)

# remove extraneous fields
training_data <- remove_extra_cols(hgb_ferr_training_data)

# 2 prediction tasks: predict hgb, predict ferritin
dt.md.hgb.outer <- subset(training_data, select = -c(fu_ferritin, fu_log_ferritin))  # predict hgb so remove ferr outcomes
dt.md.ferr.outer <- subset(training_data, select = -c(fu_hgb, fu_ferritin))  # predict log ferritin so remove hgb outcome and ferr outcome

### Perform OH for XGB -----
# Create dummy one-hot variables for categoricals
dt.OH.hgb.outer <- create_OH_var("RandID", dt.md.hgb.outer, "fu_hgb")
dt.OH.ferr.outer <- create_OH_var("RandID", dt.md.ferr.outer, "fu_log_ferritin")

### Perform Factor -----
dt.factor.hgb.outer <- create_factor_var(dt.md.hgb.outer)
dt.factor.ferr.outer <- create_factor_var(dt.md.ferr.outer)

# make OH version and factor version of splits the same
donor_rsplit_OH_hgb_ferr_outer <- readRDS("./3_intermediate/rsplits/nested_model/outer_fold/donor_rsplit_factors_hgb_ferr_outer_folds.rds")
rsplit_OH_hgb_outer <- replace_rsplit_data_with_id(donor_rsplit_OH_hgb_ferr_outer, dt.OH.hgb.outer)
rsplit_OH_ferr_outer <- replace_rsplit_data_with_id(donor_rsplit_OH_hgb_ferr_outer, dt.OH.ferr.outer)

# save unique donor splits for inner fold splits
base_path <- "./3_intermediate/rsplits/nested_model/outer_fold/"
data_version <- "hgb_ferr"

saveRDS(donor_rsplit_OH_hgb_ferr_outer, paste0(base_path, "donor_rsplit_OH_", data_version, "_outer_folds.rds"))

# # save one hot encoding dataframes
saveRDS(rsplit_OH_hgb_outer, paste0(base_path, "pred_hgb/rsplit_OH_", data_version, "_outer_folds.rds"))
saveRDS(rsplit_OH_ferr_outer, paste0(base_path, "pred_ferr/rsplit_OH_", data_version, "_outer_folds.rds"))

base_path2 <- "./3_intermediate/model_dev_data/nested_model/outer_fold/"
fwrite(dt.OH.hgb.outer, paste0(base_path2, "pred_hgb/mdset_OH_", data_version, "_outer_folds.csv"))
fwrite(dt.OH.ferr.outer, paste0(base_path2, "pred_ferr/mdset_OH_", data_version, "_outer_folds.csv"))
#________________________________________________________________________________



# INNER FOLD DATA SPLITS -----------
## RETRIEVE OUTER FOLD DATA ----
### DATA HGB ONLY ----

donor_rsplit_OH_hgb_only_outer_folds <- readRDS("./3_intermediate/rsplits/nested_model/outer_fold/donor_rsplit_OH_hgb_only_outer_folds.rds")
donor_rsplit_factors_hgb_only_outer_folds <- readRDS("./3_intermediate/rsplits/nested_model/outer_fold/donor_rsplit_factors_hgb_only_outer_folds.rds")


retrieve_outer_fold_data(dt.training = hgb_training_data,
                         rsplit = donor_rsplit_OH_hgb_only_outer_folds,
                         var_version = "OH",
                         data_version = "hgb_only")

retrieve_outer_fold_data(dt.training = hgb_training_data,
                         rsplit = donor_rsplit_factors_hgb_only_outer_folds,
                         var_version = "factors",
                         data_version = "hgb_only")
### DATA HGB AND FERRITIN ----

donor_rsplit_OH_hgb_ferr_outer_folds <- readRDS("./3_intermediate/rsplits/nested_model/outer_fold/donor_rsplit_OH_hgb_ferr_outer_folds.rds")
donor_rsplit_factors_hgb_ferr_outer_folds <- readRDS("./3_intermediate/rsplits/nested_model/outer_fold/donor_rsplit_factors_hgb_ferr_outer_folds.rds")


retrieve_outer_fold_data(dt.training = hgb_ferr_training_data,
                         rsplit = donor_rsplit_OH_hgb_ferr_outer_folds,
                         var_version = "OH",
                         data_version = "hgb_ferr")

retrieve_outer_fold_data(dt.training = hgb_ferr_training_data,
                         rsplit = donor_rsplit_factors_hgb_ferr_outer_folds,
                         var_version = "factors",
                         data_version = "hgb_ferr")




## UTILITY FUNCTION ----
inner_fold_cv_split <- function(data_version, outer_fold){
  # read outer fold training data
  training_OH_outer_fold <- fread(paste0("./3_intermediate/private/outer_fold/", data_version, "_OH_outer_fold_", outer_fold, ".csv"))
  training_factors_outer_fold <- fread(paste0("./3_intermediate/private/outer_fold/", data_version, "_factors_outer_fold_", outer_fold, ".csv"))
  
  # get unique donors 
  dt_unique_donors_OH_outer <- get_unique_donors(training_OH_outer_fold)
  dt_unique_donors_factors_outer <- get_unique_donors(training_factors_outer_fold)
  
  # remove extraneous fields
  training_OH_outer_fold <- remove_extra_cols(training_OH_outer_fold)
  training_factors_outer_fold <- remove_extra_cols(training_factors_outer_fold)
  
  # 2 prediction tasks: predict hgb, predict ferritin
  dt.md.hgb.OH.outer <- subset(training_OH_outer_fold, select = -c(fu_ferritin, fu_log_ferritin))  # predict hgb so remove ferr outcomes
  dt.md.ferr.OH.outer <- subset(training_OH_outer_fold, select = -c(fu_hgb, fu_ferritin))  # predict log ferritin so remove hgb outcome and ferr outcome
  
  dt.md.hgb.factors.outer <- subset(training_factors_outer_fold, select = -c(fu_ferritin, fu_log_ferritin))  # predict hgb so remove ferr outcomes
  dt.md.ferr.factors.outer <- subset(training_factors_outer_fold, select = -c(fu_hgb, fu_ferritin))  # predict log ferritin so remove hgb outcome and ferr outcome
  
  # Create 2 versions: one with characters as factors and
  # one with categorical variables 1-hot encoded for model types that require that
  
  ## Perform OH for XGB
  # Create dummy one-hot variables for categoricals
  dt.OH.hgb.outer <- create_OH_var("RandID", dt.md.hgb.OH.outer, "fu_hgb")
  dt.OH.ferr.outer <- create_OH_var("RandID", dt.md.ferr.OH.outer, "fu_log_ferritin")
  
  ## Perform Factor
  dt.factors.hgb.outer <- create_factor_var(dt.md.hgb.factors.outer)
  dt.factors.ferr.outer <- create_factor_var(dt.md.ferr.factors.outer)
  
  ## Do 5-fold, 3 repeats cv split on donors for each outer fold
  set.seed(123)
  donor_rsplit_factors_outer <- vfold_cv(data = dt_unique_donors_factors_outer, v = 5, repeats = 3)
  donor_rsplit_OH_outer <- copy(donor_rsplit_factors_outer) # make OH version and factor version of splits the same
  
  ## Replace donors in rsplit_res with actual donations
  rsplit_OH_hgb_outer <- replace_rsplit_data(donor_rsplit_OH_outer, dt.OH.hgb.outer)
  rsplit_OH_ferr_outer <- replace_rsplit_data(donor_rsplit_OH_outer, dt.OH.ferr.outer)
  
  rsplit_factors_hgb_outer <- replace_rsplit_data(donor_rsplit_factors_outer, dt.factors.hgb.outer)
  rsplit_factors_ferr_outer <- replace_rsplit_data(donor_rsplit_factors_outer, dt.factors.ferr.outer)

  ## Save dataframes
  base_path <- "./3_intermediate/rsplits/nested_model/inner_fold/"

  # save one hot encoding dataframes
  saveRDS(rsplit_OH_hgb_outer, paste0(base_path, "pred_hgb/rsplit_OH_", data_version, "_outer_fold_", outer_fold, ".rds"))
  saveRDS(rsplit_OH_ferr_outer, paste0(base_path, "pred_ferr/rsplit_OH_", data_version, "_outer_fold_", outer_fold, ".rds"))

  # save character as factor dataframes
  saveRDS(rsplit_factors_hgb_outer, paste0(base_path, "pred_hgb/rsplit_factors_", data_version, "_outer_fold_", outer_fold, ".rds"))
  saveRDS(rsplit_factors_ferr_outer, paste0(base_path, "pred_ferr/rsplit_factors_", data_version, "_outer_fold_", outer_fold, ".rds"))

  # Save the full training sets as CSV
  base_path2 <- "./3_intermediate/model_dev_data/nested_model/inner_fold/"

  fwrite(dt.OH.hgb.outer, paste0(base_path2, "pred_hgb/mdset_OH_", data_version, "_outer_fold_", outer_fold, ".csv"))
  fwrite(dt.OH.ferr.outer, paste0(base_path2, "pred_ferr/mdset_OH_", data_version, "_outer_fold_", outer_fold, ".csv"))
  fwrite(dt.factors.hgb.outer, paste0(base_path2, "pred_hgb/mdset_factors_", data_version, "_outer_fold_", outer_fold, ".csv"))
  fwrite(dt.factors.ferr.outer, paste0(base_path2, "pred_ferr/mdset_factors_", data_version, "_outer_fold_", outer_fold, ".csv"))
  
}


## DATA HGB ONLY ----
#________________________________________________________________________________

# inner_fold_cv_split(data_version = "hgb_only", outer_fold = 1) 
# inner_fold_cv_split(data_version = "hgb_only", outer_fold = 2) 
# inner_fold_cv_split(data_version = "hgb_only", outer_fold = 3) 

#________________________________________________________________________________

convert_rsplit_OH <- function(rsplit_factors_output, biomarker) {

  splits_list <- rsplit_factors_output$splits

  for (i in seq_along(splits_list)) {

    # Access the rsplit object for the current fold
    current_split <- splits_list[[i]]

    # Get all donation data within the current split
    donations_data <- current_split$data

    # convert factor version of donation data to OH version
    if (biomarker == "hgb"){
      donation_data_OH <- data.table(model.matrix(fu_hgb ~., data = donations_data))
      donation_data_OH <- cbind(donation_data_OH,
                                "fu_hgb" = donations_data$fu_hgb)
    } else if (biomarker == "ferritin"){
      donation_data_OH <- data.table(model.matrix(fu_log_ferritin ~., data = donations_data))
      donation_data_OH <- cbind(donation_data_OH,
                                "fu_log_ferritin" = donations_data$fu_log_ferritin)

    }

    # Replace factor version data with OH version
    current_split$data <- donation_data_OH
    # Replace in_ids with train_donor_idxs

    rsplit_factors_output$splits[[i]] <- current_split
  }

  return(rsplit_factors_output)
}


rsplit_factors_h_h_outer1 <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_factors_hgb_only_outer_fold_1.rds")
rsplit_OH_h_h_outer1 <- convert_rsplit_OH(rsplit_factors_h_h_outer1, "hgb")
saveRDS(rsplit_OH_h_h_outer1, "./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_OH_hgb_only_outer_fold_1.rds")

rsplit_factors_h_h_outer2 <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_factors_hgb_only_outer_fold_2.rds")
rsplit_OH_h_h_outer2 <- convert_rsplit_OH(rsplit_factors_h_h_outer2, "hgb")
saveRDS(rsplit_OH_h_h_outer2, "./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_OH_hgb_only_outer_fold_2.rds")

rsplit_factors_h_h_outer3 <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_factors_hgb_only_outer_fold_3.rds")
rsplit_OH_h_h_outer3 <- convert_rsplit_OH(rsplit_factors_h_h_outer3, "hgb")
saveRDS(rsplit_OH_h_h_outer3, "./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_OH_hgb_only_outer_fold_3.rds")

rsplit_factors_f_h_outer1 <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_factors_hgb_only_outer_fold_1.rds")
rsplit_OH_f_h_outer1 <- convert_rsplit_OH(rsplit_factors_f_h_outer1, "ferritin")
saveRDS(rsplit_OH_f_h_outer1, "./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_OH_hgb_only_outer_fold_1.rds")

rsplit_factors_f_h_outer2 <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_factors_hgb_only_outer_fold_2.rds")
rsplit_OH_f_h_outer2 <- convert_rsplit_OH(rsplit_factors_f_h_outer2, "ferritin")
saveRDS(rsplit_OH_f_h_outer2, "./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_OH_hgb_only_outer_fold_2.rds")

rsplit_factors_f_h_outer3 <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_factors_hgb_only_outer_fold_3.rds")
rsplit_OH_f_h_outer3 <- convert_rsplit_OH(rsplit_factors_f_h_outer3, "ferritin")
saveRDS(rsplit_OH_f_h_outer3, "./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_OH_hgb_only_outer_fold_3.rds")


### Update model development data
# convert the factor version of model development data to OH version

# all donations - both training and testing splits
mdset_factors_h_h_outer1 <- fread("./3_intermediate/model_dev_data/nested_model/inner_fold/pred_hgb/mdset_factors_hgb_only_outer_fold_1.csv")
mdset_OH_h_h_outer1 <- create_OH_var("RandID", mdset_factors_h_h_outer1, "fu_hgb")
fwrite(mdset_OH_h_h_outer1, "./3_intermediate/model_dev_data/nested_model/inner_fold/pred_hgb/mdset_OH_hgb_only_outer_fold_1.csv")

mdset_factors_h_h_outer2 <- fread("./3_intermediate/model_dev_data/nested_model/inner_fold/pred_hgb/mdset_factors_hgb_only_outer_fold_2.csv")
mdset_OH_h_h_outer2 <- create_OH_var("RandID", mdset_factors_h_h_outer2, "fu_hgb")
fwrite(mdset_OH_h_h_outer2, "./3_intermediate/model_dev_data/nested_model/inner_fold/pred_hgb/mdset_OH_hgb_only_outer_fold_2.csv")

mdset_factors_h_h_outer3 <- fread("./3_intermediate/model_dev_data/nested_model/inner_fold/pred_hgb/mdset_factors_hgb_only_outer_fold_3.csv")
mdset_OH_h_h_outer3 <- create_OH_var("RandID", mdset_factors_h_h_outer3, "fu_hgb")
fwrite(mdset_OH_h_h_outer3, "./3_intermediate/model_dev_data/nested_model/inner_fold/pred_hgb/mdset_OH_hgb_only_outer_fold_3.csv")

mdset_factors_f_h_outer1 <- fread("./3_intermediate/model_dev_data/nested_model/inner_fold/pred_ferr/mdset_factors_hgb_only_outer_fold_1.csv")
mdset_OH_f_h_outer1 <- create_OH_var("RandID", mdset_factors_f_h_outer1, "fu_log_ferritin")
fwrite(mdset_OH_f_h_outer1, "./3_intermediate/model_dev_data/nested_model/inner_fold/pred_ferr/mdset_OH_hgb_only_outer_fold_1.csv")

mdset_factors_f_h_outer2 <- fread("./3_intermediate/model_dev_data/nested_model/inner_fold/pred_ferr/mdset_factors_hgb_only_outer_fold_2.csv")
mdset_OH_f_h_outer2 <- create_OH_var("RandID", mdset_factors_f_h_outer2, "fu_log_ferritin")
fwrite(mdset_OH_f_h_outer2, "./3_intermediate/model_dev_data/nested_model/inner_fold/pred_ferr/mdset_OH_hgb_only_outer_fold_2.csv")

mdset_factors_f_h_outer3 <- fread("./3_intermediate/model_dev_data/nested_model/inner_fold/pred_ferr/mdset_factors_hgb_only_outer_fold_3.csv")
mdset_OH_f_h_outer3 <- create_OH_var("RandID", mdset_factors_f_h_outer3, "fu_log_ferritin")
fwrite(mdset_OH_f_h_outer3, "./3_intermediate/model_dev_data/nested_model/inner_fold/pred_ferr/mdset_OH_hgb_only_outer_fold_3.csv")


#________________________________________________________________________________

## DATA HGB AND FERRITIN ----
#__________________________________________________________________________________

# inner_fold_cv_split(data_version = "hgb_ferr", outer_fold = 1) 
# inner_fold_cv_split(data_version = "hgb_ferr", outer_fold = 2) 
# inner_fold_cv_split(data_version = "hgb_ferr", outer_fold = 3) 

#________________________________________________________________________________
rsplit_factors_h_hf_outer1 <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_factors_hgb_ferr_outer_fold_1.rds")
rsplit_OH_h_hf_outer1 <- convert_rsplit_OH(rsplit_factors_h_hf_outer1, "hgb")
saveRDS(rsplit_OH_h_hf_outer1, "./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_OH_hgb_ferr_outer_fold_1.rds")

rsplit_factors_h_hf_outer2 <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_factors_hgb_ferr_outer_fold_2.rds")
rsplit_OH_h_hf_outer2 <- convert_rsplit_OH(rsplit_factors_h_hf_outer2, "hgb")
saveRDS(rsplit_OH_h_hf_outer2, "./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_OH_hgb_ferr_outer_fold_2.rds")

rsplit_factors_h_hf_outer3 <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_factors_hgb_ferr_outer_fold_3.rds")
rsplit_OH_h_hf_outer3 <- convert_rsplit_OH(rsplit_factors_h_hf_outer3, "hgb")
saveRDS(rsplit_OH_h_hf_outer3, "./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_OH_hgb_ferr_outer_fold_3.rds")


rsplit_factors_f_hf_outer1 <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_factors_hgb_ferr_outer_fold_1.rds")
rsplit_OH_f_hf_outer1 <- convert_rsplit_OH(rsplit_factors_f_hf_outer1, "ferritin")
saveRDS(rsplit_OH_f_hf_outer1, "./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_OH_hgb_ferr_outer_fold_1.rds")

rsplit_factors_f_hf_outer2 <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_factors_hgb_ferr_outer_fold_2.rds")
rsplit_OH_f_hf_outer2 <- convert_rsplit_OH(rsplit_factors_f_hf_outer2, "ferritin")
saveRDS(rsplit_OH_f_hf_outer2, "./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_OH_hgb_ferr_outer_fold_2.rds")

rsplit_factors_f_hf_outer3 <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_factors_hgb_ferr_outer_fold_3.rds")
rsplit_OH_f_hf_outer3 <- convert_rsplit_OH(rsplit_factors_f_hf_outer3, "ferritin")
saveRDS(rsplit_OH_f_hf_outer3, "./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_OH_hgb_ferr_outer_fold_3.rds")

# Update model development data
# convert the factor version of model development data to OH version

# all donations - both training and testing splits
mdset_factors_h_hf_outer1 <- fread("./3_intermediate/model_dev_data/nested_model/inner_fold/pred_hgb/mdset_factors_hgb_ferr_outer_fold_1.csv")
mdset_OH_h_hf_outer1 <- create_OH_var("RandID", mdset_factors_h_hf_outer1, "fu_hgb")
fwrite(mdset_OH_h_hf_outer1, "./3_intermediate/model_dev_data/nested_model/inner_fold/pred_hgb/mdset_OH_hgb_ferr_outer_fold_1.csv")

mdset_factors_h_hf_outer2 <- fread("./3_intermediate/model_dev_data/nested_model/inner_fold/pred_hgb/mdset_factors_hgb_ferr_outer_fold_2.csv")
mdset_OH_h_hf_outer2 <- create_OH_var("RandID", mdset_factors_h_hf_outer2, "fu_hgb")
fwrite(mdset_OH_h_hf_outer2, "./3_intermediate/model_dev_data/nested_model/inner_fold/pred_hgb/mdset_OH_hgb_ferr_outer_fold_2.csv")

mdset_factors_h_hf_outer3 <- fread("./3_intermediate/model_dev_data/nested_model/inner_fold/pred_hgb/mdset_factors_hgb_ferr_outer_fold_3.csv")
mdset_OH_h_hf_outer3 <- create_OH_var("RandID", mdset_factors_h_hf_outer3, "fu_hgb")
fwrite(mdset_OH_h_hf_outer3, "./3_intermediate/model_dev_data/nested_model/inner_fold/pred_hgb/mdset_OH_hgb_ferr_outer_fold_3.csv")

mdset_factors_f_hf_outer1 <- fread("./3_intermediate/model_dev_data/nested_model/inner_fold/pred_ferr/mdset_factors_hgb_ferr_outer_fold_1.csv")
mdset_OH_f_hf_outer1 <- create_OH_var("RandID", mdset_factors_f_hf_outer1, "fu_log_ferritin")
fwrite(mdset_OH_f_hf_outer1, "./3_intermediate/model_dev_data/nested_model/inner_fold/pred_ferr/mdset_OH_hgb_ferr_outer_fold_1.csv")

mdset_factors_f_hf_outer2 <- fread("./3_intermediate/model_dev_data/nested_model/inner_fold/pred_ferr/mdset_factors_hgb_ferr_outer_fold_2.csv")
mdset_OH_f_hf_outer2 <- create_OH_var("RandID", mdset_factors_f_hf_outer2, "fu_log_ferritin")
fwrite(mdset_OH_f_hf_outer2, "./3_intermediate/model_dev_data/nested_model/inner_fold/pred_ferr/mdset_OH_hgb_ferr_outer_fold_2.csv")

mdset_factors_f_hf_outer3 <- fread("./3_intermediate/model_dev_data/nested_model/inner_fold/pred_ferr/mdset_factors_hgb_ferr_outer_fold_3.csv")
mdset_OH_f_hf_outer3 <- create_OH_var("RandID", mdset_factors_f_hf_outer3, "fu_log_ferritin")
fwrite(mdset_OH_f_hf_outer3, "./3_intermediate/model_dev_data/nested_model/inner_fold/pred_ferr/mdset_OH_hgb_ferr_outer_fold_3.csv")

#________________________________________________________________________________

