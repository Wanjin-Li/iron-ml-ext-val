# GENERATE NESTED CV OBJECTS -----------
#. Will construct 3 outer CV folds in which the model 
#. selection will be repeated to obtain three estimates of the RMSPE of 
#. our model selection procedure that are not at risk of overfitting
#. (the model selected in nested CV may differ from the 
#. model selected, that is OK).

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
  
  ## Perform OH for XGB -----
  # Create dummy one-hot variables for categoricals
  dt.OH.hgb.outer <- create_OH_var("RandID", dt.md.hgb.outer, "fu_hgb")
  dt.OH.ferr.outer <- create_OH_var("RandID", dt.md.ferr.outer, "fu_log_ferritin")
  
  ## Perform Factor -----
  dt.factor.hgb.outer <- create_factor_var(dt.md.hgb.outer)
  dt.factor.ferr.outer <- create_factor_var(dt.md.ferr.outer)
  
  ## Do 3-fold cv split on donors on the entire training set ----
  set.seed(123)
  donor_rsplit_OH_outer <- vfold_cv(data = dt_hgb_unique_donors, v = 3)
  donor_rsplit_factor_outer <- vfold_cv(data = dt_hgb_unique_donors, v = 3)
  
  ## Replace donors in rsplit_res with actual donations; note DonorID is kept which needs to be removed when training -----
  rsplit_OH_hgb_outer <- replace_rsplit_data_with_id(donor_rsplit_OH_outer, dt.OH.hgb.outer)
  rsplit_OH_ferr_outer <- replace_rsplit_data_with_id(donor_rsplit_OH_outer, dt.OH.ferr.outer)
  
  rsplit_factor_hgb_outer <- replace_rsplit_data_with_id(donor_rsplit_factor_outer, dt.factor.hgb.outer)
  rsplit_factor_ferr_outer <- replace_rsplit_data_with_id(donor_rsplit_factor_outer, dt.factor.ferr.outer)
  
  ## Save dataframes -----
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

## PREDICTION USING HGB AND FERRITIN DATASET ----
hgb_ferr_training_data <- fread("./3_intermediate/private/hgb_ferr_rise.csv")

outer_fold_cv_split(hgb_ferr_training_data, "hgb_ferr")


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
  
  ## Perform OH for XGB -----
  # Create dummy one-hot variables for categoricals
  dt.OH.hgb.outer <- create_OH_var("RandID", dt.md.hgb.OH.outer, "fu_hgb")
  dt.OH.ferr.outer <- create_OH_var("RandID", dt.md.ferr.OH.outer, "fu_log_ferritin")
  
  ## Perform Factor -----
  dt.factors.hgb.outer <- create_factor_var(dt.md.hgb.factors.outer)
  dt.factors.ferr.outer <- create_factor_var(dt.md.ferr.factors.outer)
  
  ## Do 5-fold, 3 repeats cv split on donors for each outer fold --------
  set.seed(123)
  donor_rsplit_OH_outer <- vfold_cv(data = dt_unique_donors_OH_outer, v = 5, repeats = 3)
  donor_rsplit_factors_outer <- vfold_cv(data = dt_unique_donors_factors_outer, v = 5, repeats = 3)
  
  ## Replace donors in rsplit_res with actual donations -----
  rsplit_OH_hgb_outer <- replace_rsplit_data(donor_rsplit_OH_outer, dt.OH.hgb.outer)
  rsplit_OH_ferr_outer <- replace_rsplit_data(donor_rsplit_OH_outer, dt.OH.ferr.outer)
  
  rsplit_factors_hgb_outer <- replace_rsplit_data(donor_rsplit_factors_outer, dt.factors.hgb.outer)
  rsplit_factors_ferr_outer <- replace_rsplit_data(donor_rsplit_factors_outer, dt.factors.ferr.outer)
  
  ## Save dataframes -----
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

inner_fold_cv_split(data_version = "hgb_only", outer_fold = 1) # first outer fold
inner_fold_cv_split(data_version = "hgb_only", outer_fold = 2) # second outer fold
inner_fold_cv_split(data_version = "hgb_only", outer_fold = 3) # third outer fold

## DATA HGB AND FERRITIN ----

inner_fold_cv_split(data_version = "hgb_ferr", outer_fold = 1) # first outer fold
inner_fold_cv_split(data_version = "hgb_ferr", outer_fold = 2) # second outer fold
inner_fold_cv_split(data_version = "hgb_ferr", outer_fold = 3) # third outer fold

