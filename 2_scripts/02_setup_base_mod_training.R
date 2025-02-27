# This file is to create regular cross-validation object and hyperparameter sets

#### Main Model (Train on RISE and use these models to validate on SANBS, Vitalant, and Sanquin)
# No Race variable

# Creates cross validation objects, nested cross validation objects, and model hyperparameter sets 

library(data.table)
library(rsample)

## Create directories ----
intermediate_directory <- './3_intermediate/rsplits/main_model/pred_hgb'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory, recursive = TRUE)
}

intermediate_directory <- './3_intermediate/rsplits/main_model/pred_ferr'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory, recursive = TRUE)
}

intermediate_directory <- './3_intermediate/model_dev_data/main_model/pred_hgb'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory, recursive = TRUE)
}

intermediate_directory <- './3_intermediate/model_dev_data/main_model/pred_ferr'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory, recursive = TRUE)
}

# GENERATE MODEL SELECTION CV OBJECTS -----------------------
#. Will use 3 repeats of 5-fold cross-validation
#.  to select the top model based on RMSPE
#.  for each prediction task (hgb, ferritin).
# We have 2 versions of the prediction task:
#  *hgb - predicting follow up hemoglobin
#  *ferr - predicting follow up ferritin
#
# Additionally, we have different versions of the rsplit objects based on
#   requirements for each of the prediction models:
#    *onehot - categorical variable onehot encoded
#    *factor - categorical features as factors
# So a total of 4 objects will be created
#
# The folds are defined in such a way that, for each re-sample, 
#.  all donations from a donor appear in the same CV fold.

# Read data for model development (md) dataset

## PREDICTION USING HGB ONLY DATASET  ------
hgb_training_data <- fread("./3_intermediate/private/hgb_only_rise.csv")

# get unique donors 
hgb_unique_donors <- unique(hgb_training_data$RandID)
dt_hgb_unique_donors <- data.frame(DonorID = hgb_unique_donors)

# set Gender_F as character since loads in as int
hgb_training_data$sex <- as.character(hgb_training_data$sex)

# remove extraneous fields
identifiers <- c("VisitDate", "VisitNum", 
                 "rbc_loss_in_ml",
                 "weight", "height", "bmi", "ebv", "red_cell_volume", "percent_rbc_loss", 
                 "race")

hgb_training_data[, c(identifiers) := NULL]

# 2 prediction tasks: predict hgb, predict ferritin
dt.md.hgb <- subset(hgb_training_data, select = -c(fu_ferritin, fu_log_ferritin))  # predict hgb so remove ferr outcomes
dt.md.ferr <- subset(hgb_training_data, select = -c(fu_hgb, fu_ferritin))  # predict log ferritin so remove hgb outcome and ferr outcome

# Create 2 versions: one with characters as factors and
# one with categorical variables 1-hot encoded for model types that require that

### perform OH for XGB -----
# Create dummy one-hot variables for categoricals

column_to_exclude <- "RandID"
dt.OH.hgb <- copy(dt.md.hgb)
dt.OH.hgb <- data.table(model.matrix(fu_hgb ~., data = dt.md.hgb[, !column_to_exclude, with = FALSE]))
dt.OH.hgb <- cbind(dt.OH.hgb,
                   "RandID" = dt.md.hgb$RandID,
                   "fu_hgb" = dt.md.hgb$fu_hgb)

dt.OH.ferr <- copy(dt.md.ferr)
dt.OH.ferr <- data.table(model.matrix(fu_log_ferritin ~., data = dt.md.ferr[, !column_to_exclude, with = FALSE]))
dt.OH.ferr <- cbind(dt.OH.ferr, 
                    "RandID" = dt.md.ferr$RandID,
                    "fu_log_ferritin" = dt.md.ferr$fu_log_ferritin)

### perform Factor -----
dt.factor.hgb <- copy(dt.md.hgb)
char_columns <- colnames(dt.factor.hgb)[unlist(dt.factor.hgb[, lapply(.SD, is.character),][1,])]
char_columns <- setdiff(char_columns, "RandID")
dt.factor.hgb <- dt.factor.hgb %>%
  mutate_at(vars(char_columns), as.factor)

dt.factor.ferr <- copy(dt.md.ferr)
char_columns <- colnames(dt.factor.ferr)[unlist(dt.factor.ferr[, lapply(.SD, is.character),][1,])]
char_columns <- setdiff(char_columns, "RandID")
dt.factor.ferr <- dt.factor.ferr %>%
  mutate_at(vars(char_columns), as.factor)

### Do cv split on donors --------
set.seed(123)

rsplit_res_factor <- vfold_cv(data = dt_hgb_unique_donors, v = 5, repeats = 3)
rsplit_res_OH <- copy(rsplit_res_factor) # make OH version and factor version of splits the same

### Replace donors in rsplit_res with actual donations -----
replace_rsplit_data <- function(rsplit_output, dt) {
  
  # Make a copy of the data without DonorID to replate data in rsplit
  dt_md_no_donorID <- dt[, -c('RandID')]
  
  splits_list <- rsplit_output$splits
  
  for (i in seq_along(splits_list)) {
    # Access the rsplit object for the current fold
    current_split <- splits_list[[i]]
    
    # Get int[] in_id (list of indices in the training data of the fold)
    train_donor_idxs <- current_split$in_id 
    
    # Get all donors that are inside of training data
    donors_data <- current_split$data
    train_donorIDs <- donors_data[train_donor_idxs, ]
    
    # Get all donation indices that correspond to these donors
    train_donation_idxs <- dt[RandID %in% train_donorIDs, .I]
    train_donation_idxs <- unlist(train_donation_idxs) # make int[]
    
    # Replace donor data with donations data
    current_split$data <- dt_md_no_donorID
    # Replace in_ids with train_donation_idxs
    current_split$in_id <- train_donation_idxs
    
    rsplit_output$splits[[i]] <- current_split
  }
  
  return(rsplit_output)
}


rsplit_factor_hgb <- replace_rsplit_data(rsplit_res_factor, dt.factor.hgb)
rsplit_factor_ferr <- replace_rsplit_data(rsplit_res_factor, dt.factor.ferr)

rsplit_OH_hgb <- replace_rsplit_data(rsplit_res_OH, dt.OH.hgb)
rsplit_OH_ferr <- replace_rsplit_data(rsplit_res_OH, dt.OH.ferr)


### save dataframes -----
# save one hot encoding dataframes
saveRDS(rsplit_OH_hgb, "./3_intermediate/rsplits/main_model/pred_hgb/rsplit_OH_hgb_only.rds")
saveRDS(rsplit_OH_ferr, "./3_intermediate/rsplits/main_model/pred_ferr/rsplit_OH_hgb_only.rds")

# save character as factor dataframes
saveRDS(rsplit_factor_hgb, "./3_intermediate/rsplits/main_model/pred_hgb/rsplit_factors_hgb_only.rds")
saveRDS(rsplit_factor_ferr, "./3_intermediate/rsplits/main_model/pred_ferr/rsplit_factors_hgb_only.rds")


# Save the full training sets as CSV
fwrite(dt.factor.hgb, "./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_factors_hgb_only.csv")
fwrite(dt.factor.ferr, "./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_factors_hgb_only.csv")
fwrite(dt.OH.hgb, "./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_OH_hgb_only.csv")
fwrite(dt.OH.ferr, "./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_OH_hgb_only.csv")


## PREDICTION USING HGB AND FERR DATASET  ------
hgb_ferr_training_data <- fread("./3_intermediate/private/hgb_ferr_rise.csv")

# get unique donors 
hgb_ferr_unique_donors <- unique(hgb_ferr_training_data$RandID)
dt_hgb_ferr_unique_donors <- data.frame(DonorID = hgb_ferr_unique_donors)

# set Gender_F as character since loads in as int
hgb_ferr_training_data$sex <- as.character(hgb_ferr_training_data$sex)

# remove extraneous fields
identifiers <- c("VisitDate", "VisitNum", 
                 "rbc_loss_in_ml",
                 "weight", "height", "bmi", "ebv", "red_cell_volume", "percent_rbc_loss", 
                 "race")

hgb_ferr_training_data[, c(identifiers) := NULL]

# 2 prediction tasks: predict hgb, predict ferritin
dt2.md.hgb <- subset(hgb_ferr_training_data, select = -c(fu_ferritin, fu_log_ferritin))  # predict hgb so remove ferr outcomes
dt2.md.ferr <- subset(hgb_ferr_training_data, select = -c(fu_hgb, fu_ferritin))  # predict log ferritin so remove hgb outcome and ferr outcome

# Create 2 versions: one with characters as factors and
# one with categorical variables 1-hot encoded for model types that require that

### perform OH for XGB -----
# Create dummy one-hot variables for categoricals

column_to_exclude <- "RandID"
dt2.OH.hgb <- copy(dt2.md.hgb)
dt2.OH.hgb <- data.table(model.matrix(fu_hgb ~., data = dt2.md.hgb[, !column_to_exclude, with = FALSE]))
dt2.OH.hgb <- cbind(dt2.OH.hgb,
                   "RandID" = dt2.md.hgb$RandID,
                   "fu_hgb" = dt2.md.hgb$fu_hgb)

dt2.OH.ferr <- copy(dt2.md.ferr)
dt2.OH.ferr <- data.table(model.matrix(fu_log_ferritin ~., data = dt2.md.ferr[, !column_to_exclude, with = FALSE]))
dt2.OH.ferr <- cbind(dt2.OH.ferr, 
                    "RandID" = dt2.md.ferr$RandID,
                    "fu_log_ferritin" = dt2.md.ferr$fu_log_ferritin)

### perform Factor -----
dt2.factor.hgb <- copy(dt2.md.hgb)
char_columns <- colnames(dt2.factor.hgb)[unlist(dt2.factor.hgb[, lapply(.SD, is.character),][1,])]
char_columns <- setdiff(char_columns, "RandID")
dt2.factor.hgb <- dt2.factor.hgb %>%
  mutate_at(vars(char_columns), as.factor)

dt2.factor.ferr <- copy(dt2.md.ferr)
char_columns <- colnames(dt2.factor.ferr)[unlist(dt2.factor.ferr[, lapply(.SD, is.character),][1,])]
char_columns <- setdiff(char_columns, "RandID")
dt2.factor.ferr <- dt2.factor.ferr %>%
  mutate_at(vars(char_columns), as.factor)

### Do cv split on donors --------
set.seed(123)

rsplit2_res_factor <- vfold_cv(data = dt_hgb_ferr_unique_donors, v = 5, repeats = 3)
rsplit2_res_OH <- copy(rsplit2_res_factor)

### Replace donors in rsplit_res with actual donations -----

rsplit2_factor_hgb <- replace_rsplit_data(rsplit2_res_factor, dt2.factor.hgb)
rsplit2_factor_ferr <- replace_rsplit_data(rsplit2_res_factor, dt2.factor.ferr)

rsplit2_OH_hgb <- replace_rsplit_data(rsplit2_res_OH, dt2.OH.hgb)
rsplit2_OH_ferr <- replace_rsplit_data(rsplit2_res_OH, dt2.OH.ferr)

### save dataframes -----
saveRDS(rsplit2_OH_hgb, "./3_intermediate/rsplits/main_model/pred_hgb/rsplit_OH_hgb_ferr.rds")
saveRDS(rsplit2_OH_ferr, "./3_intermediate/rsplits/main_model/pred_ferr/rsplit_OH_hgb_ferr.rds")

saveRDS(rsplit2_factor_hgb, "./3_intermediate/rsplits/main_model/pred_hgb/rsplit_factors_hgb_ferr.rds")
saveRDS(rsplit2_factor_ferr, "./3_intermediate/rsplits/main_model/pred_ferr/rsplit_factors_hgb_ferr.rds")


# Save the full training sets as CSV
fwrite(dt2.factor.hgb, "./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_factors_hgb_ferr.csv")
fwrite(dt2.factor.ferr, "./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_factors_hgb_ferr.csv")
fwrite(dt2.OH.hgb, "./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_OH_hgb_ferr.csv")
fwrite(dt2.OH.ferr, "./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_OH_hgb_ferr.csv")


# DEFINE HYPERPARAMETER SETS -------
#.   Save .csv file for each algorithm where each row contains a  
#.   hyperparameter combo to be assessed.

intermediate_directory <- './3_intermediate/hyperparameters'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}
# models to train
# elastic net, random forest, gradient boosting, svm regression

# XGB MODEL - 4800 hyperparams
xgb_param_sets <- expand.grid(
  eta = c(0.01, 0.05, 0.1, 0.2, 0.3),  # eta: learning rate or shrinkage (shrink contribution of each new tree)
  max_depth = seq(2, 20, 2),  # max_depth: tree depth
  min_child_weight = c(1, 2, 4, 8),
  subsample = c(0.5, 0.65, 0.8, 1),
  colsample_bytree = c(0.5, 0.6, 0.7, 0.8, 0.9, 1)
)
fwrite(xgb_param_sets, "./3_intermediate/hyperparameters/xgb_hyperparameters.csv")

# RANDOM FOREST - 448 hyperparams
rf_param_sets <- expand.grid(
  nodesize = c(1,2,4,8),
  mtry = c(1, 2, 3, 4),
  ntree = seq(200, 2800, 200),  # number of trees: start with 10 times num_features
  replace = c(TRUE, FALSE)
)
fwrite(rf_param_sets, "./3_intermediate/hyperparameters/rf_hyperparameters.csv")

# ELASTICNET - 1051 hyperparams
en_param_sets <- rbind(
  data.table(alpha=0, lambda=0),
  expand.grid(
    alpha = seq(0, 1, 0.05),  # alpha=0: ridge, alpha=1: lasso, 0<alpha<1: elastic net
    lambda = seq(0.01, 0.5, 0.01)  # shrinkage parameter: higher value shrinks coeff to zero more
  )
)
fwrite(en_param_sets, "./3_intermediate/hyperparameters/en_hyperparameters.csv")

# CATBOOST -> 1260 hyperparams (final)
cb_param_sets <- expand.grid(
    loss_function = 'RMSE',
  logging_level = 'Silent', # suppress iteration results
  depth = seq(2, 14, 2), # Similar to max_depth in XGBoost; 16 at max for catboost
  learning_rate = c(0.01, 0.05, 0.1), # Similar to eta in XGBoost
  iterations = seq(200, 1000, 200), # Number of trees
  l2_leaf_reg = c(1, 3, 6, 9), # Similar to lambda in XGBoost
  rsm = c(0.5, 0.7, 1)) # Similar to subsample in XGBoost

fwrite(cb_param_sets, "./3_intermediate/hyperparameters/cb_hyperparameters.csv")

