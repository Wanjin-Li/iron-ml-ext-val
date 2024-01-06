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

dat_paths <- c("./3_intermediate/private/hgb_ferr_rise.csv",
               "./3_intermediate/private/hgb_only_rise.csv")

## PREDICTION USING HGB ONLY DATASET  ------
hgb_training_data <- fread("./3_intermediate/private/hgb_only_rise.csv")

# get unique donors 
hgb_unique_donors <- unique(hgb_training_data$RandID)
print(paste0(length(hgb_unique_donors), " unique donors"))
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

# ADAPTED FROM SOPHIE'S CODE from here down

### perform OH for XGB -----
# Create dummy one-hot variables for categoricals

column_to_exclude <- "RandID"
dt.OH.hgb <- copy(dt.md.hgb)
dt.OH.hgb <- data.table(model.matrix(fu_hgb ~., data = dt.md.hgb[, !column_to_exclude, with = FALSE]))
dt.OH.hgb <- cbind(dt.OH.hgb,
                   "RandID" = dt.md.hgb$RandID,
                   "fu_hgb" = dt.md.hgb$fu_hgb)

dt.OH.ferr <- copy(dt.md.ferr)
dt.OH.ferr <- data.table(model.matrix(fu_log_ferritin ~., data = dt.OH.ferr[, !column_to_exclude, with = FALSE]))
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
rsplit_res_OH <- vfold_cv(data = dt_hgb_unique_donors, v = 5, repeats = 3)
rsplit_res_factor <- vfold_cv(data = dt_hgb_unique_donors, v = 5, repeats = 3)

### Replace donors in rsplit_res with actual donations -----
replace_rsplit_data <- function(rsplit_output, dt) {
  
  # Make a copy of the data without DonorID to replate data in rsplit
  dt_md_no_donorID <- dt[, -c('RandID')]
  
  splits_list <- rsplit_output$splits
  
  for (i in seq_along(splits_list)) {
    print(i)
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

rsplit_OH_hgb <- replace_rsplit_data(rsplit_res_OH, dt.OH.hgb)
rsplit_OH_ferr <- replace_rsplit_data(rsplit_res_OH, dt.OH.ferr)
  
rsplit_factor_hgb <- replace_rsplit_data(rsplit_res_factor, dt.factor.hgb)
rsplit_factor_ferr <- replace_rsplit_data(rsplit_res_factor, dt.factor.ferr)


# analysis(rsplit_factor_hgb$splits[[1]])
# assessment(rsplit_factor_hgb$splits[[1]])
# 

### save dataframes -----
# save one hot encoding dataframes
saveRDS(rsplit_OH_hgb, 
              "./3_intermediate/rsplits/main_model/pred_hgb/rsplit_OH_hgb_only.rds")
saveRDS(rsplit_OH_ferr, 
              "./3_intermediate/rsplits/main_model/pred_ferr/rsplit_OH_hgb_only.rds")

# save character as factor dataframes
saveRDS(rsplit_factor_hgb, 
        "./3_intermediate/rsplits/main_model/pred_hgb/rsplit_factors_hgb_only.rds")
saveRDS(rsplit_factor_ferr, 
        "./3_intermediate/rsplits/main_model/pred_ferr/rsplit_factors_hgb_only.rds")


# Save the full training sets as CSV
fwrite(dt.factor.hgb, "./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_factors_hgb_only.csv")
fwrite(dt.factor.ferr, "./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_factors_hgb_only.csv")
fwrite(dt.OH.hgb, "./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_OH_hgb_only.csv")
fwrite(dt.OH.ferr, "./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_OH_hgb_only.csv")


## PREDICTION USING HGB AND FERR DATASET  ------
hgb_ferr_training_data <- fread("./3_intermediate/private/hgb_ferr_rise.csv")

# get unique donors 
hgb_ferr_unique_donors <- unique(hgb_ferr_training_data$RandID)
print(paste0(length(hgb_ferr_unique_donors), " unique donors"))
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

# ADAPTED FROM SOPHIE'S CODE from here down

### perform OH for XGB -----
# Create dummy one-hot variables for categoricals

column_to_exclude <- "RandID"
dt2.OH.hgb <- copy(dt2.md.hgb)
dt2.OH.hgb <- data.table(model.matrix(fu_hgb ~., data = dt2.md.hgb[, !column_to_exclude, with = FALSE]))
dt2.OH.hgb <- cbind(dt2.OH.hgb,
                   "RandID" = dt2.md.hgb$RandID,
                   "fu_hgb" = dt2.md.hgb$fu_hgb)

dt2.OH.ferr <- copy(dt2.md.ferr)
dt2.OH.ferr <- data.table(model.matrix(fu_log_ferritin ~., data = dt2.OH.ferr[, !column_to_exclude, with = FALSE]))
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
rsplit2_res_OH <- vfold_cv(data = dt_hgb_ferr_unique_donors, v = 5, repeats = 3)
rsplit2_res_factor <- vfold_cv(data = dt_hgb_ferr_unique_donors, v = 5, repeats = 3)

### Replace donors in rsplit_res with actual donations -----

rsplit2_OH_hgb <- replace_rsplit_data(rsplit2_res_OH, dt2.OH.hgb)
rsplit2_OH_ferr <- replace_rsplit_data(rsplit2_res_OH, dt2.OH.ferr)

rsplit2_factor_hgb <- replace_rsplit_data(rsplit2_res_factor, dt2.factor.hgb)
rsplit2_factor_ferr <- replace_rsplit_data(rsplit2_res_factor, dt2.factor.ferr)

### save dataframes -----
# save one hot encoding dataframes
saveRDS(rsplit2_OH_hgb, 
        "./3_intermediate/rsplits/main_model/pred_hgb/rsplit_OH_hgb_ferr.rds")
saveRDS(rsplit2_OH_ferr, 
        "./3_intermediate/rsplits/main_model/pred_ferr/rsplit_OH_hgb_ferr.rds")

# save character as factor dataframes
saveRDS(rsplit2_factor_hgb, 
        "./3_intermediate/rsplits/main_model/pred_hgb/rsplit_factors_hgb_ferr.rds")
saveRDS(rsplit2_factor_ferr, 
        "./3_intermediate/rsplits/main_model/pred_ferr/rsplit_factors_hgb_ferr.rds")


# Save the full training sets as CSV
fwrite(dt2.factor.hgb, "./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_factors_hgb_ferr.csv")
fwrite(dt2.factor.ferr, "./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_factors_hgb_ferr.csv")
fwrite(dt2.OH.hgb, "./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_OH_hgb_ferr.csv")
fwrite(dt2.OH.ferr, "./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_OH_hgb_ferr.csv")






# GENERATE NESTED CV OBJECTS -----------
#. Will construct 3 outer CV folds in which the model 
#. selection will be repeated to obtain three estimates of the RMSPE of 
#. our model selection procedure that are not at risk of overfitting
#. (the model selected in nested CV may differ from the 
#. model selected, that is OK).






# DEFINE HYPERPARAMETER SETS -------
#,   Save .csv file for each algorithm where each row contains a  
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
  
  # Regularization - use if large difference btwn training error and test error
  # gamma = c(1, 5, 10 , 15, 20)
  # alpha - L1 regularization
  # lambda - L2 regularization
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

# CATBOOST -> 4000 hyperparams
cb_param_sets <- expand.grid(
    loss_function = 'RMSE',
  # logging_level = 'Silent',
  depth = seq(2, 20, 2), # Similar to max_depth in XGBoost
  learning_rate = c(0.01, 0.05, 0.1, 0.2, 0.3), # Similar to eta in XGBoost
  iterations = seq(200, 1000, 200), # Number of trees
  l2_leaf_reg = c(1, 3, 6, 9), # Similar to lambda in XGBoost
  rsm = c(0.5, 0.7, 0.9, 1)) # Similar to subsample in XGBoost

fwrite(cb_param_sets, "./3_intermediate/hyperparameters/cb_hyperparameters.csv")


# RUNNING MODEL SELECTION PROCEDURE ---------------------------

# To run the model selection we will launch multiple
#. jobs onto the server, each one training a subset of the
#. model configurations and saving the resulting RMSPEs
#. over the 3 repeats of 5-fold CV
#. These jobs will call 03_train_base_models.R
#. with arguments to specify which model configurations
#. to develop

## EN -----
# 1051 hyperparam sets - done
# Rscript --vanilla /home/wanjinli/iron-ml-ext-val/2_scripts/03_train_base_mods.R "EN" "predict_hgb" "data_hgb_only" 1 1051 0
# Rscript --vanilla /home/wanjinli/iron-ml-ext-val/2_scripts/03_train_base_mods.R "EN" "predict_ferr" "data_hgb_only" 1 1051 0
# Rscript --vanilla /home/wanjinli/iron-ml-ext-val/2_scripts/03_train_base_mods.R "EN" "predict_hgb" "data_hgb_ferr" 1 1051 0
# Rscript --vanilla /home/wanjinli/iron-ml-ext-val/2_scripts/03_train_base_mods.R "EN" "predict_ferr" "data_hgb_ferr" 1 1051 0

## RF -----
# 448 hyperparam sets
# Done
# Rscript --vanilla /home/wanjinli/iron-ml-ext-val/2_scripts/03_train_base_mods.R "RF" "predict_hgb" "data_hgb_only" 1 448 0
# Rscript --vanilla /home/wanjinli/iron-ml-ext-val/2_scripts/03_train_base_mods.R "RF" "predict_ferr" "data_hgb_only" 1 448 0
# Rscript --vanilla /home/wanjinli/iron-ml-ext-val/2_scripts/03_train_base_mods.R "RF" "predict_hgb" "data_hgb_ferr" 1 448 0
# Rscript --vanilla /home/wanjinli/iron-ml-ext-val/2_scripts/03_train_base_mods.R "RF" "predict_ferr" "data_hgb_ferr" 1 448 0

## XGB -----

# 4800 hyperparam sets
# split the jobs by subsetting hyperparam sets; use 36 cores only; parallel computing is slower

# Done
# Rscript --vanilla /home/wanjinli/iron-ml-ext-val/2_scripts/03_train_base_mods.R "XGB" "predict_hgb" "data_hgb_only" 1 4800 0
# Rscript --vanilla /home/wanjinli/iron-ml-ext-val/2_scripts/03_train_base_mods.R "XGB" "predict_ferr" "data_hgb_only" 1 4800 0
# Rscript --vanilla /home/wanjinli/iron-ml-ext-val/2_scripts/03_train_base_mods.R "XGB" "predict_hgb" "data_hgb_ferr" 1 4800 0
# Rscript --vanilla /home/wanjinli/iron-ml-ext-val/2_scripts/03_train_base_mods.R "XGB" "predict_ferr" "data_hgb_ferr" 1 4800 0

## CB -----

# Running
# Rscript --vanilla /home/wanjinli/iron-ml-ext-val/2_scripts/03_train_base_mods.R "CB" "predict_hgb" "data_hgb_only" 1 2 0






# RUNNING NESTED CV VERSION OF MODEL SELECTION PROCEDURE
#. This is identical to the original model selection procedure except
#. repeated 3 times, one in each of the outer CV folds with 2/3 of the data.




# NOTE:
# Gradient boost takes way longer: 20 jobs x 40 hyperparam per job = 2-3 days
# Elastic net fast: all hyperparams in under 2 hours (TEST TODAY)

# Syntax for computing predicted outcome with direct engines.

#Algorithm	                  Package	  Code
# Random Forest	              ranger	  predict(obj)$predictions
# Gradient boosting machine	  gbm	      predict(obj, type = "response", n.trees)
# Elastic net	                glmnet	  predict(obj, newx, type = "response")
# Catboost





## CHEN-YANG CODE FROM HERE DOWN, TO BE EDITED/INTEGRATED INTO ABOVE SCAFFOLDING

# Save cv split function ----
save_cv_split <- function(df, file_path) {
  set.seed(250)
  
  # Nested cross-validation ----
  # nested cross-validation: 5 outer folds (each fold with 5-fold CV) 
  # then repeat 3 times = 5x5x3 = 75 total validation sets
  
  # https://cran.r-project.org/web/packages/rsample/rsample.pdf
  # reference for vfold_cv and nested_cv
  rsplit_res <- nested_cv(df,
                          outside = vfold_cv(v=5, repeats = 3),
                          inside = vfold_cv(v=5)) # repeats=2  does resampling on inner loop twice
  saveRDS(rsplit_res, file_path)
}

for (dat_path in dat_paths) {
  dt.md <- fread(dat_path)  #"./3_intermediate/private/hgb_ferr_rise.csv")
  # dt.md <- fread( "./3_intermediate/private/hgb_ferr_rise.csv")
  # dt.md <- fread( "./3_intermediate/private/hgb_only_rise.csv")
  
  str(dt.md)
  
  # set Gender_F as character since loads in as int
  dt.md$sex <- as.character(dt.md$sex)
  
  ## Remove race variable ----
  identifiers <- c("RandID", "VisitDate", "VisitNum", 
                   "rbc_loss_in_ml",
                   "weight", "height", "bmi", "ebv", "red_cell_volume", "percent_rbc_loss", 
                   "race")
  
  #remove extraneous fields
  dt.md[, c(identifiers) := NULL]
  
  # 2 prediction tasks: predict hgb, predict ferritin
  dt.md.hgb <- subset(dt.md, select = -c(fu_ferritin, fu_log_ferritin))  # predict hgb so remove ferr outcomes
  dt.md.ferr <- subset(dt.md, select = -c(fu_hgb, fu_ferritin))  # predict log ferritin so remove hgb outcome and ferr outcome
  
  # Create 2 versions: one with characters as factors and
  # one with categorical variables 1-hot encoded for model types that require that
  
  # One hot encoding for XGB -----
  # Create dummy one-hot variables for categoricals
  dt.OH.hgb <- data.table(model.matrix(fu_hgb ~ ., data=dt.md.hgb))
  dt.OH.hgb <- cbind(dt.OH.hgb, "fu_hgb" = dt.md$fu_hgb)
  
  dt.OH.ferr <- data.table(model.matrix(fu_log_ferritin ~ ., data=dt.md.ferr))
  dt.OH.ferr <- cbind(dt.OH.ferr, "fu_log_ferritin" = dt.md$fu_log_ferritin)
  
  # library(lares)
  # lares::corr_cross(dt.OH.hgb)  # Look at variance in predictors, near-zero variance
  # lares::corr_cross(dt.OH.ferr)  # Look at variance in predictors, near-zero variance
  
  # Code all Character as Factor -----
  colnames(dt.md.hgb)[unlist(dt.md.hgb[ , lapply(.SD, is.character),][1,])]
  dt.md.hgb <- dt.md.hgb %>% mutate_if(is.character, as.factor)
  str(dt.md.hgb)
  
  colnames(dt.md.ferr)[unlist(dt.md.ferr[ , lapply(.SD, is.character),][1,])]
  dt.md.ferr <- dt.md.ferr %>% mutate_if(is.character, as.factor)
  str(dt.md.ferr)
  
  # CV SPLIT (Hgb) ----
  if (dat_path == "./3_intermediate/private/hgb_ferr_rise.csv") {
    
    # save character as factor dataframes
    save_cv_split(dt.md.hgb, 
                  "./3_intermediate/rsplits/main_model/pred_hgb/rsplit_factors_hgb_ferr.rds")
    save_cv_split(dt.md.ferr, 
                  "./3_intermediate/rsplits/main_model/pred_ferr/rsplit_factors_hgb_ferr.rds")
    
    # save one hot encoding dataframes
    save_cv_split(dt.OH.hgb, 
                  "./3_intermediate/rsplits/main_model/pred_hgb/rsplit_OH_hgb_ferr.rds")
    save_cv_split(dt.OH.ferr, 
                  "./3_intermediate/rsplits/main_model/pred_ferr/rsplit_OH_hgb_ferr.rds")
    
    # Save the full training sets as CSV
    fwrite(dt.md.hgb, "./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_factors_hgb_ferr.csv")
    fwrite(dt.md.ferr, "./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_factors_hgb_ferr.csv")
    fwrite(dt.OH.hgb, "./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_OH_hgb_ferr.csv")
    fwrite(dt.OH.ferr, "./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_OH_hgb_ferr.csv")
    
    # features_list<-list(
    #   "data_hgb_ferr_factor_hgb" = dt.md.hgb,
    #   "data_hgb_ferr_OH_hgb" = dt.OH.hgb,
    #   "data_hgb_ferr_factor_ferr" = dt.md.ferr,
    #   "data_hgb_ferr_OH_ferr" = dt.OH.ferr
    # )
    # saveRDS(features_list, "./1_data/model_dev_data/data_hgb_ferr/mdset_features_list.RDS")
  }
  
  else if (dat_path == "./3_intermediate/private/hgb_only_rise.csv") {
    
    # save character as factor dataframes
    save_cv_split(dt.md.hgb, 
                  "./3_intermediate/rsplits/main_model/pred_hgb/rsplit_factors_hgb_only.rds")
    save_cv_split(dt.md.ferr, 
                  "./3_intermediate/rsplits/main_model/pred_ferr/rsplit_factors_hgb_only.rds")
    
    # save one hot encoding dataframes
    save_cv_split(dt.OH.hgb, 
                  "./3_intermediate/rsplits/main_model/pred_hgb/rsplit_OH_hgb_only.rds")
    save_cv_split(dt.OH.ferr, 
                  "./3_intermediate/rsplits/main_model/pred_ferr/rsplit_OH_hgb_only.rds")
    
    # Save the full training sets as CSV
    fwrite(dt.md.hgb, "./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_factors_hgb_only.csv")
    fwrite(dt.md.ferr, "./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_factors_hgb_only.csv")
    fwrite(dt.OH.hgb, "./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_OH_hgb_only.csv")
    fwrite(dt.OH.ferr, "./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_OH_hgb_only.csv")
  
    # features_list<-list(
    #   "data_hgb_only_factor_hgb" = dt.md.hgb,
    #   "data_hgb_only_OH_hgb" = dt.OH.hgb,
    #   "data_hgb_only_factor_ferr" = dt.md.ferr,
    #   "data_hgb_only_OH_ferr" = dt.OH.ferr
    # )
    # saveRDS(features_list, "./1_data/model_dev_data/data_hgb_only/mdset_features_list.RDS")
  }
}

# caret::varImp() function can be used to plot the importance metrics


## Saving performance metrics into a file for each fold and repeat
#method - alpha - lambda - folds - repeat - RMSE - R^2 - MSE - Outcome

# folds <- 2 #change
# repeats<-2 #change
# 
# tr_en_hgb <- df[FALSE,]
# col_names<- c("Model", "Outcomes", "Alpha", "Lambda", "Repeat", "Fold", "RMSE", "R^2", "MSE")
# colnames(tr_en_hgb)<-col_names
# Method <- rep("Elastinet", (folds*repeats))
# Outcome <- rep("HGB", (folds*repeats))


#### check 1_data/ml_output_example for how to output results

#fwrite(tr_gbm_hgb, "./1_data/model_dev_data/tune_results_gbm_HGB.csv")
#fwrite(tr_gbm_fer, "./1_data/model_dev_data/tune_results_gbm_fer.csv")
#fwrite(tr_rf_hgb, "./1_data/model_dev_data/tune_results_rf_HGB.csv")
#fwrite(tr_rf_fer, "./1_data/model_dev_data/tune_results_rf_fer.csv")
#fwrite(tr_en_hgb, "./1_data/model_dev_data/tune_results_en_HGB.csv")
#fwrite(tr_en_fer, "./1_data/model_dev_data/tune_results_en_fer.csv")
