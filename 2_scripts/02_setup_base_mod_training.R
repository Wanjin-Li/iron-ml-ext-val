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

# Catboost - TO BE ADDED (check with Sophie)



# RUNNING MODEL SELECTION PROCEDURE ---------------------------

# To run the model selection we will launch multiple
#. jobs onto the server, each one training a subset of the
#. model configurations and saving the resulting RMSPEs
#. over the 3 repeats of 5-fold CV
#. These jobs will call 03_train_base_models.R
#. with arguments to specify which model configurations
#. to develop







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
                          outside = vfold_cv(v=5, repeats=3),
                          inside = vfold_cv(v=5))
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
