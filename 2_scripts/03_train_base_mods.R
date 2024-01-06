# Meant to be run via command line!
#
# This script takes arguments from the command line/terminal to train the 
#. base (non-ensembled) ML models. User must specify the following
#. arguments in order:
#. 
#. mod_name:  "RF" for random forest
#             "EN" for elasticnet
#             "XGB" for gradient boosted machine
#             "CB" for catboost
#
#. predict_biomarkers: "predict_hgb" for predicting hemoglobin
#                    "predict_ferr" for predicting ferritin
#
#. train_biomarkers: "data_hgb_only" for training on hemoglobin only
#                    "data_hgb_ferr" for training on hemoglobin and ferritin
#
#. hyperparam_config_start: integer specifying the starting index of the 
#      hyperparameter set to train (corresponds to row of the csv files)
#
#. hyperparam_config_end: integer specifying the ending index of the
#      hyperparameter set to train (corresponds to row of the csv files)
#
#. outer_fold: 0 for  regular cross validation on whole data
#.             1, 2, or 3 for outer CV folds 1, 2, or 3


# Here is an example for what could be entered into command line on server
#.  to run this script
#.  will train elasticnet models with hyperparameter configs 1 throgh
#.  5 predicting Hb using the 'Hb only data with regular (not nested) CV
#
# Rscript --vanilla 03_train_base_models.R "EN" "predict_hgb" "data_hgb_only" 1 5 0


# Parse command line arguments ----
args = commandArgs(trailingOnly=TRUE)  # for taking in inputs

if (length(args)<5) {
  stop("All five arguments must be entered into command line separated by spaces (see preamble of 03_train_base_models.R)", call.=FALSE)
}

mod_name <- args[1]  # input from command line: "RF", "EN", "XGB", "CB" (catboost)
predict_biomarkers <- args[2]  # input from command line: "predict_hgb", "predict_ferr"
train_biomarkers <- args[3]  # input from command line: "data_hgb_only", "data_hgb_ferr"
start <- as.integer(args[4])  # input from command line
end <- as.integer(args[5])  # input from command line
outer_fold <- as.integer(args[6])

# Load functions/data

# load in utility functions for model training
source('./2_scripts/utility_functions.R')  # using run_mod_assess, tune_subset, cv_config, mod_eval helper functions`

# Load hyperparameters depending on model type ----
if (mod_name == "RF") {
  param_sets <- fread("./3_intermediate/hyperparameters/rf_hyperparameters.csv")
} else if (mod_name == "EN"){
  param_sets <- fread("./3_intermediate/hyperparameters/en_hyperparameters.csv")
} else if (mod_name == "XGB") {
  param_sets <- fread("./3_intermediate/hyperparameters/xgb_hyperparameters.csv")
} else if (mod_name == "CB") {
  param_sets <- fread("./3_intermediate/hyperparameters/cb_hyperparameters.csv")
}

# !!! JENNIFER: THIS WILL NEED UPDATING BASED ON HOW YOU ARE DOING THE DATA 
#.  and to use the right outer fold version

# Load rsplit training dataset depending on biomarkers in data
if (predict_biomarkers == "predict_hgb") {  # specify which biomarker to predict: hgb
  if (train_biomarkers == "data_hgb_only") {
    if (mod_name == "XGB") {  # for XGB, use data where categorical values are one hot encoded
      dt_split <- readRDS("./3_intermediate/rsplits/main_model/pred_hgb/rsplit_OH_hgb_only.rds")
    } else {  # for other models, use data where categorical values are coded as factor
      dt_split <- readRDS("./3_intermediate/rsplits/main_model/pred_hgb/rsplit_factors_hgb_only.rds")
    }
  } else if (train_biomarkers == "data_hgb_ferr") {  # predict ferritin biomarker
    if (mod_name == "XGB") {  # for XGB, use data where categorical values are one hot encoded
      dt_split <- readRDS("./3_intermediate/rsplits/main_model/pred_hgb/rsplit_OH_hgb_ferr.rds")
    } else {  # for other models, use data where categorical values are coded as factor
      dt_split <- readRDS("./3_intermediate/rsplits/main_model/pred_hgb/rsplit_factors_hgb_ferr.rds")
    }
  }
} else if (predict_biomarkers == "predict_ferr") {
  if (train_biomarkers == "data_hgb_only") {
    if (mod_name == "XGB") {  # for XGB, use data where categorical values are one hot encoded
      dt_split <- readRDS("./3_intermediate/rsplits/main_model/pred_ferr/rsplit_OH_hgb_only.rds")
    } else {  # for other models, use data where categorical values are coded as factor
      dt_split <- readRDS("./3_intermediate/rsplits/main_model/pred_ferr/rsplit_factors_hgb_only.rds")
    }
  } else if (train_biomarkers == "data_hgb_ferr") {
    if (mod_name == "XGB") {  # for XGB, use data where categorical values are one hot encoded
      dt_split <- readRDS("./3_intermediate/rsplits/main_model/pred_ferr/rsplit_OH_hgb_ferr.rds")
    } else {  # for other models, use data where categorical values are coded as factor
      dt_split <- readRDS("./3_intermediate/rsplits/main_model/pred_ferr/rsplit_factors_hgb_ferr.rds")
    }
  }
  
}

# File name for outputing results
fname_results <- paste0("./3_intermediate/tune_results/base_mod_tune_", 
                        predict_biomarkers,
                        "_",
                        train_biomarkers,
                        "_",
                        mod_name,
                        "_",
                        "params",start,"_to_",end,
                        ifelse(outer_fold==0, ".csv", 
                               paste0("_outer_fold_",outer_fold,".csv")))

# Train models ----
tune_subset(mod_name=mod_name, 
            param_sets=param_sets, 
            dt_split=dt_split,
            fname_results=fname_results,
            start=start, 
            end=end
            )
#Train subset will output a csv file with the RMSPE for each hyperparameter
#. set for each fold of (nested or regular) cross validation. 
