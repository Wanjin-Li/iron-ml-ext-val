# This script trains the base model using nested cross-validation 
# to obtain an unbiased estimate of model performance.


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


# Parse command line arguments ----
args = commandArgs(trailingOnly=TRUE)  # for taking in inputs

if (length(args)<6) {
  stop("All six arguments must be entered into command line separated by spaces (see preamble of 03_train_base_models.R)", call.=FALSE)
}

mod_name <- args[1]  # input from command line: "RF", "EN", "XGB", "CB" (catboost)
predict_biomarkers <- args[2]  # input from command line: "predict_hgb", "predict_ferr"
train_biomarkers <- args[3]  # input from command line: "data_hgb_only", "data_hgb_ferr"
start <- as.integer(args[4])  # input from command line
end <- as.integer(args[5])  # input from command line
outer_fold <- as.integer(args[6]) # input from command line

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

# Load rsplit training dataset depending on biomarkers in data
if (predict_biomarkers == "predict_hgb") {  # specify which biomarker to predict: hgb
  if (train_biomarkers == "data_hgb_only") {
    if (mod_name == "XGB") {  # for XGB, use data where categorical values are one hot encoded
      if (outer_fold == 1) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_OH_hgb_only_outer_fold_1.rds")
      } else if (outer_fold == 2) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_OH_hgb_only_outer_fold_2.rds")
      } else if (outer_fold == 3) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_OH_hgb_only_outer_fold_3.rds")
      }
    } else {  # for other models, use data where categorical values are coded as factor
      if (outer_fold == 1) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_factors_hgb_only_outer_fold_1.rds")
      } else if (outer_fold == 2) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_factors_hgb_only_outer_fold_2.rds")
      } else if (outer_fold == 3) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_factors_hgb_only_outer_fold_3.rds")
      }
    }
  } else if (train_biomarkers == "data_hgb_ferr") {  # predict ferritin biomarker
    if (mod_name == "XGB") {  # for XGB, use data where categorical values are one hot encoded
      if (outer_fold == 1) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_OH_hgb_ferr_outer_fold_1.rds")
      } else if (outer_fold == 2) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_OH_hgb_ferr_outer_fold_2.rds")
      } else if (outer_fold == 3) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_OH_hgb_ferr_outer_fold_3.rds")
      }
    } else {  # for other models, use data where categorical values are coded as factor
      if (outer_fold == 1) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_factors_hgb_ferr_outer_fold_1.rds")
      } else if (outer_fold == 2) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_factors_hgb_ferr_outer_fold_2.rds")
      } else if (outer_fold == 3) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_factors_hgb_ferr_outer_fold_3.rds")
      }
    }
  }
} else if (predict_biomarkers == "predict_ferr") {
  if (train_biomarkers == "data_hgb_only") {
    if (mod_name == "XGB") {  # for XGB, use data where categorical values are one hot encoded
      if (outer_fold == 1) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_OH_hgb_only_outer_fold_1.rds")
      } else if (outer_fold == 2) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_OH_hgb_only_outer_fold_2.rds")
      } else if (outer_fold == 3) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_OH_hgb_only_outer_fold_3.rds")
      }
    } else {  # for other models, use data where categorical values are coded as factor
      if (outer_fold == 1) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_factors_hgb_only_outer_fold_1.rds")
      } else if (outer_fold == 2) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_factors_hgb_only_outer_fold_2.rds")
      } else if (outer_fold == 3) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_factors_hgb_only_outer_fold_3.rds")
      }
    }
  } else if (train_biomarkers == "data_hgb_ferr") {
    if (mod_name == "XGB") {  # for XGB, use data where categorical values are one hot encoded
      if (outer_fold == 1) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_OH_hgb_ferr_outer_fold_1.rds")
      } else if (outer_fold == 2) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_OH_hgb_ferr_outer_fold_2.rds")
      } else if (outer_fold == 3) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_OH_hgb_ferr_outer_fold_3.rds")
      }
    } else {  # for other models, use data where categorical values are coded as factor
      if (outer_fold == 1) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_factors_hgb_ferr_outer_fold_1.rds")
      } else if (outer_fold == 2) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_factors_hgb_ferr_outer_fold_2.rds")
      } else if (outer_fold == 3) {
        dt_split <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_factors_hgb_ferr_outer_fold_3.rds")
      }
    }
  }
}

# File name for outputing results
fname_results <- paste0("./3_intermediate/tune_results/nested_model/inner_fold/base_mod_tune_", 
                        predict_biomarkers,
                        "_",
                        train_biomarkers,
                        "_",
                        ifelse(outer_fold==0,
                               paste0(mod_name,
                               "_",
                               "params",start,"_to_",end, ".csv"),
                               paste0("outer_fold_", outer_fold, 
                                      "_", 
                                      mod_name,
                                      "_",
                                      "params",start,"_to_",end, ".csv")
                               ))

# Train models ----
tune_subset(mod_name=mod_name, 
            param_sets=param_sets, 
            dt_split=dt_split,
            fname_results=fname_results,
            start=start, 
            end=end
)

# RUNNING MODEL SELECTION PROCEDURE ---------------------------

# To run the model selection we will launch multiple
#. jobs onto the server, each one training a subset of the
#. model configurations and saving the resulting RMSPEs
#. over the 3 repeats of 5-fold CV
#. These jobs will call 03_train_base_models.R
#. with arguments to specify which model configurations
#. to develop

### EN -----
# 1051 hyperparam sets
# Rscript --vanilla /home/wanjinli/iron-ml-ext-val/2_scripts/03_train_nested_inner_fold_mods.R "EN" "predict_hgb" "data_hgb_only" 1 1051 1

### RF -----
# 448 hyperparam sets
# Rscript --vanilla /home/wanjinli/iron-ml-ext-val/2_scripts/03_train_nested_inner_fold_mods.R "RF" "predict_hgb" "data_hgb_only" 1 448 1


### XGB -----
# 4800 hyperparam sets
# Rscript --vanilla /home/wanjinli/iron-ml-ext-val/2_scripts/03_train_nested_inner_fold_mods.R "XGB" "predict_hgb" "data_hgb_only" 1 4800 1

### CB -----
# 1260 hyperparam sets
# Rscript --vanilla /home/wanjinli/iron-ml-ext-val/2_scripts/03_train_nested_inner_fold_mods.R "CB" "predict_hgb" "data_hgb_only" 1 1260 1
