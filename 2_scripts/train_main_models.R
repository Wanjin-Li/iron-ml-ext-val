# This script performs ML model training and takes inputs from the bash
# script to specify the model and task (predict hgb or predict ferritin) to train

# load in utility functions for model training
source('./2_scripts/utility_functions.R')  # using run_mod_assess, tune_subset, cv_config, mod_eval helper functions

# Input commands setup ----
args = commandArgs(trailingOnly=TRUE)  # for taking in inputs
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}

mod_name <- args[1]  # input from bash script: "RF", "EN", "XGB"
predict_biomarkers <- args[2]  # input from bash script: "predict_hgb", "predict_ferr"
train_biomarkers <- args[3]  # input from bash script: "data_hgb_only", "data_hgb_ferr"
start <- as.integer(args[4])  # input from bash script
end <- as.integer(args[5])  # input from bash script


# Load hyperparameters depending on model type ----
if (mod_name == "RF") {
  param_sets <- fread("./3_intermediate/hyperparameters/rf_hyperparameters.csv")
} else if (mod_name == "EN"){
  param_sets <- fread("./3_intermediate/hyperparameters/en_hyperparameters.csv")
} else if (mod_name == "XGB") {
  param_sets <- fread("./3_intermediate/hyperparameters/xgb_hyperparameters.csv")
}

# Load rsplit training dataset depending on biomarkers in data ----
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

fname_results <- paste0("./3_intermediate/tune_results/main_model/", 
                        predict_biomarkers,
                        "/",
                        train_biomarkers,
                        "/",
                        mod_name,
                        "_")

# Train models ----
if (mod_name == "EN") {
  run_mod_assess(mod_name=mod_name, 
                 param_sets=param_sets, 
                 dt_split=dt_split,
                 idx_start=1,
                 fname_results=fname_results)
  
  } else {  # RF, XGB
    tune_subset(mod_name=mod_name, 
                param_sets=param_sets, 
                dt_split=dt_split,
                fname_results=fname_results,
                start=start, 
                end=end)  
}

               


