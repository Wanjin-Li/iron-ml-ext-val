# This script performs the sensivity analysis that retrains the hyperparameter configurations selected in the original analysis using the full RISE dataset
# but for individuals with > 1 donation, only a single randomly selected donation is included.


library(data.table)
library(rsample)
library(ggplot2)
library(tidyverse)
library(stringr)
source('https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R')
theme_set(theme_bw())
source("./2_scripts/utility_functions.R")


intermediate_directory <- './3_intermediate/sensitivity_analysis'  # directory to store sensitivity analysis results
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}

intermediate_directory <- './3_intermediate/sensitivity_analysis/results'  # directory to store sensitivity analysis results
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}

intermediate_directory <- './3_intermediate/sensitivity_analysis/results/rise'  # directory to store sensitivity analysis results
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}


intermediate_directory <- './3_intermediate/sensitivity_analysis/results/rise/nested_model'  # directory to store sensitivity analysis results for sanbs
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}

args = commandArgs(trailingOnly=TRUE)  # for taking in inputs
if (length(args)<3) {
  stop("All 3 arguments must be supplied (input file).n", call.=FALSE)
}

# Define input arguments
predict_outcome <- args[1]  # input from command line: "predict_hgb", "predict_ferr"
train_version <- args[2]  # input from command line: "hgb_only", "hgb_ferr"
outer_fold_name <- args[3] # input from command line: "outer_fold_1", "outer_fold_2", "outer_fold_3"

## load rise data ----
outer_fold_test_dt_path <- "./3_intermediate/private/single_donation_data/rise/outer_fold_test_data/"

if (predict_outcome == "predict_hgb"){
  
  test_data_factor <- fread(paste0(outer_fold_test_dt_path, "pred_hgb/test_factors_", train_version, "_", outer_fold_name, ".csv"))
  test_data_oh <- fread(paste0(outer_fold_test_dt_path, "pred_hgb/test_OH_", train_version, "_", outer_fold_name, ".csv"))

} else if (predict_outcome == "predict_ferr"){
  
  test_data_factor <- fread(paste0(outer_fold_test_dt_path, "pred_ferr/test_factors_", train_version, "_", outer_fold_name, ".csv"))
  test_data_oh <- fread(paste0(outer_fold_test_dt_path, "pred_ferr/test_OH_", train_version, "_", outer_fold_name, ".csv"))
}

test_data_factor <- mutate_if(test_data_factor, is.character, as.factor)

test_data_factor <- test_data_factor[, -"RandID"]
test_data_oh <- test_data_oh[, -"RandID"]

# helper function to validate model ----
validate_single_model <- function(test_data, fitted_model, mod_name){
  
  # change response variable to "fu_outcome" for easier coding below
  lookup <- c(fu_outcome = "fu_hgb", fu_outcome = "fu_log_ferritin")
  test_data <- test_data %>% rename(any_of(lookup))
  
  if (mod_name == "RF"){  # RANDOM FOREST
    preds <- as.data.table(predict(fitted_model, test_data))  # generate predictions
    
  } else if (mod_name == "EN"){  # ELASTIC NET (NO INTERACTIONS)
    X_test <- data.matrix(test_data[ , 1:(ncol(test_data)-1)])  # remove last column (outcome)
    preds <- as.data.table(predict(fitted_model, newx = X_test))  # generate predictions
    
  } else if (mod_name == "XGB"){  # XGB GRADIENT BOOSTED MACHINE
    # Extract test set in xgb's special format
    xgb.test = xgb.DMatrix(data = as.matrix(test_data[,-"fu_outcome"]),  # fu_hgb, fu_log_ferritin
                           label = as.matrix(test_data[,"fu_outcome"]))  # fu_hgb, fu_log_ferritin
    preds <- as.data.table(predict(fitted_model, xgb.test, reshape = T))
    
  } else if (mod_name == "CB"){ # CB CATBOOST
    x_test <- test_data[, -"fu_outcome"]
    y_test <- test_data$fu_outcome
    
    test_pool <- catboost.load_pool(data = x_test, label = y_test)
    
    # Predict
    preds <- as.data.table(catboost.predict(fitted_model,
                                            test_pool,
                                            prediction_type = 'RawFormulaVal',
                                            thread_count=24, # specify the number of threads to use
                                            verbose = FALSE)) 
  }
  colnames(preds) <- paste0("prediction")
  fu_outcome <- unlist(test_data[, "fu_outcome"])  # fu_hgb, fu_log_ferritin
  results <- cbind(preds, fu_outcome)
  return(results)
}



# Uses helper function validate_single_model to get sensitivity analysis results
load_model_and_predict <- function(version, res_dir, fold_name) {
  pattern_spec <- c(version, fold_name)
  model_paths <- list.files(path="./3_intermediate/trained_models/updates/nested_model", pattern=paste0(pattern_spec, collapse = ".+"), full.names = TRUE) 
  print(model_paths)
  for (path in model_paths) {  # for each model in ensemble
    base_model_parts <- strsplit(path, "_")[[1]]
    base_model_name <- base_model_parts[9]
    mod_name <- str_split(base_model_name, "[.]")[[1]][1]  # "XGB"
    
    fitted_model <- readRDS(path)  # load model
    
    # Decide which test dataset (One hot or factors to use) 
    if (mod_name == "XGB") {
      test_data <- test_data_oh
    } else {
      test_data <- test_data_factor
    }
    
    # Validate base model
    results <- validate_single_model(test_data, fitted_model, mod_name)
    
    file_name <- str_split(path, "[/]")[[1]][6]  # "hgb_ferr_predict_hgb_5XGB1RF_RF.208.rds"
    file_name <- paste0(gsub(".rds", "", file_name), ".csv")  # remove ".rds" and add ".csv" -> # "hgb_ferr_predict_hgb_5XGB1RF_RF.208.csv"
    fwrite(results, paste0(res_dir, file_name)) # "./3_intermediate/sensitivity_analysis/results/rise/nested_modelhgb_ferr_predict_hgb_5XGB1RF_RF.208.csv"
  }
}

if (predict_outcome == "predict_hgb"){
  if (train_version == "hgb_only"){
    
    load_model_and_predict(version = "hgb_only_predict_hgb",
                           res_dir='./3_intermediate/sensitivity_analysis/results/rise/nested_model/',
                           fold_name = outer_fold_name)
  } else if (train_version == "hgb_ferr"){
    load_model_and_predict(version = "hgb_ferr_predict_hgb",
                           res_dir='./3_intermediate/sensitivity_analysis/results/rise/nested_model/',
                           fold_name = outer_fold_name)
  }
} else if (predict_outcome == "predict_ferr"){
  if (train_version == "hgb_only"){
    load_model_and_predict(version = "hgb_only_predict_ferr",
                           res_dir='./3_intermediate/sensitivity_analysis/results/rise/nested_model/',
                           fold_name = outer_fold_name)
  } else if (train_version == "hgb_ferr"){
    load_model_and_predict(version = "hgb_ferr_predict_ferr",
                           res_dir='./3_intermediate/sensitivity_analysis/results/rise/nested_model/',
                           fold_name = outer_fold_name)
  }
} 

# Rscript --vanilla /home/wanjinli/iron-ml-ext-val/2_scripts/11_rise_internal_validation_single_donation.R "predict_hgb" "hgb_only" "outer_fold_1"

