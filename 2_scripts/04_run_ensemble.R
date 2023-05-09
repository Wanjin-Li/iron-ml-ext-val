library(data.table)
library(pROC)
library(ggplot2)
library(readxl)
library(scales)
library(dplyr)
library(ggforce)
library(rsample)
library(xgboost)
library(randomForest)
library(gridExtra)
library(glmnet)
library(purrr)
theme_set(theme_bw())
source("./2_scripts/utility_functions.R")

intermediate_directory <- './3_intermediate/ensemble'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}

# Run ensemble assessment ----
args = commandArgs(trailingOnly=TRUE)  # for taking in inputs
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}

i <- as.integer(args[1])  # input from bash script

## Predict hgb ----
base_mod_spec_hgb_ferr_predict_hgb <- readRDS("./3_intermediate/ensemble/base_mod_spec_hgb_ferr_predict_hgb.RDS")
base_mod_spec_hgb_only_predict_hgb <- readRDS("./3_intermediate/ensemble/base_mod_spec_hgb_only_predict_hgb.RDS")

## Predict ferritin ----
base_mod_spec_hgb_ferr_predict_ferr <- readRDS("./3_intermediate/ensemble/base_mod_spec_hgb_ferr_predict_ferr.RDS")
base_mod_spec_hgb_only_predict_ferr <- readRDS("./3_intermediate/ensemble/base_mod_spec_hgb_only_predict_ferr.RDS")

list_of_specs <- list(base_mod_spec_hgb_ferr_predict_hgb, 
                      base_mod_spec_hgb_only_predict_hgb,
                      base_mod_spec_hgb_ferr_predict_ferr,
                      base_mod_spec_hgb_only_predict_ferr)

list_of_configs <- c("5XGB1RF", "5XGB1RF", "5EN1RF", "6RF")

list_of_versions <- c("hgb_ferr_predict_hgb", 
                      "hgb_only_predict_hgb",
                      "hgb_ferr_predict_ferr",
                      "hgb_only_predict_ferr")

print(list_of_specs[[i]])
print(list_of_configs[i])
print(list_of_versions[i])


run_ensemble_assess(base_model_specs = list_of_specs[[i]], 
                    ensemble_config = list_of_configs[i],
                    version=list_of_versions[i])





