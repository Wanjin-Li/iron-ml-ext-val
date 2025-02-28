# This script develops ensemble models using top-performing models selected from regular cross-validation.

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

intermediate_directory <- './3_intermediate/ensemble/updates'
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
### data hgb only ----
#### ensemble 1 ----
em1_base_mod_spec_hgb_only_predict_hgb <- readRDS("./3_intermediate/ensemble/updates/em1_base_mod_spec_hgb_only_predict_hgb.RDS")

#### ensemble 2 ----
em2_base_mod_spec_hgb_only_predict_hgb <- readRDS("./3_intermediate/ensemble/updates/em2_base_mod_spec_hgb_only_predict_hgb.RDS")

### data hgb ferr ----
#### ensemble 1 ----
em1_base_mod_spec_hgb_ferr_predict_hgb <- readRDS("./3_intermediate/ensemble/updates/em1_base_mod_spec_hgb_ferr_predict_hgb.RDS")

#### ensemble 2 ----
em2_base_mod_spec_hgb_ferr_predict_hgb <- readRDS("./3_intermediate/ensemble/updates/em2_base_mod_spec_hgb_ferr_predict_hgb.RDS")


## Predict ferr ----
### data hgb only ----
#### ensemble 1 ----
em1_base_mod_spec_hgb_only_predict_ferr <- readRDS("./3_intermediate/ensemble/updates/em1_base_mod_spec_hgb_only_predict_ferr.RDS")

#### ensemble 2 ----
em2_base_mod_spec_hgb_only_predict_ferr <- readRDS("./3_intermediate/ensemble/updates/em2_base_mod_spec_hgb_only_predict_ferr.RDS")

### data hgb ferr ----
#### ensemble 1 ----
em1_base_mod_spec_hgb_ferr_predict_ferr <- readRDS("./3_intermediate/ensemble/updates/em1_base_mod_spec_hgb_ferr_predict_ferr.RDS")

#### ensemble 2 ----
em2_base_mod_spec_hgb_ferr_predict_ferr <- readRDS("./3_intermediate/ensemble/updates/em2_base_mod_spec_hgb_ferr_predict_ferr.RDS")



list_of_specs <- list(em1_base_mod_spec_hgb_only_predict_hgb,
                      em1_base_mod_spec_hgb_ferr_predict_hgb,
                      em1_base_mod_spec_hgb_only_predict_ferr,
                      em1_base_mod_spec_hgb_ferr_predict_ferr,
                      em2_base_mod_spec_hgb_only_predict_hgb,
                      em2_base_mod_spec_hgb_ferr_predict_hgb,
                      em2_base_mod_spec_hgb_only_predict_ferr,
                      em2_base_mod_spec_hgb_ferr_predict_ferr)

list_of_configs <- rep(c("ensemble1", "ensemble2"), each = 4)


list_of_versions <- rep(c("hgb_only_predict_hgb",
                      "hgb_ferr_predict_hgb",
                      "hgb_only_predict_ferr",
                      "hgb_ferr_predict_ferr"), 2)
  
  

print(list_of_specs[[i]])
print(list_of_configs[i])
print(list_of_versions[i])


run_ensemble_assess(base_model_specs = list_of_specs[[i]], 
                    ensemble_config = list_of_configs[i],
                    version=list_of_versions[i],
                    ensemble_type="average")

# Rscript --vanilla /home/wanjinli/iron-ml-ext-val/2_scripts/04_run_ensemble.R 1 # 1, 2, 3, 4, 5, 6, 7, 8




