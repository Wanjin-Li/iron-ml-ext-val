# This script develops ensemble models using top-performing models selected from nested cross-validation.

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

intermediate_directory <- './3_intermediate/ensemble/updates/nested_model'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}

# Run ensemble assessment ----
args = commandArgs(trailingOnly=TRUE)  # for taking in inputs
if (length(args)<4) {
  stop("All 4 arguments must be supplied (input file).n", call.=FALSE)
}


ensemble_name <- args[1]  # input from command line: "ensemble1", "ensemble2"
predict_outcome <- args[2]  # input from command line: "predict_hgb", "predict_ferr"
train_version <- args[3]  # input from command line: "data_hgb_only", "data_hgb_ferr"
outer_fold_name <- as.integer(args[4]) # input from command line


# load rsplit training dataset for corresponding ensemble
if (ensemble_name == "ensemble1"){
  if (predict_outcome == "predict_hgb"){
    if (train_version == "data_hgb_only"){
      data_version <- "hgb_only_predict_hgb"
      if (outer_fold_name == 1){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble1_base_mod_spec_hgb_only_predict_hgb_outer_fold_1.RDS")
      } else if (outer_fold_name == 2){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble1_base_mod_spec_hgb_only_predict_hgb_outer_fold_2.RDS")
      } else if (outer_fold_name == 3){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble1_base_mod_spec_hgb_only_predict_hgb_outer_fold_3.RDS")
      }
    } else if (train_version == "data_hgb_ferr"){
      data_version <- "hgb_ferr_predict_hgb"
      if (outer_fold_name == 1){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble1_base_mod_spec_hgb_ferr_predict_hgb_outer_fold_1.RDS")
      } else if (outer_fold_name == 2){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble1_base_mod_spec_hgb_ferr_predict_hgb_outer_fold_2.RDS")
      } else if (outer_fold_name == 3){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble1_base_mod_spec_hgb_ferr_predict_hgb_outer_fold_3.RDS")
      }
    }
  } else if (predict_outcome == "predict_ferr"){
    if (train_version == "data_hgb_only"){
      data_version <- "hgb_only_predict_ferr"
      if (outer_fold_name == 1){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble1_base_mod_spec_hgb_only_predict_ferr_outer_fold_1.RDS")
      } else if (outer_fold_name == 2){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble1_base_mod_spec_hgb_only_predict_ferr_outer_fold_2.RDS")
      } else if (outer_fold_name == 3){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble1_base_mod_spec_hgb_only_predict_ferr_outer_fold_3.RDS")
      }
    } else if (train_version == "data_hgb_ferr"){
      data_version <- "hgb_ferr_predict_ferr"
      if (outer_fold_name == 1){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble1_base_mod_spec_hgb_ferr_predict_ferr_outer_fold_1.RDS")
      } else if (outer_fold_name == 2){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble1_base_mod_spec_hgb_ferr_predict_ferr_outer_fold_2.RDS")
      } else if (outer_fold_name == 3){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble1_base_mod_spec_hgb_ferr_predict_ferr_outer_fold_3.RDS")
      }
    }
  }
} else if (ensemble_name == "ensemble2"){
  if (predict_outcome == "predict_hgb"){
    if (train_version == "data_hgb_only"){
      data_version <- "hgb_only_predict_hgb"
      if (outer_fold_name == 1){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble2_base_mod_spec_hgb_only_predict_hgb_outer_fold_1.RDS")
      } else if (outer_fold_name == 2){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble2_base_mod_spec_hgb_only_predict_hgb_outer_fold_2.RDS")
      } else if (outer_fold_name == 3){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble2_base_mod_spec_hgb_only_predict_hgb_outer_fold_3.RDS")
      }
    } else if (train_version == "data_hgb_ferr"){
      data_version <- "hgb_ferr_predict_hgb"
      if (outer_fold_name == 1){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble2_base_mod_spec_hgb_ferr_predict_hgb_outer_fold_1.RDS")
      } else if (outer_fold_name == 2){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble2_base_mod_spec_hgb_ferr_predict_hgb_outer_fold_2.RDS")
      } else if (outer_fold_name == 3){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble2_base_mod_spec_hgb_ferr_predict_hgb_outer_fold_3.RDS")
      }
    }
  } else if (predict_outcome == "predict_ferr"){
    if (train_version == "data_hgb_only"){
      data_version <- "hgb_only_predict_ferr"
      if (outer_fold_name == 1){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble2_base_mod_spec_hgb_only_predict_ferr_outer_fold_1.RDS")
      } else if (outer_fold_name == 2){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble2_base_mod_spec_hgb_only_predict_ferr_outer_fold_2.RDS")
      } else if (outer_fold_name == 3){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble2_base_mod_spec_hgb_only_predict_ferr_outer_fold_3.RDS")
      }
    } else if (train_version == "data_hgb_ferr"){
      data_version <- "hgb_ferr_predict_ferr"
      if (outer_fold_name == 1){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble2_base_mod_spec_hgb_ferr_predict_ferr_outer_fold_1.RDS")
      } else if (outer_fold_name == 2){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble2_base_mod_spec_hgb_ferr_predict_ferr_outer_fold_2.RDS")
      } else if (outer_fold_name == 3){
        dt_split <- readRDS("./3_intermediate/ensemble/updates/nested_model/ensemble2_base_mod_spec_hgb_ferr_predict_ferr_outer_fold_3.RDS")
      }
    }
  }
}


outer_fold_ensemble_assess(base_model_specs = dt_split, 
                                ensemble_config = ensemble_name,
                                version=data_version,
                                outer_fold = outer_fold_name,
                                ensemble_type="average")

# Rscript --vanilla /home/wanjinli/iron-ml-ext-val/2_scripts/04_nested_run_ensemble.R "ensemble1" "predict_hgb" "data_hgb_only" 1
