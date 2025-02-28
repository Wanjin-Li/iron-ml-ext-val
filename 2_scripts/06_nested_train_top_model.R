# This script trains top-performing models (either ensemble or a single base model) from nested cross-validation on the full RISE dataset.


# Train selected top models on RISE data and external validation on SANBS and Vitalant
library(xgboost)
library(randomForest)
library(glmnet)
library(ggplot2)
library(tidyverse)
source('https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R')
theme_set(theme_bw())

source("./2_scripts/utility_functions.R")

intermediate_directory <- './3_intermediate/trained_models/updates/nested_model'  # directory to store trained models
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
outer_fold_name <- as.integer(args[3]) # input from command line

## Load top model specs ----

base_mod_path <- "./3_intermediate/ensemble/updates/nested_model/"


# Predict hgb top models
## OUTER FOLD 1:
### hgb only: Ensemble 2 (2XGB2CB2EN)
### hgb_ferr: XGB.4308

## OUTER FOLD 2:
### hgb only: XGB.4771
### hgb_ferr: Ensemble 1 (6XGB)

## OUTER FOLD 3:
### hgb only: CB.171
### hgb_ferr: XGB.4791


# Predict ferritin top model
## OUTER FOLD 1:
# hgb only: EN.1 
# hgb ferr: CB.801

## OUTER FOLD 2:
# hgb only: CB.507 
# hgb ferr: Ensemble 1 (6CB)

## OUTER FOLD 3:
# hgb only: Ensemble 2 (2XGB2CB2EN) 
# hgb ferr: CB.1168


# Load training data

ensemble1_base_mod_spec <- readRDS(paste0(base_mod_path, "ensemble1_base_mod_spec_", train_version, "_", predict_outcome, "_outer_fold_", outer_fold_name, ".RDS"))
ensemble2_base_mod_spec <- readRDS(paste0(base_mod_path, "ensemble2_base_mod_spec_", train_version, "_", predict_outcome, "_outer_fold_", outer_fold_name, ".RDS"))


model_dev_path <- "./3_intermediate/model_dev_data/nested_model/inner_fold/"


if (predict_outcome == "predict_hgb"){
  rise_factor <- fread(paste0(model_dev_path, "pred_hgb/mdset_factors_", train_version, "_outer_fold_", outer_fold_name, ".csv"))
  rise_oh <- fread(paste0(model_dev_path, "pred_hgb/mdset_OH_", train_version, "_outer_fold_", outer_fold_name, ".csv"))
} else if (predict_outcome == "predict_ferr"){
  rise_factor <- fread(paste0(model_dev_path, "pred_ferr/mdset_factors_", train_version, "_outer_fold_", outer_fold_name, ".csv"))
  rise_oh <- fread(paste0(model_dev_path, "pred_ferr/mdset_OH_", train_version, "_outer_fold_", outer_fold_name, ".csv"))
}

rise_factor <- rise_factor[, -"RandID"]
rise_oh <- rise_oh[, -"RandID"]

# Convert to factor ---- 
# convert character columns to factor for factor dataframes
# make sure blood_type and sex are factors
rise_factor <- mutate_if(rise_factor, is.character, as.factor)

train_single_model <- function(train_data, params, mod_name, path, ensemble_config, 
                               version, outcome, base_mod_name, outer_fold){
  set.seed(444)  # set the seed for reproducibility
  
  # change response variable to "fu_outcome" for easier coding below
  lookup <- c(fu_outcome = "fu_hgb", fu_outcome = "fu_log_ferritin")
  train_data <- train_data %>% rename(any_of(lookup))
  
  ncol_rsplit <- ncol(train_data) # for elastic net
  
  # RANDOM FOREST
  if (mod_name == "RF"){
    fitted_model <- randomForest(fu_outcome~.,  # fu_hgb, fu_log_ferritin
                                 data = train_data,
                                 nodesize = params$nodesize,
                                 mtry=params$mtry,
                                 ntree = params$ntree,
                                 replace = params$replace)
    
    #ELASTIC NET (NO INTERACTIONS)
  } else if (mod_name == "EN"){
    fitted_model <- glmnet(x = as(data.matrix(train_data[ , 1:(ncol_rsplit-1)]), "dgCMatrix"),
                           y = train_data$fu_outcome,  # fu_hgb, fu_ferritin
                           lambda = params$lambda,
                           alpha = params$alpha,
                           family="gaussian")  # gaussian for regression
    
    # XGB GRADIENT BOOSTED MACHINE
  } else if (mod_name == "XGB"){
    
    # Use train and val since XGBoost parameters: early_stopping_rounds and watchlist need it
    train_indices <- sample(nrow(train_data), 0.8 * nrow(train_data))  # Sample 80% of the rows for the train set
    train <- train_data[train_indices, ]  # Create the train set
    val <- train_data[-train_indices, ]  # Create the val set (by selecting the rows not in the train set)
    
    # Extract train and test sets in xgb's special format
    train_set <- xgb.DMatrix(data = as.matrix(train[,-"fu_outcome"]),  # fu_hgb, fu_log_ferritin
                             label = as.matrix(train[,"fu_outcome"]))  # fu_hgb, fu_log_ferritin
    val_set <- xgb.DMatrix(data = as.matrix(val[,-"fu_outcome"]),  # fu_hgb, fu_log_ferritin
                           label = as.matrix(val[,"fu_outcome"]))  # fu_hgb, fu_log_ferritin
    
    # Fit model
    fitted_model <- xgb.train(params = params,
                              data = train_set,
                              nrounds = 10000,
                              early_stopping_rounds = 10,
                              watchlist = list(val1 = train_set, val2 = val_set),
                              verbose = 0,
                              booster = "gbtree",
                              gamma = 3,
                              objective = "reg:squarederror",
                              eval_metric = "rmse")
    
  } else if (mod_name == "CB"){
    # https://catboost.ai/en/docs/concepts/r-reference_catboost-train
    x_train <- train_data[, -"fu_outcome"]
    y_train <- train_data$fu_outcome
    
    # Load pools for training and testing sets
    train_pool <- catboost.load_pool(data = x_train, label = y_train)
    
    # convert char to numeric for certain hyperparams
    if (!is.numeric(params[["depth"]])){
      params[["depth"]] <- as.numeric(params[["depth"]])
    }
    if (!is.numeric(params[["learning_rate"]])){
      params[["learning_rate"]] <- as.numeric(params[["learning_rate"]])
    }
    if (!is.numeric(params[["iterations"]])){
      params[["iterations"]] <- as.numeric(params[["iterations"]])
    }
    if (!is.numeric(params[["l2_leaf_reg"]])){
      params[["l2_leaf_reg"]] <- as.numeric(params[["l2_leaf_reg"]])
    }
    if (!is.numeric(params[["rsm"]])){
      params[["rsm"]] <- as.numeric(params[["rsm"]])
    }
    # Train
    fitted_model <- catboost.train(train_pool, params = params)
    
  }
  # save model
  file_name <- paste0(path, version, "_", outcome, "_", ensemble_config, "_", base_mod_name, "_outer_fold_", outer_fold, ".rds")
  saveRDS(fitted_model, file_name)
}



train_top_model <- function(base_mod_spec, 
                            is_ensemble,
                            path = "./3_intermediate/trained_models/updates/nested_model/",
                            ensemble_config=ensemble_config_name,
                            version=train_version,
                            outcome=predict_outcome, 
                            outer_fold = outer_fold_name){
  
  if (is_ensemble) {  # ensemble 
    for (base_model_idx in 1:length(base_mod_spec)){
      mod_name <- base_mod_spec[[base_model_idx]]$mod_name  # EN, RF, XGB, CB.
      params <- base_mod_spec[[base_model_idx]]$hyperparams
      base_mod_name <- names(base_mod_spec)[base_model_idx]  # XGB.4066
      
      ########################################### Very specific to case and not generalizable
      if (outer_fold==1){
        if (version == "hgb_only" & outcome == "predict_hgb") {
          if (mod_name == "XGB") {
            train_data <- rise_oh
          } else {
            train_data <- rise_factor
          }
        } 
      } else if (outer_fold==2){
        if (version == "hgb_ferr" & outcome == "predict_hgb"){
          if (mod_name == "XGB") {
            train_data <- rise_oh
          } else {
            train_data <- rise_factor
          }
        } else if (version == "hgb_ferr" & outcome == "predict_ferr"){
          if (mod_name == "XGB") {
            train_data <- rise_oh
          } else {
            train_data <- rise_factor
          }
        }
      } else if (outer_fold==3){
        if (version == "hgb_only" & outcome == "predict_ferr"){
          if (mod_name == "XGB") {
            train_data <- rise_oh
          } else {
            train_data <- rise_factor
          }
        }
      }
      #########################################
      
      # train single model
      train_single_model(train_data, params, mod_name, path, ensemble_config, version, outcome, base_mod_name, outer_fold)
    } 
  } else {  # single model
    params <- base_mod_spec$hyperparams
    mod_name <- base_mod_spec$mod_name
    
    ########################################### Very specific to case and not generalizable
    if (outer_fold==1){
      if (version == "hgb_ferr" & outcome == "predict_ferr") {
        base_mod_name <- "CB.801"
        train_data <- rise_factor
      } else if (version == "hgb_ferr" & outcome == "predict_hgb") {
        base_mod_name <- "XGB.4308"
        train_data <- rise_oh
      } else if (version == "hgb_only" & outcome == "predict_ferr"){
        base_mod_name <- "EN.1"
        train_data <- rise_factor
      }
    } else if (outer_fold==2){
      if (version == "hgb_only" & outcome == "predict_hgb"){
        base_mod_name <- "XGB.4771"
        train_data <- rise_oh
      } else if (version == "hgb_only" & outcome == "predict_ferr"){
        base_mod_name <- "CB.507"
        train_data <- rise_factor
      }
    } else if (outer_fold==3){
      if (version == "hgb_only" & outcome == "predict_hgb"){
        base_mod_name <- "CB.171"
        train_data <- rise_factor
      } else if (version == "hgb_ferr" & outcome == "predict_hgb"){
        base_mod_name <- "XGB.4791"
        train_data <- rise_oh
      } else if (version == "hgb_ferr" & outcome == "predict_ferr"){
        base_mod_name <- "CB.1168"
        train_data <- rise_factor
      }
    }
    ########################################### 
    
    # train single model
    train_single_model(train_data, params, mod_name, path, ensemble_config, version, outcome, base_mod_name, outer_fold)
    
  }
}



if (predict_outcome == "predict_hgb"){
  if (train_version == "hgb_only"){
    if (outer_fold_name==1){
      train_top_model(base_mod_spec = ensemble2_base_mod_spec,
                      is_ensemble = TRUE,
                      path = "./3_intermediate/trained_models/updates/nested_model/",
                      ensemble_config = "2CB2XGB2EN",
                      version = train_version, 
                      outcome = predict_outcome)
    } else if (outer_fold_name==2){
      train_top_model(base_mod_spec = ensemble1_base_mod_spec$XGB.4771,
                      is_ensemble = FALSE,
                      path = "./3_intermediate/trained_models/updates/nested_model/",
                      ensemble_config = "1XGB",
                      version = train_version, 
                      outcome = predict_outcome)
      
    } else if (outer_fold_name==3){
      train_top_model(base_mod_spec = ensemble1_base_mod_spec$CB.171,
                      is_ensemble = FALSE,
                      path = "./3_intermediate/trained_models/updates/nested_model/",
                      ensemble_config = "1CB",
                      version = train_version, 
                      outcome = predict_outcome)
    }
  } else if (train_version == "hgb_ferr"){
    if (outer_fold_name==1){
      train_top_model(base_mod_spec = ensemble1_base_mod_spec$XGB.4308,
                      is_ensemble = FALSE,
                      path = "./3_intermediate/trained_models/updates/nested_model/",
                      ensemble_config = "1XGB",
                      version = train_version, 
                      outcome = predict_outcome)
    } else if (outer_fold_name==2){
      train_top_model(base_mod_spec = ensemble1_base_mod_spec,
                      is_ensemble = TRUE,
                      path = "./3_intermediate/trained_models/updates/nested_model/",
                      ensemble_config = "6XGB",
                      version = train_version, 
                      outcome = predict_outcome)
    } else if (outer_fold_name==3){
      train_top_model(base_mod_spec = ensemble1_base_mod_spec$XGB.4791,
                      is_ensemble = FALSE,
                      path = "./3_intermediate/trained_models/updates/nested_model/",
                      ensemble_config = "1XGB",
                      version = train_version, 
                      outcome = predict_outcome)
    }
  }
} else if (predict_outcome == "predict_ferr"){
  if (train_version == "hgb_only"){
    if (outer_fold_name==1){
      train_top_model(base_mod_spec = ensemble1_base_mod_spec$EN.1,
                      is_ensemble = FALSE,
                      path = "./3_intermediate/trained_models/updates/nested_model/",
                      ensemble_config = "1EN",
                      version = train_version, 
                      outcome = predict_outcome)
    } else if (outer_fold_name==2){
      train_top_model(base_mod_spec = ensemble1_base_mod_spec$CB.507,
                      is_ensemble = FALSE,
                      path = "./3_intermediate/trained_models/updates/nested_model/",
                      ensemble_config = "1CB",
                      version = train_version, 
                      outcome = predict_outcome)
    } else if (outer_fold_name==3){
      train_top_model(base_mod_spec = ensemble2_base_mod_spec,
                      is_ensemble = TRUE,
                      path = "./3_intermediate/trained_models/updates/nested_model/",
                      ensemble_config = "2CB2XGB2EN",
                      version = train_version, 
                      outcome = predict_outcome)
    }
  } else if (train_version == "hgb_ferr"){
    if (outer_fold_name==1){
      train_top_model(base_mod_spec = ensemble1_base_mod_spec$CB.801,
                      is_ensemble = FALSE,
                      path = "./3_intermediate/trained_models/updates/nested_model/",
                      ensemble_config = "1CB",
                      version = train_version, 
                      outcome = predict_outcome)
    } else if (outer_fold_name==2){
      train_top_model(base_mod_spec = ensemble1_base_mod_spec,
                      is_ensemble = TRUE,
                      path = "./3_intermediate/trained_models/updates/nested_model/",
                      ensemble_config = "6CB",
                      version = train_version, 
                      outcome = predict_outcome)
    } else if (outer_fold_name==3){
      train_top_model(base_mod_spec = ensemble1_base_mod_spec$CB.1168,
                      is_ensemble = FALSE,
                      path = "./3_intermediate/trained_models/updates/nested_model/",
                      ensemble_config = "1CB",
                      version = train_version, 
                      outcome = predict_outcome)
    }
  }
}


# Rscript --vanilla /home/wanjinli/iron-ml-ext-val/2_scripts/06_nested_train_top_model.R "predict_hgb" "hgb_only" 1




