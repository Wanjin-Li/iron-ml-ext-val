# Train selected top models on RISE data and external validation on SANBS and Vitalant
library(xgboost)
library(randomForest)
library(glmnet)
library(ggplot2)
library(tidyverse)
source('https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R')
theme_set(theme_bw())

source("./2_scripts/utility_functions.R")

intermediate_directory <- './3_intermediate/trained_models'  # directory to store trained models
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}

args = commandArgs(trailingOnly=TRUE)  # for taking in inputs
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}

i <- as.integer(args[1])  # input from bash script

## Load top model specs ----

### Predict hgb ----
base_mod_spec_hgb_ferr_predict_hgb <- readRDS("./3_intermediate/ensemble/base_mod_spec_hgb_ferr_predict_hgb.RDS")
base_mod_spec_hgb_only_predict_hgb <- readRDS("./3_intermediate/ensemble/base_mod_spec_hgb_only_predict_hgb.RDS")

### Predict ferritin ----
base_mod_spec_hgb_ferr_predict_ferr <- readRDS("./3_intermediate/ensemble/base_mod_spec_hgb_ferr_predict_ferr.RDS")
base_mod_spec_hgb_only_predict_ferr <- readRDS("./3_intermediate/ensemble/base_mod_spec_hgb_only_predict_ferr.RDS")

# Predict hgb top models
# hgb_ferr: Ensemble (6 base models)
# hgb only: Ensemble (6 base models)

# Predict ferritin top model
# hgb ferr: EN.1
# hgb only: RF.125

# add specs to a list
list_of_specs <- list(base_mod_spec_hgb_ferr_predict_hgb, 
                      base_mod_spec_hgb_only_predict_hgb,
                      base_mod_spec_hgb_ferr_predict_ferr$EN.1,  # single model
                      base_mod_spec_hgb_only_predict_ferr$RF.125)  # single model

list_of_configs <- c("5XGB1RF", "5XGB1RF", "1EN", "1RF")

list_of_versions <- c("hgb_ferr_predict_hgb", 
                      "hgb_only_predict_hgb",
                      "hgb_ferr_predict_ferr",
                      "hgb_only_predict_ferr")

print(list_of_specs[[i]])
print(list_of_configs[i])
print(list_of_versions[i])


# Load RISE training data ----
# One hot (OH) for XGB
# Factors for RF, EN

## Predict hgb ----
### hgb and ferr ----
rise_hf_pred_h_factor <- fread("./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_factors_hgb_ferr.csv")  # factors
rise_hf_pred_h_oh <- fread("./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_OH_hgb_ferr.csv")  # One hot 
### hgb only ----
rise_h_pred_h_factor <- fread("./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_factors_hgb_only.csv")
rise_h_pred_h_oh <- fread("./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_OH_hgb_only.csv")

## Predict ferr ----
### hgb and ferr ----
rise_hf_pred_f_factor <- fread("./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_factors_hgb_ferr.csv")
rise_hf_pred_f_oh <- fread("./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_OH_hgb_ferr.csv")
### hgb only ----
rise_h_pred_f_factor <- fread("./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_factors_hgb_only.csv")
rise_h_pred_f_oh <- fread("./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_OH_hgb_only.csv")

# Convert to factor ---- 
# convert character columns to factor for factor dataframes
# make sure blood_type and sex are factors
rise_hf_pred_h_factor <- mutate_if(rise_hf_pred_h_factor, is.character, as.factor)
rise_h_pred_h_factor <- mutate_if(rise_h_pred_h_factor, is.character, as.factor)
rise_hf_pred_f_factor <- mutate_if(rise_hf_pred_f_factor, is.character, as.factor)
rise_h_pred_f_factor <- mutate_if(rise_h_pred_f_factor, is.character, as.factor)






# 1. Train top model that is a single model and save model
# 2. Train top model that is an ensemble model type and save base models in list

# 3. external validation: load single model and get predictions
# 4. external validation: load ensemble model (list of base models) and get predictions




# Helper function to train a single base model
train_single_model <- function(train_data, params, mod_name, path, ensemble_config, version, base_mod_name){
  
  # change response variable to "fu_outcome" for easier coding below
  lookup <- c(fu_outcome = "fu_hgb", fu_outcome = "fu_log_ferritin")
  train_data <- train_data %>% rename(any_of(lookup))
  
  # RANDOM FOREST
  if (mod_name == "RF"){
    set.seed(250)
    fitted_model <- randomForest(fu_outcome~.,  # fu_hgb, fu_log_ferritin
                                 data = train_data,
                                 nodesize = params$nodesize,
                                 mtry=params$mtry,
                                 ntree = params$ntree,
                                 replace = params$replace)
    
    #ELASTIC NET (NO INTERACTIONS)
  } else if (mod_name == "EN"){
    set.seed(250)  # set the seed for reproducibility
    ncol_rsplit <- ncol(train_data)  # for elastic net
    fitted_model <- glmnet(x = as(data.matrix(train_data[ , 1:(ncol_rsplit-1)]), "dgCMatrix"),
                           y = train_data$fu_outcome,  # fu_hgb, fu_ferritin
                           lambda = params$lambda,
                           alpha = params$alpha,
                           family="gaussian")  # gaussian for regression
    
    # XGB GRADIENT BOOSTED MACHINE
  } else if (mod_name == "XGB"){
    
    # Use train and val since XGBoost parameters: early_stopping_rounds and watchlist need it
    set.seed(250)  # Set the seed for reproducibility
    train_indices <- sample(nrow(train_data), 0.8 * nrow(train_data))  # Sample 80% of the rows for the train set
    train <- train_data[train_indices, ]  # Create the train set
    val <- train_data[-train_indices, ]  # Create the val set (by selecting the rows not in the train set)

    # Extract train and test sets in xgb's special format
    train_set <- xgb.DMatrix(data = as.matrix(train[,-"fu_outcome"]),  # fu_hgb, fu_log_ferritin
                             label = as.matrix(train[,"fu_outcome"]))  # fu_hgb, fu_log_ferritin
    val_set <- xgb.DMatrix(data = as.matrix(val[,-"fu_outcome"]),  # fu_hgb, fu_log_ferritin
                           label = as.matrix(val[,"fu_outcome"]))  # fu_hgb, fu_log_ferritin
    
    # Fit model
    set.seed(250)  # set the seed for reproducibility
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
    
  }
  # save model
  file_name <- paste0(path, version, "_", ensemble_config, "_", base_mod_name, ".rds")
  saveRDS(fitted_model, file_name)
}



# Note: this function needs to stay here since the "train_data" variable is using variables in this script
# Assess top model that is a single model or ensemble model
train_top_model <- function(base_mod_spec, 
                            is_ensemble,
                            path = "./3_intermediate/trained_models/",
                            ensemble_config="",
                            version="hgb_ferr_predict_ferr"){
  

  if (is_ensemble) {  # ensemble 
    for (base_model_idx in 1:length(base_mod_spec)){
      mod_name <- base_mod_spec[[base_model_idx]]$mod_name  # XGB, RF, EN
      params <- base_mod_spec[[base_model_idx]]$hyperparams
      base_mod_name <- names(base_mod_spec)[base_model_idx]  # XGB.4066
      
      ########################################### Veery specific to case and not generalizable
      if (version == "hgb_ferr_predict_hgb") {
        if (mod_name == "XGB") {
          train_data <- rise_hf_pred_h_oh
        } else if (mod_name == "RF") {
          train_data <- rise_hf_pred_h_factor
        }
      } else if (version == "hgb_only_predict_hgb") {
        if (mod_name == "XGB") {
          train_data <- rise_h_pred_h_oh
        } else if (mod_name == "RF") {
          train_data <- rise_h_pred_h_factor
        }
      }
      #########################################
      
      # train single model
      train_single_model(train_data, params, mod_name, path, ensemble_config, version, base_mod_name)
      
    } 
    
  } else {  # single model
    params <- base_mod_spec$hyperparams
    mod_name <- base_mod_spec$mod_name
    
    ########################################### Veery specific to case and not generalizable
    
    if (version == "hgb_ferr_predict_ferr") {
      base_mod_name <- "EN.1"
      train_data <- rise_hf_pred_f_factor
    } else if (version == "hgb_only_predict_ferr") {
      base_mod_name <- "RF.125"
      train_data <- rise_h_pred_f_factor
    }
    ########################################### 
    
    # train single model
    train_single_model(train_data, params, mod_name, path, ensemble_config, version, base_mod_name)
      
  }
}


# Launching training
if (i == 3 || i == 4) {
  train_top_model(base_mod_spec=list_of_specs[[i]], 
                  is_ensemble = FALSE,
                  path = "./3_intermediate/trained_models/",
                  ensemble_config=list_of_configs[i],
                  version=list_of_versions[i])
  
} else if (i == 1 || i == 2) {
  train_top_model(base_mod_spec=list_of_specs[[i]], 
                  is_ensemble = TRUE,
                  path = "./3_intermediate/trained_models/",
                  ensemble_config=list_of_configs[i],
                  version=list_of_versions[i])
}


