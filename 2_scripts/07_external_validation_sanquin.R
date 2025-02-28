library(xgboost)
library(randomForest)
library(glmnet)
library(dplyr)
library(data.table)
library(stringr)
library(catboost)

################### External validation on Sanquin data

intermediate_directory <- './3_intermediate/external_validation_sanquin'  # directory to store external validation results
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}

## load Sanquin data ----
dat_paths <- c("./private/hgb_ferr_sanquin.csv",
               "./private/hgb_only_sanquin.csv")

for (dat_path in dat_paths) {
  dt.md <- fread(dat_path) 
  str(dt.md)
  identifiers <- c("DonorID", "Visit_Date")
  dt.md[, c(identifiers) := NULL]  # remove extraneous fields
  
  # 2 prediction tasks: predict hgb, predict ferritin
  dt.md.hgb <- subset(dt.md, select = -fu_log_ferritin) %>% mutate_if(is.character, as.factor)
  dt.md.ferr <- subset(dt.md, select = -fu_hgb) %>% mutate_if(is.character, as.factor)
  
  # Create 2 versions: one with characters as factors and
  # one with categorical variables 1-hot encoded for model types that require that
  
  # One hot encoding for XGB ----
  # Create dummy one-hot variables for categoricals
  dt.OH.hgb <- data.table(model.matrix(fu_hgb ~ ., data=dt.md.hgb))
  dt.OH.hgb <- cbind(dt.OH.hgb, "fu_hgb" = dt.md$fu_hgb)
  
  dt.OH.ferr <- data.table(model.matrix(fu_log_ferritin ~ ., data=dt.md.ferr))
  dt.OH.ferr <- cbind(dt.OH.ferr, "fu_log_ferritin" = dt.md$fu_log_ferr)
  
  if (dat_path == "./private/hgb_ferr_sanquin.csv") {
    hf_pred_h_factor <- dt.md.hgb
    hf_pred_h_oh <- dt.OH.hgb
    hf_pred_f_factor <- dt.md.ferr
    hf_pred_f_oh <- dt.OH.ferr
  } else if (dat_path == "./private/hgb_only_sanquin.csv") {
    h_pred_h_factor <- dt.md.hgb
    h_pred_h_oh <- dt.OH.hgb
    h_pred_f_factor <- dt.md.ferr
    h_pred_f_oh <- dt.OH.ferr
  }
}


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
    preds = as.data.table(catboost.predict(fitted_model,
                                           test_pool,
                                           prediction_type = 'RawFormulaVal',
                                           thread_count=24,
                                           verbose = FALSE)) # specify the number of threads to use
  }
  colnames(preds) <- paste0("prediction")
  fu_outcome <- unlist(test_data[, "fu_outcome"])  # fu_hgb, fu_log_ferritin
  results <- cbind(preds,fu_outcome)
  return(results)
}



# Uses helper function validate_single_model to get external validation results
load_model_and_predict <- function(version, res_dir) {
  version_pattern <- paste0(version, "*")
  model_paths <- list.files(path="./3_intermediate/trained_models/updates", pattern=version_pattern, full.names = TRUE) 
  
  for (path in model_paths) {  # for each model in ensemble
    base_model_name <- gsub("^.*_", "", path)  # gets the last element after splitting by "_"
    base_model_name <- gsub(".rds", "", base_model_name)  # remove ".rds" -> XGB.4076
    mod_name <- str_split(base_model_name, "[.]")[[1]][1]  # "XGB"
    
    fitted_model <- readRDS(path)  # load model
    
    # Decide which test dataset (One hot or factors to use) 
    if (version == "hgb_ferr_predict_hgb") {
      if (mod_name == "XGB") {
        test_data <- hf_pred_h_oh
      } else {
        test_data <- hf_pred_h_factor
      }
    } else if (version == "hgb_only_predict_hgb") {
      if (mod_name == "XGB") {
        test_data <- h_pred_h_oh
      } else {
        test_data <- h_pred_h_factor
      }
    } else if (version == "hgb_ferr_predict_ferr") {
      if (mod_name == "XGB") {
        test_data <- hf_pred_f_oh
      } else {
        test_data <- hf_pred_f_factor
      }
    } else if (version == "hgb_only_predict_ferr") {
      if (mod_name == "XGB") {
        test_data <- h_pred_f_oh
      } else {
        test_data <- h_pred_f_factor
      }
    }
    
    # Validate base model
    results <- validate_single_model(test_data, fitted_model, mod_name)
    # path <- "./trained_models/hgb_ferr_predict_hgb_5XGB1RF_RF.208.rds"
    file_name <- gsub("^.*/", "", path)[[1]][5]  # get last part of path: "hgb_ferr_predict_hgb_5XGB1RF_RF.208.rds"
    file_name <- paste0(gsub(".rds", "", file_name), ".csv")  # remove ".rds" and add ".csv" -> # "hgb_ferr_predict_hgb_5XGB1RF_RF.208.csv"
    print(file_name)
    fwrite(results, paste0(res_dir, file_name)) # "./3_intermediate/external_validation_sanquin/hgb_ferr_predict_hgb_5XGB1RF_RF.208.csv"
  }
}


# load model and predict on Sanquin ----
# External validation of trained models on Sanquin data

# Ensemble
load_model_and_predict(version = "hgb_ferr_predict_hgb",
                       res_dir='./3_intermediate/external_validation_sanquin/')
load_model_and_predict(version = "hgb_only_predict_hgb",
                       res_dir='./3_intermediate/external_validation_sanquin/')
# single models
load_model_and_predict(version = "hgb_ferr_predict_ferr",
                       res_dir='./3_intermediate/external_validation_sanquin/')
load_model_and_predict(version = "hgb_only_predict_ferr",
                       res_dir='./3_intermediate/external_validation_sanquin/')


#TODO: send external_validation_sanquin folder back to wanjin.li@mail.mcgill.ca
