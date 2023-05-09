# This file also gets uploaded to sherlock for training on computing cluster

library(stringr)
library(xgboost)
library(randomForest)
library(glmnet)
library(rsample)
library(caret)
library(dplyr)
library(data.table)

row_outer = 1
row_inner = 1

# random forest, elastic net
# outer_split <- readRDS("./1_data/rsplits/data_hgb_only/rsplit_factors_pred_hgb.rds")

# xgboost
outer_split <- readRDS("./1_data/rsplits/data_hgb_only/rsplit_OH_pred_ferr.rds")

inner_split <- outer_split$inner_resamples[[row_outer]]

dt_split <- outer_split
# mod_name <- "RF"  # random forest
# param_sets <- fread("./1_data/hyperparam_sets/rf_param_sets.csv")

# mod_name <- "EN"  # elastic net
# param_sets <- fread("./1_data/hyperparam_sets/en_param_sets.csv")

mod_name <- "XGB"
param_sets <- fread("./1_data/hyperparam_sets/xgb_param_sets.csv")

row = 1
params <- as.list(unlist(param_sets[row,]))

mod_eval(inner_split$splits[[row_inner]], params, mod_name)

rsplit_obj <- inner_split$splits[[row_inner]]





row_num = 1

row_outer <- 1

row_inner <- 1

idx_start = 1
fname_results = "assess_results.csv"
  


# MODEL TRAIN AND SELECTION FUNCTIONS ---------------

# MODEL ASSESSMENT using specified single model configuration (non ensemble)
# and set of hyperparameters to assess in nested CV.
# Must also feed the appropriate format of the dataset split for CV
# and provide path for saving the results
# idx_start: When  splitting a large set of hyperparams
#  across multiple jobs run on a computing cluster, set id_start
#  to the number of the first row in hyperparam table under assessment
#  in this job.

run_mod_assess <- function(mod_name, param_sets, dt_split,
                           idx_start = 1,
                           fname_results = "assess_results.csv"){

  #### Info ####
  
 # Check 202 docstring how to write
  #Description of parameters
  
  #Example input:
  
  ##############
 
  # Construct table for storing results
  rmse_res_cols <-  matrix(ncol = 15, nrow = 0)
  rsq_res_cols <-  matrix(ncol = 15, nrow = 0)
  mae_res_cols <-  matrix(ncol = 15, nrow = 0)
  
  colnames(rmse_res_cols) <- paste0("rmse_repeat",
                                    formatC(ceiling(1:15/5)),
                                    "_fold",
                                    formatC((1:15-1)%%5 + 1))
  colnames(rsq_res_cols) <- paste0("rsq_repeat",
                                    formatC(ceiling(1:15/5)),
                                    "_fold",
                                    formatC((1:15-1)%%5 + 1))
  colnames(mae_res_cols) <- paste0("mae_repeat",
                                    formatC(ceiling(1:15/5)),
                                    "_fold",
                                    formatC((1:15-1)%%5 + 1))
  
  dt.results.rmse <- cbind(data.table(model = character(), 
                                 hyperparam_idx = integer()),
                           rmse_res_cols)
  dt.results.rsq <- cbind(data.table(model = character(), 
                                      hyperparam_idx = integer()),
                           rsq_res_cols)
  dt.results.mae <- cbind(data.table(model = character(), 
                                      hyperparam_idx = integer()),
                          mae_res_cols)
  
  # Loop over all hyperparam sets and compute RMSE for each of 15 partitions
  print(Sys.time())
  
  for (row in 1:nrow(param_sets)){
    #print(paste0("Param row ", row))
    params <- as.list(unlist(param_sets[row,]))  # get one set of parameters
    
    dt.results.rmse <- rbind(dt.results.rmse,
                             cv_config(dt_split, params, row+idx_start-1, mod_name)$rmse)
    dt.results.rsq <- rbind(dt.results.rsq,
                             cv_config(dt_split, params, row+idx_start-1, mod_name)$rsq)
    dt.results.mae <- rbind(dt.results.mae,
                             cv_config(dt_split, params, row+idx_start-1, mod_name)$mae)
    
    #Overwrite every time in case job times out
    fwrite(dt.results.rmse, paste0(fname_results, "rmse.csv"))
    fwrite(dt.results.rsq, paste0(fname_results, "rsq.csv"))
    fwrite(dt.results.mae, paste0(fname_results, "mae.csv"))
    print(paste0("Completed hyperparam set ", row,". ", Sys.time()))
  }
  
  dt.results.rmse[ , res_mean := rowMeans(.SD), .SDcols = paste0("rmse_repeat",
                                                            formatC(ceiling(1:15/5)),
                                                            "_fold",
                                                            formatC((1:15-1)%%5 + 1))]
  dt.results.rsq[ , res_mean := rowMeans(.SD), .SDcols = paste0("rsq_repeat",
                                                            formatC(ceiling(1:15/5)),
                                                            "_fold",
                                                            formatC((1:15-1)%%5 + 1))]
  dt.results.mae[ , res_mean := rowMeans(.SD), .SDcols = paste0("mae_repeat",
                                                            formatC(ceiling(1:15/5)),
                                                            "_fold",
                                                            formatC((1:15-1)%%5 + 1))]
  
  # Overwrite at end with mean RMSE added
  fwrite(dt.results.rmse, paste0(fname_results, "rmse.csv"))
  fwrite(dt.results.rsq, paste0(fname_results, "rsq.csv"))
  fwrite(dt.results.mae, paste0(fname_results, "mae.csv"))
}


# for XGB to tune subset of parameters
tune_subset <- function(mod_name, param_sets, dt_split,
                        fname_results = "idi/XGB_assess_results_",
                        start, end) {
  # ex: start = 1, end = 50
  # fname = "idi/XGB_assess_results_0001to0050.csv"
  
  fname <- paste0(fname_results, formatC(start, width=4, flag=0), "to",
                  formatC(end, width=4, flag=0), "_")
  
  run_mod_assess(mod_name,
                 param_sets[start:end, ], dt_split,
                 idx_start = start,
                 fname_results = fname)
}


# CV CONFIGURATION (called by model assessment function)
# For param set, perform CV across all inner folds
# and return RMSE for each of 3X5=15 inner CV sets

# outer_split <- readRDS("./1_data/rsplits/data_hgb_only/rsplit_factors_pred_hgb.rds")

cv_config <- function(outer_split, params, row_num, mod_name){
  
  rmse_outcome <- list(model=mod_name, hyperparam_idx = row_num)
  rsq_outcome <- list(model=mod_name, hyperparam_idx = row_num)
  mae_outcome <- list(model=mod_name, hyperparam_idx = row_num)
  
  for (row_outer in 1:nrow(outer_split)){  # 15 rows
    # print(paste0(" outer row ", row_outer))
    inner_split <- outer_split$inner_resamples[[row_outer]]
    #Table to store predictions across each inner fold
    dt_inner_fold_preds <- data.table("prediction"= numeric(),
                                      "fu_outcome"= numeric())
    
    for (row_inner in 1:nrow(inner_split)){
      #print(paste0("  inner row ", row_inner))
      tryCatch({
        dt_inner_fold_preds <- rbind(
          dt_inner_fold_preds,
          mod_eval(inner_split$splits[[row_inner]], params, mod_name)
        )
      }, error = function(error_condition){
        cat("Could not fit on this inner fold")
      })
    }
    
    # Calc RMSE, Rsquared, MAE across all folds using postResample from caret
    res <- caret::postResample(dt_inner_fold_preds$fu_outcome,
                               dt_inner_fold_preds$prediction)  # RMSE  Rsquared       MAE 
    rmse <- res[1]
    rsq <- res[2]
    mae <- res[3]
    
    rmse_outcome[paste0("rmse_repeat",
                   formatC(ceiling(row_outer/5)),
                   "_fold",
                   formatC((row_outer-1)%%5 + 1))] <- rmse

    rsq_outcome[paste0("rsq_repeat",
                        formatC(ceiling(row_outer/5)),
                        "_fold",
                        formatC((row_outer-1)%%5 + 1))] <- rsq

    mae_outcome[paste0("mae_repeat",
                        formatC(ceiling(row_outer/5)),
                        "_fold",
                        formatC((row_outer-1)%%5 + 1))] <- mae
    
  }
  all_metrics <- list(rmse=rmse_outcome, rsq=rsq_outcome, mae=mae_outcome)
  return(all_metrics)
}




# MODEL EVALUATION (called by cv_config)
#  for a given train-test split in the rsplit_object, set of hyperparameters,
#  assess performance. When used for ensembles, set return_train_pred to
#  true so those can be accessed to tune the ensemble

# rsplit_rds <- readRDS("./1_data/rsplits/data_hgb_only/rsplit_factors_pred_hgb.rds")
# f <- analysis(rsplit_rds$splits[[1]])
# ncol_rsplit <- ncol(analysis(rsplit_rds$splits[[1]]))
mod_eval <- function(rsplit_obj, params, mod_name, return_train_pred=FALSE){
  # The "analysis" data are those that we selected in the resample. 
  # For 5-fold cross-validation, this is the 80% of the data.
  
  # The assessment data are usually the section of the original data not covered by the analysis set. 
  # Again, in 5-fold CV, this is the 20% held out. 
  
  train_data <- analysis(rsplit_obj)  
  test_data <-  assessment(rsplit_obj)
  
  # change response variable to "fu_outcome" for easier coding below
  lookup <- c(fu_outcome = "fu_hgb", fu_outcome = "fu_ferritin")
  train_data <- train_data %>% rename(any_of(lookup))
  test_data <- test_data %>% rename(any_of(lookup))
  
  ncol_rsplit <- ncol(train_data)  
  
  # RANDOM FOREST
  if (mod_name == "RF"){
    rf_fit <- randomForest(fu_outcome~.,  # fu_hgb, fu_ferritin
                           data = train_data,
                           nodesize = params$nodesize,
                           ntree = params$ntree,
                           replace = params$replace)
    
    # generate predictions
    preds = as.data.table(predict(rf_fit, test_data))
    
    #TODO: if statement check
    if(return_train_pred==TRUE){  # for ensemble training only
      pred_train <- as.data.table(predict(rf_fit, train_data))
      preds<-rbind(cbind(Set="Train", pred_train), cbind(Set="Test", preds))
      colnames(preds) <- c("Set", "prediction")
      fu_outcome = rbind(train_data[,"fu_outcome"],  # fu_hgb, fu_ferritin
                         test_data[,"fu_outcome"])   # fu_hgb, fu_ferritin
      
    } else {
      colnames(preds) <- paste0("prediction")
      fu_outcome = test_data[,"fu_outcome"]   # fu_hgb, fu_ferritin
    }
    
    
    
    #ELASTIC NET (NO INTERACTIONS)
  } else if (mod_name == "EN"){
    
    fit <- glmnet(x = as(data.matrix(train_data[ , 1:(ncol_rsplit-1)]), "dgCMatrix"),
                  y = train_data$fu_outcome,  # fu_hgb, fu_ferritin
                  lambda = params$lambda,
                  alpha = params$alpha,
                  family="gaussian")  # gaussian for regression
    
    # generate predictions
    X_test <- data.matrix(test_data[ , 1:(ncol_rsplit-1)])  # remove last column (outcome)
    preds = as.data.table(predict(fit, newx = X_test))
    
    if(return_train_pred==TRUE){  # for ensemble training
      X_train <- data.matrix(train_data[ , 1:(ncol_rsplit-1)])  # remove last col (outcome)
      pred_train <- as.data.table(predict(fit, newx = X_train ))
      preds<-rbind(cbind(Set="Train", pred_train), cbind(Set="Test", preds))
      colnames(preds) <- c("Set", "prediction")
      fu_outcome = unlist(rbind(train_data[,"fu_outcome"],  # fu_hgb, fu_ferritin
                                test_data[,"fu_outcome"]))  # fu_hgb, fu_ferritin
      
    } else{
      colnames(preds) <- paste0("prediction")
      fu_outcome = unlist(test_data[, "fu_outcome"])  # fu_hgb, fu_ferritin
    }
    
    
    
    # XGB GRADIENT BOOSTED MACHINE
  } else if (mod_name == "XGB"){
    
    # Extract train and test sets in xgb's special format
    xgb.train = xgb.DMatrix(data = as.matrix(train_data[,-"fu_outcome"]),  # fu_hgb, fu_ferritin
                            label = as.matrix(train_data[,"fu_outcome"]))  # fu_hgb, fu_ferritin
    xgb.test = xgb.DMatrix(data = as.matrix(test_data[,-"fu_outcome"]),  # fu_hgb, fu_ferritin
                           label = as.matrix(test_data[,"fu_outcome"]))  # fu_hgb, fu_ferritin
    
    # Fit model
    xgb.fit = xgb.train(params = params,
                        data = xgb.train,
                        nrounds = 10000,
                        early_stopping_rounds = 10,
                        watchlist = list(val1 = xgb.train, val2 = xgb.test),
                        verbose = 0,
                        booster = "gbtree",
                        gamma = 3,
                        objective = "reg:squarederror",
                        eval_metric = "rmse")
    
    preds = as.data.table(predict(xgb.fit, xgb.test, reshape = T))
    
    if(return_train_pred==TRUE){  # for ensemble training
      pred_train <- as.data.table(predict(xgb.fit, xgb.train, reshape = T))
      preds<-rbind(cbind(Set="Train", pred_train), cbind(Set="Test", preds))
      colnames(preds) <- c("Set", "prediction")
      fu_outcome = unlist(rbind(train_data[,"fu_outcome"],  # fu_hgb, fu_ferritin
                                test_data[,"fu_outcome"]))  # fu_hgb, fu_ferritin
      
    } else{
      colnames(preds) <- paste0("prediction")
      fu_outcome = unlist(test_data[, "fu_outcome"])  # fu_hgb, fu_ferritin
    }
  }
  
  results <- cbind(preds,fu_outcome)
  
  return(results)
}
