
# To test CY's codes

library(stringr)
library(xgboost)
library(randomForest)
library(glmnet)
library(rsample)
library(caret)
library(dplyr)
library(data.table)

source('./2_scripts/utility_functions.R')  # using run_mod_assess, tune_subset, cv_config, mod_eval helper functions

# test random forest
## Load hyperparameters depending on model type ----
param_sets <- fread("./3_intermediate/hyperparameters/rf_hyperparameters.csv")

# Load rsplit training dataset depending on biomarkers in data ----

## predict hgb only 
dt_split <- readRDS("./3_intermediate/rsplits/main_model/pred_hgb/rsplit_factors_hgb_only.rds")

fname_results <- paste0("./3_intermediate/test_results/", 
                        "predict_hgb",
                        "/",
                        "data_hgb_only",
                        "/",
                        "RF",
                        "_")


# call tune_subset() ----

# tune_subset(mod_name="RF",
#             param_sets=param_sets[1:4],
#             dt_split=dt_split,
#             fname_results=fname_results,
#             start=1,
#             end=4)

mod_name <- "RF"

fname <- paste0(fname_results, formatC(1, width=4, flag=0), "to",
                formatC(4, width=4, flag=0), "_")

# call run_mod_assess() ----

# run_mod_assess(mod_name,
#                param_sets[1:4, ], dt_split,
#                idx_start = 1,
#                fname_results = fname)
# 

rmspe_res_cols <-  matrix(ncol = 15, nrow = 0)
#mae_res_cols <-  matrix(ncol = 15, nrow = 0)

# colnames(rmse_res_cols) <- paste0("rmse_repeat",
#                                   formatC(ceiling(1:15/5)),
#                                   "_fold",
#                                   formatC((1:15-1)%%5 + 1))
colnames(rmspe_res_cols) <- paste0("rmspe_repeat",
                                   formatC(ceiling(1:15/5)),
                                   "_fold",
                                   formatC((1:15-1)%%5 + 1))
# colnames(mae_res_cols) <- paste0("mae_repeat",
#                                  formatC(ceiling(1:15/5)),
#                                  "_fold",
#                                  formatC((1:15-1)%%5 + 1))

# dt.results.rmse <- cbind(data.table(model = character(), 
#                                     hyperparam_idx = integer()),
#                          rmse_res_cols)
dt.results.rmspe <- cbind(data.table(model = character(), 
                                     hyperparam_idx = integer()),
                          rmspe_res_cols)
# dt.results.mae <- cbind(data.table(model = character(), 
#                                    hyperparam_idx = integer()),
#                         mae_res_cols)

# Loop over all hyperparam sets and compute RMSE for each of 15 partitions
print(paste0(mod_name, ": ", Sys.time()))

for (row in 1:nrow(param_sets)){
  #print(paste0("Param row ", row))
  params <- as.list(unlist(param_sets[row,]))  # get one set of parameters
  
  dt.results.rmspe <- rbind(dt.results.rmspe,
                            cv_config(dt_split, params, row+idx_start-1, mod_name)$rmspe)
  print(paste0("Completed hyperparam set ", row+idx_start-1,". ", Sys.time()))
}

# call cv_config() ----
params <- as.list(unlist(param_sets[1,]))  # get one set of parameters

# cv_config(dt_split, params, row+idx_start-1, mod_name)$rmspe
idx_start <- 1
row <- 1
row_num <- row+idx_start-1

rmspe_outcome <- list(model=mod_name, hyperparam_idx = row_num)
#mae_outcome <- list(model=mod_name, hyperparam_idx = row_num)

for (row_outer in 1:nrow(outer_split)){  # 10 rows
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
  
  EPSILON <-  1e-10  # prevent division by zero
  # rmspe <- (sqrt(mean(((y_true - y_pred) / (y_true + EPSILON))**2))) * 100
  rmspe <- (sqrt(mean(((dt_inner_fold_preds$fu_outcome - dt_inner_fold_preds$prediction) / (dt_inner_fold_preds$fu_outcome + EPSILON))**2))) * 100
  
  # Calc RMSE, MAE across all folds using postResample from caret
  # res <- caret::postResample(dt_inner_fold_preds$fu_outcome,
  #                            dt_inner_fold_preds$prediction)  # RMSE     MAE 
  #rmse <- res[1]
  #mae <- res[3]
  
  # rmse_outcome[paste0("rmse_repeat",
  #                     formatC(ceiling(row_outer/5)),
  #                     "_fold",
  #                     formatC((row_outer-1)%%5 + 1))] <- rmse
  
  rmspe_outcome[paste0("rmspe_repeat",
                       formatC(ceiling(row_outer/5)),
                       "_fold",
                       formatC((row_outer-1)%%5 + 1))] <- rmspe
  
  # mae_outcome[paste0("mae_repeat",
  #                    formatC(ceiling(row_outer/5)),
  #                    "_fold",
  #                    formatC((row_outer-1)%%5 + 1))] <- mae
  
  # add root mean squared percentage
  
}
all_metrics <- list(rmspe=rmspe_outcome)
# all_metrics <- list(rmse=rmse_outcome, rmspe=rmspe_outcome, mae=mae_outcome)
return(all_metrics)




# dt.results.rmse[ , res_mean := rowMeans(.SD), .SDcols = paste0("rmse_repeat",
#                                                                formatC(ceiling(1:15/5)),
#                                                                "_fold",
#                                                                formatC((1:15-1)%%5 + 1))]
dt.results.rmspe[ , res_mean := rowMeans(.SD), .SDcols = paste0("rmspe_repeat",
                                                                formatC(ceiling(1:15/5)),
                                                                "_fold",
                                                                formatC((1:15-1)%%5 + 1))]
# dt.results.mae[ , res_mean := rowMeans(.SD), .SDcols = paste0("mae_repeat",
#                                                               formatC(ceiling(1:15/5)),
#                                                               "_fold",
#                                                               formatC((1:15-1)%%5 + 1))]

# Overwrite at end with mean RMSE added
# fwrite(dt.results.rmse, paste0(fname_results, "rmse.csv"))


# fwrite(dt.results.rmspe, paste0(fname_results, "rmspe.csv")) # uncomment this after finishing testing
return(dt.results.rmspe) # comment out this after finishing testing




# call mod_eval()
train_data <- analysis(rsplit_obj)  
test_data <-  assessment(rsplit_obj)

