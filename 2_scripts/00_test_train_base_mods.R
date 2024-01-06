# test on XGB ------
# RF test done
# EN test done
# XGB test done

source('./2_scripts/utility_functions.R')  # using run_mod_assess, tune_subset, cv_config, mod_eval helper functions`


mod_name <- "CB"
param_sets <- fread("./3_intermediate/hyperparameters/cb_hyperparameters.csv")

predict_biomarkers <- "predict_hgb"
train_biomarkers <- "data_hgb_only"
dt_split <- readRDS("./3_intermediate/rsplits/main_model/pred_hgb/rsplit_factors_hgb_only.rds")

outer_fold = 0
start = 1
end = 5
fname_results <- paste0("./3_intermediate/tune_results/base_mod_tune_", 
                        predict_biomarkers,
                        "_",
                        train_biomarkers,
                        "_",
                        mod_name,
                        "_",
                        "params",start,"_to_",end,
                        ifelse(outer_fold==0, ".csv", 
                               paste0("_outer_fold_",outer_fold,".csv")))


tune_subset(mod_name=mod_name, 
            param_sets=param_sets, 
            dt_split=dt_split,
            fname_results=fname_results,
            start=start, 
            end=end
)

# test utility functions

rmspe_res_cols <-  matrix(ncol = 15, nrow = 0)

#names of rmspe columns for data.table
colnames(rmspe_res_cols) <- paste0("rmspe_rpt",
                                   rep(1:3, each = 5),
                                   "_fold",
                                   rep(1:5, times = 3))

# Create data.table to store rmspe results
dt.results.rmspe <- cbind(data.table(model = character(), 
                                     hyperparam_idx = integer()),
                          rmspe_res_cols)

# Loop over all hyperparam sets and compute RMSE for each of 15 partitions
print(paste0(mod_name, ": ", Sys.time()))


row_num = 1

params <- as.list(unlist(param_sets[row_num,])) 

# # get data for this model configuration
rmspe_outcome <- list(model=mod_name, hyperparam_idx = row_num)
rsplit_obj <- dt_split$splits
dt_preds_for_fold <- data.table("prediction"= numeric(),
                                "fu_outcome"= numeric())

i = 1
print(paste0("inner row: ", i))
train_data <- analysis(rsplit_obj[[i]])
validate_data <- assessment(rsplit_obj[[i]])
idxs_info <- rsplit_obj[[i]]
rpt <- as.numeric(gsub("[^0-9]", "", idxs_info$id[1]))
fold <- as.numeric(gsub("[^0-9]", "", idxs_info$id[2]))


mod_eval(train_data,
         validate_data, 
         params, 
         mod_name)


dt.results.rmspe <- rbind(dt.results.rmspe,
                          cv_config(dt_split, params, row_num, mod_name))




# # get data for this model configuration
rmspe_outcome <- list(model=mod_name, hyperparam_idx = row_num)
rsplit_obj <- dt_split$splits
dt_preds_for_fold <- data.table("prediction"= numeric(),
                                "fu_outcome"= numeric())
i = 1
print(paste0("inner row: ", i))
train_data <- analysis(rsplit_obj[[i]])
validate_data <- assessment(rsplit_obj[[i]])
idxs_info <- rsplit_obj[[i]]
rpt <- as.numeric(gsub("[^0-9]", "", idxs_info$id[1]))
fold <- as.numeric(gsub("[^0-9]", "", idxs_info$id[2]))

fit_params <- list(loss_function = 'MultiClass',
                   l2_leaf_reg = 1,
                   depth = 8,
                   learning_rate = 0.01)
mod_eval(train_data,
         validate_data, 
         fit_params, 
         mod_name)

dt_preds_for_fold <- rbind(
  dt_preds_for_fold,
  mod_eval(train_data,
           validate_data, 
           params, 
           mod_name)
)

#calculate RMSPE
EPSILON <-  1e-10  # prevent division by zero
rmspe <- (sqrt(mean(((dt_preds_for_fold$fu_outcome - dt_preds_for_fold$prediction) / 
                       (dt_preds_for_fold$fu_outcome + EPSILON))^2))) * 100

rmspe_outcome[paste0("rmspe_rpt",
                     rpt,
                     "_fold",
                     fold)] <- rmspe




