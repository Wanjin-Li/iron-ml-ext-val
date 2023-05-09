##ENSEMBLE MODEL ASSESSMENT
run_ensemble_assess <- function(base_model_specs, path = "./data/", version="withXB",
                                ensemble_type="average"){
  
  base_model_specs <- base_mod_spec_hgb_ferr_predict_hgb
  # base_mdoel_specs <- base_mod_spec_hgb_only_predict_hgb
  
  
  outcome<-list()
  outcome_base_mods <- list()
  
  for (row_outer in 1:nrow(base_model_specs[[1]]$dt_split)){  # 1:15
    
    row_outer <- 1 
    # Table to store predictions across each outer fold
    # Table to store predictions across each inner fold
    dt_inner_fold_preds <- data.table(
      "donor_idx"=numeric(),
      "prediction"= numeric(),
      "fu_outcome"= numeric())
    dt_inner_fold_preds_base <- data.table(
      "base_model" = character(),
      "prediction"= numeric(),
      "fu_outcome"= numeric())
    for (row_inner in 1:nrow(base_model_specs[[1]]$dt_split$inner_resamples[[1]])){  # 1:5
      #Table to store predictions across each inner fold
      dt_single_fold_preds <- data.table(
        "base_model" = character(),
        "Set"=character(),
        "prediction"= numeric(),
        "fu_outcome"= numeric()
      )
      # Train base models
      for (base_model_idx in 1:length(base_model_specs)){  # number of base models
        
        base_model_idx <- 4  # test elastic net so fast
        # Generate predictions on single inner fold
        # Keep raw predictions from both train and test data within the fold
        # Save result from training data
        dt_single_fold_preds <- rbind(
          dt_single_fold_preds,
          cbind("base_model" = names(base_model_specs)[base_model_idx],
                mod_eval(rsplit_obj = base_model_specs[[base_model_idx]]$dt_split$inner_resamples[[row_outer]]$splits[[row_inner]],
                         params = base_model_specs[[base_model_idx]]$hyperparams,
                         mod_name = base_model_specs[[base_model_idx]]$mod_name,
                         return_train_pred=TRUE))
        )
      }
      
      dt_single_fold_preds[, donor_idx := rep(1:(nrow(dt_single_fold_preds)/length(base_model_specs)), length(base_model_specs))]

      # train_acc_in_fold <- dt_single_fold_preds[Set=="Train", list("train_acc"=sum(prediction==fu_outcome)/.N), by=base_model]

      # dt_single_fold_preds<-dt_single_fold_preds[train_acc_in_fold, on="base_model"]
      dt_inner_fold_preds_base<-rbind(dt_inner_fold_preds_base,
                                      dt_single_fold_preds[Set=="Test", .SD, .SDcols=c("base_model", "prediction", "fu_outcome")])

      # model average
      dt_inner_fold_preds<-rbind(dt_inner_fold_preds,
                                 dt_single_fold_preds[Set=="Test", list(
                                   "prediction"=mean(prediction),
                                   "fu_outcome"=mean(fu_outcome)),
                                   by=donor_idx])
    }

    # Calc RMSE, MAE across all folds using postResample from caret
    res <- caret::postResample(dt_inner_fold_preds$fu_outcome,
                               dt_inner_fold_preds$prediction)  # RMSE     MAE 
    rmse <- res[1]
    
    outcome[paste0("rmse_repeat",
                        formatC(ceiling(row_outer/5)),
                        "_fold",
                        formatC((row_outer-1)%%5 + 1))] <- rmse
    print(paste0("outer fold complete: ",row_outer))
    outcome_base_mods[[paste0("rmse_repeat",
                              formatC(ceiling(row_outer/5)),
                              "_fold",
                              formatC((row_outer-1)%%5 + 1))]] <-
      dt_inner_fold_preds_base[, list(prediction, fu_outcome), by=base_model]
  }
  
  ####
  dt.results <- as.data.table(outcome)
  dt.results[ , AUC_mean := rowMeans(.SD), .SDcols =
                paste0("AUC_",formatC(1:15, width=2, flag="0"))]
  
  dt.results_basemods <- as.data.table(outcome_base_mods)
  
  fwrite(dt.results, paste0(path, paste0("ensemble_assess_results_3RF3GBM_",version,"_", ensemble_type,".csv")))
  fwrite(dt.results_basemods, paste0(path, paste0("ensemble_basemods_assess_results_3RF3GBM_",version,"_", ensemble_type,".csv")))
  
}
