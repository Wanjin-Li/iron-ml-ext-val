library(stringr)
library(xgboost)
library(randomForest)
library(glmnet)
library(rsample)
library(caret)
library(dplyr)
library(data.table)

# ENSEMBLE MODELS ------

#Construct model specs as lists
#RF uses dt_split_factor
#GBM uses dt_split_xgb
#EN (no interaction) uses dt_split_xgb
#EN interaction uses dt_split_interactions
#GBM

"./1_data/rsplits/data_hgb_ferr/rsplit_factors_pred_hgb.rds"
"./1_data/rsplits/data_hgb_ferr/rsplit_factors_pred_ferr.rds"
"./1_data/rsplits/data_hgb_ferr/rsplit_OH_pred_hgb.rds"
"./1_data/rsplits/data_hgb_ferr/rsplit_OH_pred_ferr.rds"

"./1_data/rsplits/data_hgb_only/rsplit_factors_pred_hgb.rds"
"./1_data/rsplits/data_hgb_only/rsplit_factors_pred_ferr.rds"
"./1_data/rsplits/data_hgb_only/rsplit_OH_pred_hgb.rds"
"./1_data/rsplits/data_hgb_only/rsplit_OH_pred_ferr.rds"

# version = {pred_hgb, pred_ferr}
mod_spec_as_list <- function(mod_id, version="withXB"){
  mod_id_split <- unlist(str_split(mod_id, "\\."))
  mod_name <-mod_id_split[1]
  hyperparam_idx <- as.numeric(mod_id_split[2])
  
  
  if (mod_name == "XGB"){
    dt_split <- get(paste0("rsplit_OH_", version))
    hyperparams = as.list(xgb_param_sets[hyperparam_idx, ])
    
  } else if (mod_name=="RF"){
    
    dt_split = get(paste0("rsplit_factor_",version))
    hyperparams = as.list(rf_param_sets[hyperparam_idx, ])
    
  } else if (mod_name=="elastic_net"){
    dt_split = get(paste0("rsplit_OH_",version))
    hyperparams = as.list(en_param_sets[hyperparam_idx, ])
    
  } 
  return(list(mod_name=mod_name,
              dt_split=dt_split,
              hyperparams = hyperparams))
}

##ENSEMBLE MODEL ASSESSMENT
run_ensemble_assess <- function(base_model_specs, path = "./data/", version="withXB",
                                ensemble_type="CAWPE"){
  outcome<-list()
  outcome_base_mods <- list()
  
  for (row_outer in 1:nrow(base_model_specs[[1]]$dt_split)){
    # #Table to store predictions across each outer fold
    #Table to store predictions across each inner fold
    dt_inner_fold_preds <- data.table(
      "donor_idx"=numeric(),
      "Z0"= numeric(),
      "Z1"= numeric(),
      "Z2"= numeric(),
      "Z3"= numeric(),
      "fu_outcome"= numeric()
    )
    dt_inner_fold_preds_base <- data.table(
      "base_model" = character(),
      "Z0"= numeric(),
      "Z1"= numeric(),
      "Z2"= numeric(),
      "Z3"= numeric(),
      "fu_outcome"= numeric()
    )
    for (row_inner in 1:nrow(base_model_specs[[1]]$dt_split$inner_resamples[[1]])){
      #Table to store predictions across each inner fold
      dt_single_fold_preds <- data.table(
        "base_model" = character(),
        "Set"=character(),
        "Z0"= numeric(),
        "Z1"= numeric(),
        "Z2"= numeric(),
        "Z3"= numeric(),
        "fu_outcome"= numeric()
      )
      #Train base models
      for (base_model_idx in 1:length(base_model_specs)){
        #Generate predictions on single inner fold
        # Keep raw predictions from both train and test data within the fold
        #Save accuracy from training data
        dt_single_fold_preds <- rbind(
          dt_single_fold_preds,
          cbind("base_model" = names(base_model_specs)[base_model_idx],
                mod_eval(rsplit_obj = base_model_specs[[base_model_idx]]$dt_split$inner_resamples[[row_outer]]$splits[[row_inner]],
                         params = base_model_specs[[base_model_idx]]$hyperparams,
                         mod_name = base_model_specs[[base_model_idx]]$mod_name,
                         return_train_pred=TRUE))
        )
      }
      #
      #
      
      dt_single_fold_preds[, donor_idx := rep(1:(nrow(dt_single_fold_preds)/length(base_model_specs)), length(base_model_specs))]
      dt_single_fold_preds[ , prediction := paste0("Z",max.col(dt_single_fold_preds[,3:6])-1)]
      train_acc_in_fold <- dt_single_fold_preds[Set=="Train", list("train_acc"=sum(prediction==fu_outcome)/.N), by=base_model]
      dt_single_fold_preds<-dt_single_fold_preds[train_acc_in_fold, on="base_model"]
      dt_inner_fold_preds_base<-rbind(dt_inner_fold_preds_base,
                                      dt_single_fold_preds[Set=="Test", .SD, .SDcols=c("base_model", "Z0", "Z1", "Z2", "Z3", "fu_outcome")])

      # Ensemble: model average
      dt_inner_fold_preds<-rbind(dt_inner_fold_preds,
                                 dt_single_fold_preds[Set=="Test", list(
                                   "Z0"=mean(Z0),
                                   "Z1"=mean(Z1),
                                   "Z2"=mean(Z2),
                                   "Z3"=mean(Z3),
                                   "fu_outcome"=min(fu_outcome)),
                                   by=donor_idx])
    }
    
    #Calc 1vsAll AUC across all folds for ensemble
    outcome[paste0("AUC_",formatC(row_outer, width=2, flag="0"))] <-
      multiclass.roc(fu_outcome~Z0+Z1+Z2+Z3, data = dt_inner_fold_preds)$auc
    print(paste0("outer fold complete: ",row_outer))
    outcome_base_mods[[paste0("AUC_",formatC(row_outer, width=2, flag="0"))]] <-
      dt_inner_fold_preds_base[, list("AUC"=AUC(Z0, Z1, Z2, Z3, fu_outcome)), by=base_model]
  }
  
  ####
  dt.results <- as.data.table(outcome)
  dt.results[ , AUC_mean := rowMeans(.SD), .SDcols =
                paste0("AUC_",formatC(1:15, width=2, flag="0"))]
  
  dt.results_basemods <- as.data.table(outcome_base_mods)
  
  fwrite(dt.results, paste0(path, paste0("ensemble_assess_results_3RF3GBM_",version,"_", ensemble_type,".csv")))
  fwrite(dt.results_basemods, paste0(path, paste0("ensemble_basemods_assess_results_3RF3GBM_",version,"_", ensemble_type,".csv")))
  
}



outer_fold_assess <- function(base_mod_spec,
                              path = "./data/", version="withXB"){
  
  top_model_aucs <- list()
  
  dt_preds_all_repeats <- data.table(
    "rpt" = numeric(),
    "fold" = numeric(),
    "fu_outcome"= numeric()
  )
  
  Sys.time()
  for (idx_rpt in 1:3){
    #Table to store predictions across each fold of each resample
    dt_rpt_preds <- data.table(
      "fold" = numeric(),
      "fu_outcome"= numeric()
    )
    for (idx_fld in 1:5){
      rsplit_obj<-filter(base_mod_spec$dt_split, id==paste0("Repeat",idx_rpt) & id2==paste0("Fold",idx_fld))$splits[[1]]
      dt_rpt_preds <- rbind(
        dt_rpt_preds,
        cbind(
          "fold" = idx_fld,
          mod_eval(rsplit_obj,
                   base_mod_spec$hyperparams,
                   base_mod_spec$mod_name,
                   return_train_pred=FALSE)
        ))
      
      print(paste0("Fold ", idx_fld, " Repeat ", idx_rpt, " complete ",Sys.time()))
    }
    dt_preds_all_repeats <- rbind(dt_preds_all_repeats,
                                  cbind(
                                    "rpt" = idx_rpt,
                                    dt_rpt_preds))
    #Calc 1vsAll AUC across all folds
    top_model_aucs[paste0("AUC_",formatC(idx_rpt, width=2, flag="0"))] <- multiclass.roc(fu_outcome~Z0+Z1+Z2+Z3, data = dt_rpt_preds)$auc
  }
  fwrite(dt_preds_all_repeats, file = paste0(path,
                                             "top_model_assess_",
                                             version,
                                             ".csv"))
  #dt_preds_all_repeats[ , multiclass.roc(fu_outcome~Z0+Z1+Z2+Z3)$auc, by= c("rpt")]
}



outer_fold_assess_ensemble <- function(base_model_specs,
                                       path = "./data/", version="withXB"){
  #SIMPLE MODEL AVG- NOW CAWPE
  top_model_aucs <- list()
  
  dt_preds_all_repeats <- data.table(
    "rpt" = numeric(),
    "fold" = numeric(),
    "donor_idx"=numeric(),
    "Z0"= numeric(),
    "Z1"= numeric(),
    "Z2"= numeric(),
    "Z3"= numeric(),
    "fu_outcome"= numeric()
  )
  
  Sys.time()
  for (idx_rpt in 1:3){
    #Table to store predictions across each fold of each resample
    dt_rpt_preds <- data.table(
      "fold" = numeric(),
      "donor_idx"=numeric(),
      "Z0"= numeric(),
      "Z1"= numeric(),
      "Z2"= numeric(),
      "Z3"= numeric(),
      "fu_outcome"= numeric()
    )
    for (idx_fld in 1:5){
      #Table to store predictions across each inner fold
      dt_single_fold_preds <- data.table(
        "base_model" = character(),
        "Z0"= numeric(),
        "Z1"= numeric(),
        "Z2"= numeric(),
        "Z3"= numeric(),
        "fu_outcome"= numeric()
      )
      #Train base models
      for (base_model_idx in 1:length(base_model_specs)){
        #Generate predictions on single inner fold
        # Keep raw predictions from both train and test data within the fold
        #Save accuracy from training data
        rsplit_obj<-filter(base_model_specs[[base_model_idx]]$dt_split,
                           id==paste0("Repeat",idx_rpt) &
                             id2==paste0("Fold",idx_fld))$splits[[1]]
        
        
        dt_single_fold_preds <- rbind(
          dt_single_fold_preds,
          cbind("base_model" = names(base_model_specs)[base_model_idx],
                mod_eval(rsplit_obj = rsplit_obj,
                         params = base_model_specs[[base_model_idx]]$hyperparams,
                         mod_name = base_model_specs[[base_model_idx]]$mod_name,
                         return_train_pred=FALSE))
        )
      }
      
      dt_single_fold_preds[, donor_idx := rep(1:(nrow(dt_single_fold_preds)/length(base_model_specs)), length(base_model_specs))]
      
      #model average
      dt_rpt_preds<-rbind(dt_rpt_preds,
                          cbind("fold"=idx_fld,
                                dt_single_fold_preds[, list(
                                  "Z0"=mean(Z0),
                                  "Z1"=mean(Z1),
                                  "Z2"=mean(Z2),
                                  "Z3"=mean(Z3),
                                  "fu_outcome"=min(fu_outcome)),
                                  by=donor_idx]))
      
      print(paste0("Fold ", idx_fld, " Repeat ", idx_rpt, " complete ",Sys.time()))
    }
    dt_preds_all_repeats <- rbind(dt_preds_all_repeats,
                                  cbind(
                                    "rpt" = idx_rpt,
                                    dt_rpt_preds))
    #Calc 1vsAll AUC across all folds
    #top_model_aucs[paste0("AUC_",formatC(idx_rpt, width=2, flag="0"))] <- multiclass.roc(fu_outcome~Z0+Z1+Z2+Z3, data = dt_rpt_preds)$auc
  }
  fwrite(dt_preds_all_repeats, file = paste0(pafth,
                                             "top_model_assess_",
                                             version,
                                             ".csv"))
}


train_mod <- function(mod_name, params, features){
  
  if (mod_name=="XGB"){
    #Extract train and test sets in xgb's special format
    train_matrix = xgb.DMatrix(data = as.matrix(features[,-"fu_outcome"]),
                               label = as.matrix(features[,"fu_outcome"]))
    model <- xgb.train(params=params,
                       data=train_matrix,
                       nrounds=10000,
                       early_stopping_rounds=10,
                       watchlist=list(val1=train_matrix),
                       verbose=0,
                       booster="gbtree",
                       gamma=3,
                       objective="reg:squarederror",
                       eval_metric="rmse")

  } else if (mod_name=="RF"){
    model <- randomForest(fu_outcome~.,
                          data = features,
                          nodesize = params$nodesize,
                          ntree = params$ntree,
                          replace = params$replace)
    
  } else if (mod_name=="EN"){
    model <- glmnet(x = as(data.matrix(features[,-"fu_outcome"]), "dgCMatrix"),
                    y = features$fu_outcome,
                    lambda = params$lambda,
                    alpha = params$alpha,
                    family="gaussian")  # gaussian for regression
  } 
  return(model)
}