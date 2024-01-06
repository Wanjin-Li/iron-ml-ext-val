library(stringr)
library(xgboost)
library(randomForest)
library(glmnet)
library(rsample)
library(caret)
library(dplyr)
library(data.table)
library(catboost)

## DATA MUNGING --------

#Creates 3 versions of a given features table: onehot encoded, factor, and interactions
gen_features_list <- function(dt, version){
  dt.md <- dt
  # str(dt.md)
  
  # set Gender_F as character since loads in as int
  dt.md$sex <- as.character(dt.md$sex)
  
  # Create 2 versions: one with characters as factors and
  # one with categorical variables 1-hot encoded for model types that require that
  
  # One hot encoding for XGB -----
  # Create dummy one-hot variables for categoricals
  dt.OH <- data.table(model.matrix(fu_outcome ~ ., data=dt.md))
  dt.OH <- cbind(dt.OH, "fu_outcome" = dt.md$fu_outcome)
  
  # Code all Character as Factor -----
  colnames(dt.md)[unlist(dt.md[ , lapply(.SD, is.character),][1,])]
  dt.md <- dt.md %>% mutate_if(is.character, as.factor)
  # str(dt.md)
  
  return(list("OH"=dt.OH,
              "factor"=dt.md))
  
}

# ## DATA PROCESSING ----

# #Load top model
# top.model <- xgb.load("./data/topmodel")
# calib_weights <- readRDS("./data/calib_weights_noplatt.RDS")





# MODEL TRAIN AND SELECTION FUNCTIONS ---------------

# MODEL ASSESSMENT using specified single model configuration (non ensemble)
# and set of hyperparameters to assess in nested CV.
# Must also feed the appropriate format of the dataset split for CV
# and provide path for saving the results
# idx_start: When  splitting a large set of hyperparams
#  across multiple jobs run on a computing cluster, set id_start
#  to the number of the first row in hyperparam table under assessment
#  in this job.


# Trains and assesses model configurations
#. (algorithm/mod_name + hyperparameter set)
#. specified by the arguments
tune_subset <- function(
    mod_name, #"XGB", "RF", "EN", "CB"
    param_sets, #Table of hyperparameter sets to use
    dt_split,
    fname_results,
    start, end #rows of param_sets from which to draw hyperparameter sets
) {
  
  #Run model assessment
  # Construct table for storing results
  #. ROWS: hyperparam sets
  #. COLS: one to store RMSPE for each fold/rpt
  
  rmspe_res_cols <-  matrix(ncol = 15, nrow = 0)
  #mae_res_cols <-  matrix(ncol = 15, nrow = 0)
  
  # colnames(rmse_res_cols) <- paste0("rmse_repeat",
  #                                   formatC(ceiling(1:15/5)),
  #                                   "_fold",
  #                                   formatC((1:15-1)%%5 + 1))
  
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
  
  for (row_num in start:end){
    # get set of hyperparameters
    params <- as.list(unlist(param_sets[row_num,]))  
    
    # Run 3 rpt 5 fold CV for this hyperparam set with cv_convig and append
    dt.results.rmspe <- rbind(dt.results.rmspe,
                              cv_config(dt_split, params, row_num, mod_name))
    
    # print status
    print(paste0("Completed hyperparam set ", row_num,". ", Sys.time()))
  }
  
  #calculate mean RMSPE for each model configuration (row)
  dt.results.rmspe[ , rmspe_mean := rowMeans(.SD), .SDcols = colnames(rmspe_res_cols)]
  
  
  #append columns with model configuration info
  
  # Save csv file to disk
  fwrite(dt.results.rmspe, fname_results)
}







# CV CONFIGURATION (called by tune_subset() function)
# For param set, perform CV across all inner folds
# and return RMSE for each of 3X5=15 inner CV sets

cv_config <- function(dt_split, params, row_num, mod_name){
  
  # # get data for this model configuration
  rmspe_outcome <- list(model=mod_name, hyperparam_idx = row_num)
  rsplit_obj <- dt_split$splits
  
  for (i in seq_along(rsplit_obj)){
    # get data for this fold/repeat
    # validate_date is data in fold
    # train_data is all other data
    
    train_data <- analysis(rsplit_obj[[i]])
    validate_data <- assessment(rsplit_obj[[i]])
    idxs_info <- rsplit_obj[[i]]
    rpt <- as.numeric(gsub("[^0-9]", "", idxs_info$id[1]))
    fold <- as.numeric(gsub("[^0-9]", "", idxs_info$id[2]))
    
    #Create to store predictions for the selected fold/repeat (for RMSPE calcs)
    dt_preds_for_fold <- data.table("prediction"= numeric(),
                                    "fu_outcome"= numeric())
    
    #train model & save predictions in table
    tryCatch({
      dt_preds_for_fold <- rbind(
        dt_preds_for_fold,
        mod_eval(train_data,
                 validate_data, 
                 params, 
                 mod_name)
      )
    }, error = function(error_condition){
      cat("Could not fit model on rpt ", rpt, " fold ", fold)
    })
    
    #calculate RMSPE
    EPSILON <-  1e-10  # prevent division by zero
    rmspe <- (sqrt(mean(((dt_preds_for_fold$fu_outcome - dt_preds_for_fold$prediction) / 
                           (dt_preds_for_fold$fu_outcome + EPSILON))^2))) * 100
    
    rmspe_outcome[paste0("rmspe_rpt",
                         rpt,
                         "_fold",
                         fold)] <- rmspe
    
  }
  
  return(rmspe_outcome)
}






# MODEL EVALUATION (called by cv_config)
#  for a given train data and assess data,
#. model algorithm/name, and hyperparameter set,
#. run the model and return RMSPE.
#  When used for ensembles, set return_train_pred to
#  true so those can be accessed to tune the ensemble

mod_eval <- function(train_data, 
                     validate_data, 
                     params, 
                     mod_name, 
                     return_train_pred=FALSE){
  set.seed(444)  # set the seed for reproducibility
  
  # change response variable to "fu_outcome" for easier coding below
  lookup <- c(fu_outcome = "fu_hgb", fu_outcome = "fu_log_ferritin")
  train_data <- train_data %>% rename(any_of(lookup))
  validate_data <- validate_data %>% rename(any_of(lookup))
  
  ncol_rsplit <- ncol(train_data)  
  
  # RANDOM FOREST
  if (mod_name == "RF"){
    rf_fit <- randomForest(fu_outcome~.,  # fu_hgb, fu_log_ferritin
                           data = train_data,
                           nodesize = params$nodesize,
                           mtry=params$mtry,
                           ntree = params$ntree,
                           replace = params$replace)
    
    # generate predictions
    preds = as.data.table(predict(rf_fit, validate_data))
    
    if(return_train_pred==TRUE){  # for ensemble training only
      pred_train <- as.data.table(predict(rf_fit, train_data))
      preds<-rbind(cbind(Set="Train", pred_train), cbind(Set="Test", preds))
      colnames(preds) <- c("Set", "prediction")
      fu_outcome = rbind(train_data[,"fu_outcome"],  # fu_hgb, fu_log_ferritin
                         validate_data[,"fu_outcome"])   # fu_hgb, fu_log_ferritin
      
    } else {
      colnames(preds) <- paste0("prediction")
      fu_outcome = validate_data[,"fu_outcome"]   # fu_hgb, fu_log_ferritin
    }
    
    #ELASTIC NET (NO INTERACTIONS)
  } else if (mod_name == "EN"){
    fit <- glmnet(x = as(data.matrix(train_data[ , 1:(ncol_rsplit-1)]), "dgCMatrix"),
                  y = train_data$fu_outcome,  # fu_hgb, fu_ferritin
                  lambda = params$lambda,
                  alpha = params$alpha,
                  family="gaussian")  # gaussian for regression
    
    # generate predictions
    X_test <- data.matrix(validate_data[ , 1:(ncol_rsplit-1)])  # remove last column (outcome)
    preds = as.data.table(predict(fit, newx = X_test))
    
    if(return_train_pred==TRUE){  # for ensemble training
      X_train <- data.matrix(train_data[ , 1:(ncol_rsplit-1)])  # remove last col (outcome)
      pred_train <- as.data.table(predict(fit, newx = X_train ))
      preds<-rbind(cbind(Set="Train", pred_train), cbind(Set="Test", preds))
      colnames(preds) <- c("Set", "prediction")
      fu_outcome = unlist(rbind(train_data[,"fu_outcome"],  # fu_hgb, fu_log_ferritin
                                validate_data[,"fu_outcome"]))  # fu_hgb, fu_log_ferritin
      
    } else{
      colnames(preds) <- paste0("prediction")
      fu_outcome = unlist(validate_data[, "fu_outcome"])  # fu_hgb, fu_log_ferritin
    }
    
    # XGB GRADIENT BOOSTED MACHINE
  } else if (mod_name == "XGB"){
    # Extract train and test sets in xgb's special format
    xgb.train = xgb.DMatrix(data = as.matrix(train_data[,-"fu_outcome"]),  # fu_hgb, fu_log_ferritin
                            label = as.matrix(train_data[,"fu_outcome"]))  # fu_hgb, fu_log_ferritin
    xgb.test = xgb.DMatrix(data = as.matrix(validate_data[,-"fu_outcome"]),  # fu_hgb, fu_log_ferritin
                           label = as.matrix(validate_data[,"fu_outcome"]))  # fu_hgb, fu_log_ferritin
    
    # Fit model
    xgb.fit = xgb.train(params = params,
                        data = xgb.train,
                        nrounds = 10000,
                        nthread = 36,
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
      fu_outcome = unlist(rbind(train_data[,"fu_outcome"],  # fu_hgb, fu_log_ferritin
                                validate_data[,"fu_outcome"]))  # fu_hgb, fu_log_ferritin
      
    } else{
      colnames(preds) <- paste0("prediction")
      fu_outcome = unlist(validate_data[, "fu_outcome"])  # fu_hgb, fu_log_ferritin
    }
  } 
  else if (mod_name == "CB"){
    # https://catboost.ai/en/docs/concepts/r-reference_catboost-train
    x_train <- train_data[, -"fu_outcome"]
    y_train <- train_data$fu_outcome
    
    x_test <- validate_data[, -"fu_outcome"]
    y_test <- validate_data$fu_outcome
    
    # Load pools for training and testing sets
    train_pool <- catboost.load_pool(data = x_train, label = y_train)
    test_pool <- catboost.load_pool(data = x_test, label = y_test)
    
    # convert char to numeric for certain hyperparams
    params[["depth"]] <- as.numeric(params[["depth"]])
    params[["learning_rate"]] <- as.numeric(params[["learning_rate"]])
    params[["iterations"]] <- as.numeric(params[["iterations"]])
    params[["l2_leaf_reg"]] <- as.numeric(params[["l2_leaf_reg"]])
    params[["rsm"]] <- as.numeric(params[["rsm"]])
    
    # Train
    cb_fit <- catboost.train(train_pool, params = params)
    
    # Predict
    preds = as.data.table(catboost.predict(cb_fit,
                                           test_pool,
                                           prediction_type = 'RawFormulaVal'))
    
    # Save predictions
    colnames(preds) <- paste0("prediction")
    fu_outcome = validate_data$fu_outcome  
  }
  
  results <- cbind(preds,fu_outcome)
  
  return(results)
}



# ENSEMBLE MODELS ------

#Construct model specs as lists
#RF uses dt_split_factor
#GBM uses dt_split_xgb
#EN (no interaction) uses dt_split_xgb

# JENNIFER: need to fix dt_split so it will do 5-fold 3-rpt CV, nothing about outer folds!----

mod_spec_as_list <- function(mod_id, biomarkers){
  mod_id_split <- unlist(str_split(mod_id, "\\."))  #     # XGB.1406 -> XGB
  mod_name <-mod_id_split[1]  
  hyperparam_idx <- as.numeric(mod_id_split[2])
  if (mod_name == "XGB"){
    dt_split <- get(paste0("rsplit_OH_", biomarkers))
    hyperparams <- as.list(xgb_param_sets[hyperparam_idx, ])
  } else if (mod_name=="RF"){
    dt_split <- get(paste0("rsplit_factors_", biomarkers))
    hyperparams <- as.list(rf_param_sets[hyperparam_idx, ])
  } else if (mod_name=="EN"){
    dt_split <- get(paste0("rsplit_factors_", biomarkers))
    hyperparams <- as.list(en_param_sets[hyperparam_idx, ])
  } 
  return(list(mod_name=mod_name,
              dt_split=dt_split,
              hyperparams = hyperparams))
}

##ENSEMBLE MODEL ASSESSMENT
run_ensemble_assess <- function(base_model_specs, path = "./3_intermediate/ensemble/", ensemble_config="NA", version="hgb_ferr_predict_ferr",
                                ensemble_type="average"){
  
  outcome<-list()
  outcome_base_mods <- list()
  EPSILON <- 1e-10  # for RMSPE to prevent division by 0
  
  # JENNIFER- this must be fixed, should not be using outer folds!!! ---------
  
  for (row_outer in 1:nrow(base_model_specs[[1]]$dt_split)){  # 1:15
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
      # train_res_in_fold <- dt_single_fold_preds[Set=="Train", list("train_res"=sum(prediction)/.N), by=base_model]
      
      # dt_single_fold_preds<-dt_single_fold_preds[train_res_in_fold, on="base_model"]
      dt_inner_fold_preds_base<-rbind(dt_inner_fold_preds_base,
                                      dt_single_fold_preds[Set=="Test", .SD, .SDcols=c("base_model", "prediction", "fu_outcome")])
      
      # model average: for each datapoint, get the average over all model predictions. e.g., if 6 models, then averaging over 6 predictions for each datapoint
      dt_inner_fold_preds<-rbind(dt_inner_fold_preds,
                                 dt_single_fold_preds[Set=="Test", list(
                                   "prediction"=mean(prediction),  
                                   "fu_outcome"=mean(fu_outcome)),
                                   by=donor_idx])
    }
    rmspe <- (sqrt(mean(((dt_inner_fold_preds$fu_outcome - dt_inner_fold_preds$prediction) / (dt_inner_fold_preds$fu_outcome + EPSILON))**2))) * 100
    
    outcome[paste0("rmspe_repeat",
                   formatC(ceiling(row_outer/5)),
                   "_fold",
                   formatC((row_outer-1)%%5 + 1))] <- rmspe
    
    print(paste0("outer fold complete: ",row_outer))
    
    outcome_base_mods[[paste0("rmspe_repeat",
                              formatC(ceiling(row_outer/5)),
                              "_fold",
                              formatC((row_outer-1)%%5 + 1))]] <- dt_inner_fold_preds_base[, list(prediction, fu_outcome), by=base_model]
  }
  
  dt.results <- as.data.table(outcome)
  dt.results[ , res_mean := rowMeans(.SD), .SDcols =  # get mean over all folds (average across all cols in the row)
                paste0("rmspe_repeat",
                       formatC(ceiling(1:15/5)),
                       "_fold",
                       formatC((1:15-1)%%5 + 1))]
  
  dt.results_basemods <- as.data.table(outcome_base_mods)
  
  fwrite(dt.results, paste0(path, paste0("ensemble_assess_results_", ensemble_config, "_", version,"_", ensemble_type,".csv")))
  fwrite(dt.results_basemods, paste0(path, paste0("ensemble_basemods_assess_results_", ensemble_config, "_",version,"_", ensemble_type,".csv")))
}




train_mod <- function(mod_name, params, features) {
  if (mod_name=="XGB"){
    #Extract train and test sets in xgb's special format
    train_matrix = xgb.DMatrix(data = as.matrix(features[,-"fu_outcome"]),
                               label = as.matrix(features[,"fu_outcome"]))
    set.seed(250)  # set the seed for reproducibility
    model=xgb.train(params=params,
                    data=train_matrix,
                    nrounds=10000,
                    early_stopping_rounds=10,
                    watchlist=list(val1=train_matrix),
                    verbose=0,
                    booster="gbtree",
                    gamma=3,
                    objective = "reg:squarederror",
                    eval_metric="rmse")
    
  } else if (mod_name=="RF"){
    set.seed(250)
    model=randomForest(fu_outcome~.,
                       data = features,
                       nodesize = params$nodesize,
                       mtry=params$mtry,
                       ntree = params$ntree,
                       replace = params$replace)
    
  } else if (mod_name=="EN"){
    set.seed(250)
    model=glmnet(x = as(data.matrix(features[,-"fu_outcome"]), "dgCMatrix"),
                 y = features$fu_outcome,
                 lambda = params$lambda,
                 alpha = params$alpha,
                 family="gaussian")  # gaussian for regression
  } 
  return(model)
}



# FEATURE IMPORTANCE ----


#Function for calculating overall AUC and each one-vs-rest AUC
auc_calcs <- function(dt.scores){
  #dt.scores<-dt.scores[fu_outcome!="Z-1"]
  dt.scores[, is_Z0 := ifelse(fu_outcome=="Z0",1,0)]
  dt.scores[, is_Z1 := ifelse(fu_outcome=="Z1",1,0)]
  dt.scores[, is_Z2 := ifelse(fu_outcome=="Z2",1,0)]
  dt.scores[, is_Z3 := ifelse(fu_outcome=="Z3",1,0)]
  
  AUCs <- dt.scores[ , list(Overall = multiclass.roc(fu_outcome~Z0+Z1+Z2+Z3)$auc,
                            Z0 = roc(is_Z0~Z0)$auc,
                            Z1 = roc(is_Z1~Z1)$auc,
                            Z2 = roc(is_Z2~Z2)$auc,
                            Z3 = roc(is_Z3~Z3)$auc)
  ]
  return(AUCs)
}

# Generate feature list, run  ensemble, return AUCs
gen_metric_row<-function(base_mods, validate_data, idx_rpt, idx_fld, feat_name){
  #Generate risk scores on test data - no perturbations
  risk_scores <- risk_scores_ensemble(features_list=gen_features_list(validate_data), base_mods=base_mods)  # 3 cols: donor_idx, prediction, fu_outcome
  
  print(risk_scores)
  stop()
  # risk_scores<-cbind(risk_scores, "fu_outcome"=validate_data$fu_outcome)
  # 
  # risk_scores[ , prediction := paste0("Z",max.col(risk_scores[,2:5])-1)]
  
  
  #Calc metrics and append to table
  
  EPSILON <-  1e-10  # prevent division by zero
  rmspe <- (sqrt(mean(((risk_scores$fu_outcome - risk_scores$prediction) / (risk_scores$fu_outcome + EPSILON))**2))) * 100
  
  output = c(idx_rpt, idx_fld, feat_name, rmspe)
  
  return(t(output))
}



# Performing variable importance across all model selection partitions using the permutation method
ensemble_feature_importance <- function(base_model_specs, 
                                        dt,
                                        path = "./3_intermediate/feature_importance/main_model/",
                                        configs,
                                        version="hgb_ferr_predict_hgb") {
  #data table to store results
  dt_feat_metrics <- data.table("rpt" = numeric(), 
                                "fold" = numeric(),
                                "feature"=numeric(),
                                "res"=numeric())
  
  lookup <- c(fu_outcome = "fu_hgb", fu_outcome = "fu_log_ferritin")
  dt <- dt %>% dplyr::rename(any_of(lookup))
  
  #rsplit
  set.seed(250)
  rsplit_unformatted <- nested_cv(dt,
                                  outside = vfold_cv(v=5, repeats=3),
                                  inside = vfold_cv(v=5))
  feat_names <- colnames(dt[,-"fu_outcome"])
  
  Sys.time()
  for (idx_rpt in 1:3){
    for (idx_fld in 1:5){
      #Generate training data
      train_data <- data.table(
        analysis(
          filter(
            rsplit_unformatted,
            id==paste0("Repeat",idx_rpt) &
              id2==paste0("Fold",idx_fld))$splits[[1]]
        ))
      features_list <- gen_features_list(train_data)
      
      
      #Train base models
      base_mods<-list()
      for (mod in names(base_model_specs)){
        print(mod)
        #extract model type and hyperparam set from id
        mod_id_split <- unlist(str_split(mod, "\\."))  
        mod_name <-mod_id_split[1]  # XGB.4066 -> XGB
        
        # #Extract training data for model assessment partition
        # rsplit_obj<-filter(base_model_specs[[mod]]$dt_split,
        #                    id==paste0("Repeat",idx_rpt) &
        #                      id2==paste0("Fold",idx_fld))$splits[[1]]
        
        if(mod_name == "XGB"){
          features <- features_list$OH
        } else{  # RF, EN
          features <- features_list$factor
        }
        
        # Develop base model
        base_mods[[mod]] <- train_mod(mod_name,
                                      params = base_model_specs[[mod]]$hyperparams,
                                      features = features)
      }
      
      #Extract test set for model assessment partition
      validate_data <- data.table(
        assessment(
          filter(rsplit_unformatted,
                 id==paste0("Repeat",idx_rpt) &
                   id2==paste0("Fold",idx_fld))$splits[[1]]))
      
      #append baseline metrics to table
      dt_feat_metrics <- rbind(dt_feat_metrics,
                               gen_metric_row(base_mods, validate_data, idx_rpt, idx_fld, "baseline"),
                               use.names=FALSE)
      
      for (idx_feat in 1:(ncol(dt)-1)){ # loop over features to perturb and calculate performance
        #shuffle selected feature
        dt_temp <- cbind(validate_data)[,feat_names[idx_feat] := sample(get(feat_names[idx_feat]),replace=FALSE)]
        #Run model and calculate risk scores
        dt_feat_metrics <- rbind(dt_feat_metrics,
                                 gen_metric_row(base_mods, dt_temp, idx_rpt, idx_fld, feat_names[idx_feat]),
                                 use.names=FALSE)
      }
      fwrite(dt_feat_metrics, file = paste0(path, configs, "_", version, ".csv"))
      print(paste0("Fold ", idx_fld, " Repeat ", idx_rpt, " complete ",Sys.time()))
    }
  }
}










#  PREDICTING WITH TRAINED MODELS ---------------------
#Generate uncalibrated scores from a model object, name, and features
raw_scores <- function(mod_name, model, features){
  if (mod_name=="XGB"){
    set.seed(250)
    xgb.test = xgb.DMatrix(data = as.matrix(features[,-"fu_outcome"]),
                           label = as.matrix(features[,"fu_outcome"]))
    preds = as.data.table(predict(model, xgb.test, reshape = T))
  } else if (mod_name=="RF"){
    set.seed(250)
    preds = as.data.table(predict(model, features))
  } else if (mod_name=="EN"){
    set.seed(250)
    X_test <- data.matrix(features[ , -"fu_outcome"])  # remove last column (outcome)
    preds = as.data.table(predict(model, newx = X_test))
  } 
  colnames(preds) <- paste0("prediction")
  fu_outcome = unlist(features[, "fu_outcome"])  # fu_hgb, fu_log_ferritin
  results <- cbind(preds,fu_outcome)
  return(results)
}


weight_score <- function(preds, weights){
  preds[, Q0 := weights[1]*Z0/( weights[1]*Z0+ weights[2]*Z1+ weights[3]*Z2+ weights[4]*Z3)]
  preds[, Q1 := weights[2]*Z1/( weights[1]*Z0+ weights[2]*Z1+ weights[3]*Z2+ weights[4]*Z3)]
  preds[, Q2 := weights[3]*Z2/( weights[1]*Z0+ weights[2]*Z1+ weights[3]*Z2+ weights[4]*Z3)]
  preds[, Q3 := weights[4]*Z3/( weights[1]*Z0+ weights[2]*Z1+ weights[3]*Z2+ weights[4]*Z3)]
  
  
  return(preds[, paste0("Q",0:3)])
}


#Generates calibrated predictions using top model if if it's an XGB model
risk_scores_XGB <- function(features,
                            model,
                            weights = rep(1,4)
){
  # #ONE HOT
  # dt.xgb <- data.table(model.matrix(fu_outcome ~ ., data=features))
  # dt.xgb <- dt.xgb[ , gender_menstrating_cohortsM := NULL]
  #GBM matrix
  dt.DMatrix = xgb.DMatrix(data = as.matrix(features))
  #Uncalibrated predictions
  preds = as.data.table(predict(model, dt.DMatrix, reshape = T))
  colnames(preds) <- paste0("Z", 0:3)
  return(weight_score(dt_ensemb_pred, weights))
}

#Generates calibrated predictions using top model if if it's an XGB model
risk_scores_rf <- function(features,
                           model,
                           weights = NA
){
  # rf_fit <- randomForest(
  #   fu_outcome~.,
  #   data = analysis(rsplit_obj),
  #   nodesize = params$nodesize,
  #   ntree = params$ntree,
  #   replace = params$replace
  # )
  #generate predictions
  preds = as.data.table(predict(model, features, type="prob"))
  #Uncalibrated predictions
  colnames(preds) <- paste0("Z", 0:3)
  
  if(is.na(weights)){
    return(preds)
  } else{
    return(weight_score(preds, weights))
  }
}




risk_scores_ensemble <- function(features_list,  # 2 versions: OH=one hot, factor
                                 base_mods,  #LIST OF MODELS WITH mod_id AS NAME
                                 weights = NA,
                                 incl_base_preds=FALSE){
  
  dt_base_preds <- data.table("base_model" = character(), "prediction"=numeric(), "fu_outcome"= numeric())
  
  # Gen predictions for each base model
  for (base_mod_idx in 1:length(base_mods)){
    #extract model name
    mod_name <- unlist(strsplit(names(base_mods)[base_mod_idx], "\\."))[1]
    if(mod_name=="XGB"){ features = features_list$OH } else{ features = features_list$factor }
    
    #Generate raw (uncallibrated) scores and concat
    dt_base_preds <- rbind(
      dt_base_preds,
      cbind("base_model" = names(base_mods)[[base_mod_idx]],
            raw_scores(mod_name,
                       base_mods[[base_mod_idx]], features)))
  }
  # print(dt_base_preds)
  #     base_model prediction fu_outcome
  # 1:   XGB.4066   12.74841   13.15789
  # 2:   XGB.4066   12.80722   11.84211
  # 3:   XGB.4066   13.26275   13.81579
  # 4:   XGB.4066   13.04470   13.15789
  # 5:   XGB.4066   13.04760   12.82895
  # ---                                 
  # 3146:     RF.208   12.46228   13.15789
  # 3147:     RF.208   13.72977   16.80000
  # 3148:     RF.208   14.58893   14.14474
  # 3149:     RF.208   12.64662   12.17105
  
  dt_base_preds[,donor_idx := rep(1:(nrow(dt_base_preds)/base_mod_idx), base_mod_idx)]
  
  # print(dt_base_preds)
  #     base_model prediction fu_outcome donor_idx
  # 1:   XGB.4066   12.74841   13.15789         1
  # 2:   XGB.4066   12.80722   11.84211         2
  # 3:   XGB.4066   13.26275   13.81579         3
  # 4:   XGB.4066   13.04470   13.15789         4
  # 5:   XGB.4066   13.04760   12.82895         5
  # ---                                           
  # 3146:     RF.208   12.46228   13.15789       521
  # 3147:     RF.208   13.72977   16.80000       522
  # 3148:     RF.208   14.58893   14.14474       523
  # 3149:     RF.208   12.64662   12.17105       524
  # 3150:     RF.208   12.76384   12.50000       525
  
  #model average
  dt_ensemb_pred <- dt_base_preds[, list("prediction"=mean(prediction),
                                         "fu_outcome"=mean(fu_outcome)),
                                  by=donor_idx]
  
  if(incl_base_preds==TRUE){
    if(is.na(weights)){
      return(list("ensemble"=dt_ensemb_pred,
                  "base_mods"=dt_base_preds))
    } else{
      return(list("ensemble"=weight_score(dt_ensemb_pred, weights),
                  "base_mods"=dt_base_preds))
    }
  } else {
    if(is.na(weights)){
      return(dt_ensemb_pred)
    } else{
      return(weight_score(dt_ensemb_pred, weights))
    }
  }
  
}