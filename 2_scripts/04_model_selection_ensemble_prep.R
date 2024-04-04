library(data.table)
library(pROC)
library(ggplot2)
library(readxl)
library(scales)
library(tidyverse)
library(ggforce)
library(rsample)
library(xgboost)
library(randomForest)
library(gridExtra)
library(glmnet)
library(purrr)
theme_set(theme_bw())
source('https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R')
source("./2_scripts/utility_functions.R")

# extract hyperparam info
en_param_sets <- fread("./3_intermediate/hyperparameters/en_hyperparameters.csv")
rf_param_sets <- fread("./3_intermediate/hyperparameters/rf_hyperparameters.csv")
xgb_param_sets <- fread("./3_intermediate/hyperparameters/xgb_hyperparameters.csv")
cb_param_sets <- fread("./3_intermediate/hyperparameters/cb_hyperparameters.csv")

# EXTRACT TUNING RESULTS ----
# Assessing model selection results
#function takes all the csv files matching the pattern, concatenates them into a single data.table
#. and adds 2 columns for version and predicted_biomarker
filepath = "./3_intermediate/tune_results/"
extract_tune_files <- function(path, prediction_task, train_biomarker, train_algorithm){
  dt <- list.files(path,
                   pattern = paste0("^base_mod_tune_", prediction_task, "_", train_biomarker, "_", train_algorithm),
                   full.names = TRUE) |>
    map_df(~fread(.))
  
  #Add columns for version and predicted_biomarker
  dt[, version := ifelse(train_biomarker == "data_hgb_only", "Hemoglobin only", "Hemoglobin and Ferritin")]
  dt[, predicted_biomarker := ifelse(prediction_task == "predict_hgb", "Hemoglobin", "Ferritin")]
  
  setorder(dt, hyperparam_idx)
  
  return(dt)
}

# Extract results from each algorithm into 4 data.tables

# predict hgb + data hgb only ----
dt_tune_results_hgb_only_predict_hgb_EN <- extract_tune_files(filepath, "predict_hgb", "data_hgb_only", "EN")
dt_tune_results_hgb_only_predict_hgb_RF <- extract_tune_files(filepath, "predict_hgb", "data_hgb_only", "RF")
dt_tune_results_hgb_only_predict_hgb_XGB <- extract_tune_files(filepath, "predict_hgb", "data_hgb_only", "XGB")

dt_tune_results_hgb_only_predict_hgb_CB <- extract_tune_files(filepath, "predict_hgb", "data_hgb_only", "CB")

## CB only ----
# 1st attempt: initially 3200 hyperparams; downsized later on
cb_param_sets3200 <- expand.grid(
  loss_function = 'RMSE',
  logging_level = 'Silent', # suppress iteration results
  depth = seq(2, 16, 2), # Similar to max_depth in XGBoost; 16 at max for catboost; initial training  used depth = seq(2, 16, 2)
  learning_rate = c(0.01, 0.05, 0.1, 0.2, 0.3), # Similar to eta in XGBoost
  iterations = seq(200, 1000, 200), # Number of trees
  l2_leaf_reg = c(1, 3, 6, 9), # Similar to lambda in XGBoost
  rsm = c(0.5, 0.7, 0.9, 1)) # Similar to subsample in XGBoost

# merge params with catboost results
dt_tune_results_hgb_only_predict_hgb_CB <- cbind(dt_tune_results_hgb_only_predict_hgb_CB, cb_param_sets3200)

# delete tuning results from hyperparams with depth=16 for catboost
dt_tune_results_hgb_only_predict_hgb_CB <- dt_tune_results_hgb_only_predict_hgb_CB[depth!=16,]

# delete learning rate = c(0.2, 0.3)
dt_tune_results_hgb_only_predict_hgb_CB <- dt_tune_results_hgb_only_predict_hgb_CB[learning_rate!=0.2 & learning_rate!=0.3, ]

# delete rsm = c(0.9)
dt_tune_results_hgb_only_predict_hgb_CB <- dt_tune_results_hgb_only_predict_hgb_CB[rsm!=0.9, ]

# remove extraneous params columns
dt_tune_results_hgb_only_predict_hgb_CB <- dt_tune_results_hgb_only_predict_hgb_CB[, colnames(cb_param_sets3200):=NULL]

# re-code hyperparam_indx
dt_tune_results_hgb_only_predict_hgb_CB$hyperparam_idx <- as.numeric(row.names(dt_tune_results_hgb_only_predict_hgb_CB))
# range(dt_tune_results_hgb_only_predict_hgb_CB$hyperparam_idx)


# predict hgb + data hgb ferr ----
dt_tune_results_hgb_ferr_predict_hgb_EN <- extract_tune_files(filepath, "predict_hgb", "data_hgb_ferr", "EN")
dt_tune_results_hgb_ferr_predict_hgb_RF <- extract_tune_files(filepath, "predict_hgb", "data_hgb_ferr", "RF")
dt_tune_results_hgb_ferr_predict_hgb_XGB <- extract_tune_files(filepath, "predict_hgb", "data_hgb_ferr", "XGB")

## CB only ----

dt_tune_results_hgb_ferr_predict_hgb_CB <- extract_tune_files(filepath, "predict_hgb", "data_hgb_ferr", "CB")

# 2nd attempt: remove the max depth = 16 -> 2800 hyperparams
# then downsized to 2800 hyperparams
cb_param_sets2800 <- expand.grid(
  loss_function = 'RMSE',
  logging_level = 'Silent', # suppress iteration results
  depth = seq(2, 14, 2), # Similar to max_depth in XGBoost; 16 at max for catboost; initial training  used depth = seq(2, 16, 2)
  learning_rate = c(0.01, 0.05, 0.1, 0.2, 0.3), # Similar to eta in XGBoost
  iterations = seq(200, 1000, 200), # Number of trees
  l2_leaf_reg = c(1, 3, 6, 9), # Similar to lambda in XGBoost
  rsm = c(0.5, 0.7, 0.9, 1)) # Similar to subsample in XGBoost

newpath = "./3_intermediate/tune_results/base_mod_CB_params841_1260_12Feb2024"

update_cb_results_param <- function(dt_result, cb_param_set, filepath, 
                                    prediction_task, train_biomarker, train_algorithm){
  # merge params with catboost results from the 2nd version of hyperparam sets which only includes rsm = c(0.5, 0.7)
  dt_result <- cbind(dt_result, cb_param_set[1:1400, ])
  
  # delete learning rate = c(0.2, 0.3)
  dt_result <- dt_result[learning_rate!=0.2 & learning_rate!=0.3, ]
  
  # read hyperparam results from the 3rd version of hyperparams set which includes results with rsm = c(1)
  dt_result_param841_to_1260 <- extract_tune_files(newpath, prediction_task, train_biomarker, train_algorithm)
  
  # remove extraneous params columns
  dt_result <- dt_result[, colnames(cb_param_set):=NULL]
  
  # combine rmspe results from 2rd version and 3rd version of hyperparams
  dt_result <- rbind(dt_result, dt_result_param841_to_1260)
  
  # re-code hyperparam_indx
  dt_result$hyperparam_idx <- as.numeric(row.names(dt_result))
  
  return(dt_result)
}

dt_tune_results_hgb_ferr_predict_hgb_CB <- update_cb_results_param(dt_tune_results_hgb_ferr_predict_hgb_CB, cb_param_sets2800, newpath,
                                                                   "predict_hgb", "data_hgb_ferr", "CB")

# predict ferr + data hgb only ----
dt_tune_results_hgb_only_predict_ferr_EN <- extract_tune_files(filepath, "predict_ferr", "data_hgb_only", "EN")
dt_tune_results_hgb_only_predict_ferr_RF <- extract_tune_files(filepath, "predict_ferr", "data_hgb_only", "RF")
dt_tune_results_hgb_only_predict_ferr_XGB <- extract_tune_files(filepath, "predict_ferr", "data_hgb_only", "XGB")

## CB only ----
dt_tune_results_hgb_only_predict_ferr_CB <- extract_tune_files(filepath, "predict_ferr", "data_hgb_only", "CB")
dt_tune_results_hgb_only_predict_ferr_CB <- update_cb_results_param(dt_tune_results_hgb_only_predict_ferr_CB, cb_param_sets2800, newpath,
                                                                   "predict_ferr", "data_hgb_only", "CB")
# predict ferr + data hgb ferr ----
dt_tune_results_hgb_ferr_predict_ferr_EN <- extract_tune_files(filepath, "predict_ferr", "data_hgb_ferr", "EN")
dt_tune_results_hgb_ferr_predict_ferr_RF <- extract_tune_files(filepath, "predict_ferr", "data_hgb_ferr", "RF")
dt_tune_results_hgb_ferr_predict_ferr_XGB <- extract_tune_files(filepath, "predict_ferr", "data_hgb_ferr", "XGB")

## CB only ----
dt_tune_results_hgb_ferr_predict_ferr_CB <- extract_tune_files(filepath, "predict_ferr", "data_hgb_ferr", "CB")
dt_tune_results_hgb_ferr_predict_ferr_CB <- update_cb_results_param(dt_tune_results_hgb_ferr_predict_ferr_CB, cb_param_sets2800, newpath,
                                                                    "predict_ferr", "data_hgb_ferr", "CB")



# merge into one big datatable and calculate rmspe summary statistics

dt_tune_results_hgb_only_predict_hgb_all <- rbind(dt_tune_results_hgb_only_predict_hgb_EN,
                                                  dt_tune_results_hgb_only_predict_hgb_RF,
                                                  dt_tune_results_hgb_only_predict_hgb_XGB,
                                                  dt_tune_results_hgb_only_predict_hgb_CB)

dt_tune_results_hgb_ferr_predict_hgb_all <- rbind(dt_tune_results_hgb_ferr_predict_hgb_EN,
                                                  dt_tune_results_hgb_ferr_predict_hgb_RF,
                                                  dt_tune_results_hgb_ferr_predict_hgb_XGB,
                                                  dt_tune_results_hgb_ferr_predict_hgb_CB)

dt_tune_results_hgb_only_predict_ferr_all <- rbind(dt_tune_results_hgb_only_predict_ferr_EN,
                                                   dt_tune_results_hgb_only_predict_ferr_RF,
                                                   dt_tune_results_hgb_only_predict_ferr_XGB,
                                                   dt_tune_results_hgb_only_predict_ferr_CB)

dt_tune_results_hgb_ferr_predict_ferr_all <- rbind(dt_tune_results_hgb_ferr_predict_ferr_EN,
                                                   dt_tune_results_hgb_ferr_predict_ferr_RF,
                                                   dt_tune_results_hgb_ferr_predict_ferr_XGB,
                                                   dt_tune_results_hgb_ferr_predict_ferr_CB)


calc_rmspe_stats <- function(dt_hgb_only_predict_hgb,
                             dt_hgb_only_predict_ferr,
                             dt_hgb_ferr_predict_hgb,
                             dt_hgb_ferr_predict_ferr
                             ){
  
  dt_tune_results <- rbind(dt_hgb_only_predict_hgb,
                        dt_hgb_only_predict_ferr,
                        dt_hgb_ferr_predict_hgb,
                        dt_hgb_ferr_predict_ferr)
  
  
  #RMSPE col names
  cols_rmspe <- paste0("rmspe_rpt",
                       rep(1:3, each = 5),
                       "_fold",
                       rep(1:5, times = 3))
  
  # Compute upper and lower CI bounds for each model configuration
  # from the standard deviation
  # using students t distribution with 14 degrees of freedom
  
  dt_tune_results[, rmspe_sd := sd(.SD), .SDcols = cols_rmspe, by = 1:nrow(dt_tune_results)]
  
  dt_tune_results[, rmspe_lower := rmspe_mean - qt(0.975, 14)*rmspe_sd/sqrt(15)]
  dt_tune_results[, rmspe_upper := rmspe_mean + qt(0.975, 14)*rmspe_sd/sqrt(15)]
  dt_tune_results[, modelID := paste0(model, ".", hyperparam_idx)]
  
  return(dt_tune_results)
  
}


tune_results_all <- calc_rmspe_stats(dt_tune_results_hgb_only_predict_hgb_all,
                                     dt_tune_results_hgb_only_predict_ferr_all,
                                     dt_tune_results_hgb_ferr_predict_hgb_all,
                                     dt_tune_results_hgb_ferr_predict_ferr_all)

tune_results_all_predict_hgb <- tune_results_all[predicted_biomarker=="Hemoglobin",]
tune_results_all_predict_ferr <- tune_results_all[predicted_biomarker=="Ferritin",]


#save 
fwrite(tune_results_all, file = "./3_intermediate/tune_results/base_model/rmspe_tune_results_all.csv")
fwrite(tune_results_all_predict_hgb, file = "./3_intermediate/tune_results/base_model/rmspe_tune_results_all_predict_hgb.csv")
fwrite(tune_results_all_predict_ferr, file = "./3_intermediate/tune_results/base_model/rmspe_tune_results_all_predict_ferr.csv")

# # PLOT TUNING RESULTS ----
# ggplot(dt_tune_results, aes(x = model, y = res_mean, fill=version, color=version))+
#   geom_sina()+
#   theme(legend.position = "bottom")+
#   ylab("Mean overall AUC\ncross validation across 15 tuning sets")+xlab("Model types")+
#   scale_x_discrete(labels = c("Elastic Net", "Elastic net\nwith interactions", "Random forest", "Regression trees", "Gradient\nboosted trees"))

plot_model_hyperparams <- function(dt_tune_results, predict_biomarkers, metric) {
  mod_names <- c("Elastic net", "Random forest", "Gradient boosted trees", "Catboost")
  names(mod_names)<-c("EN", "RF", "XGB", "CB")
  
  metric_upper <- toupper(metric)
  top_mods <- dt_tune_results[, list(top_mean_rmspe = min(rmspe_mean)), by = version]
  print(top_mods)
  dt_tune_results[, model := factor(model, levels = names(mod_names))]
  
  ggplot(dt_tune_results, aes(x = version, y = rmspe_mean, fill=version))+
    facet_wrap(vars(model), ncol=1, labeller = labeller(model=mod_names))+
    coord_flip()+
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8)+
    geom_point(position = position_jitter(width = .15), size = .5, alpha = 0.8,
               aes(color=version))+
    scale_x_discrete(name="", labels=c("",""))+
    scale_y_continuous(name=paste0("Mean ", metric, " of each candidate model configuration"), 
                       labels = percent_format(scale = 1))+  # For integers, use: labels =waiver())+#, limits=c(.1,1))+
    theme(legend.position = "bottom",
          axis.ticks.y = element_blank())+
    geom_hline(data=top_mods, aes(color=version, yintercept = top_mean_rmspe)) +
    guides(fill = guide_legend(nrow = 2))

    # rmse_tuning_no_ensemble_predict_hgb.png
  # fname_svg <- paste0("./4_output/updates/figs/", metric, "_tuning_no_ensemble_", predict_biomarkers, ".svg")
  # fname_png <- paste0("./4_output/updates/figs/", metric, "_tuning_no_ensemble_", predict_biomarkers, ".png")
  # ggsave(fname_svg, width = 6, height = 5.5, unit = "in")
  # ggsave(fname_png, width = 6, height = 5.5, unit = "in")
  
}


rmspe_all_predict_hgb <- fread("./3_intermediate/tune_results/base_model/rmspe_tune_results_all_predict_hgb.csv")
rmspe_all_predict_ferr <- fread("./3_intermediate/tune_results/base_model/rmspe_tune_results_all_predict_ferr.csv")

# call plot function
p1_hgb <- plot_model_hyperparams(dt_tune_results=rmspe_all_predict_hgb, predict_biomarkers="predict_hgb", metric="RMSPE") 
p1_ferr <- plot_model_hyperparams(dt_tune_results=rmspe_all_predict_ferr, predict_biomarkers="predict_ferr", metric="RMSPE") 

# combine plots
p1_combined <- ggpubr::ggarrange(p1_hgb, p1_ferr, labels = c("A", "B"), ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom") 
ggsave("./4_output/updates/figs/combined_RMSPE_tuning_no_ensemble.png", plot = p1_combined, width = 10, height = 8, unit = "in")

# predict hgb - min rmspe by version
min(rmspe_all_predict_hgb[rmspe_all_predict_hgb$version == "Hemoglobin only"]$rmspe_mean)
min(rmspe_all_predict_hgb[rmspe_all_predict_hgb$version == "Hemoglobin and Ferritin"]$rmspe_mean)

# predict ferr - min rmspe by version
min(rmspe_all_predict_ferr[rmspe_all_predict_ferr$version == "Hemoglobin only"]$rmspe_mean)
min(rmspe_all_predict_ferr[rmspe_all_predict_ferr$version == "Hemoglobin and Ferritin"]$rmspe_mean)  



# ENSEMBLE MODEL SELECTION -----

# put all this in loop to do for each version/prediction task
dt_tune_results_all <- fread("./3_intermediate/tune_results/base_model/rmspe_tune_results_all.csv")
setorder(dt_tune_results_all, version, rmspe_mean)

cols_rmspe <- paste0("rmspe_rpt",
                     rep(1:3, each = 5),
                     "_fold",
                     rep(1:5, times = 3))


list_version <- c("Hemoglobin only", "Hemoglobin and Ferritin")
list_predicted_biomarker <- c("Hemoglobin", "Ferritin")


dt_ensemble_model <- data.frame(matrix(NA, nrow = 0, ncol = 9, 
                                       dimnames = list(NULL, 
                                                       c("mod1", "mod2", "mod3", "mod4", "mod5", "mod6",
                                                         "ensemble", "version", "predicted_biomarker"))))
list_ensemble_rmspe <- list()

for (version in list_version){
  v <- version
  
  for (predicted_biomarker in list_predicted_biomarker){
    p <- predicted_biomarker
    
    print(paste0("Start: version - ", v, "| predicted biomaker - ", p))
    
    dt_tune_results_temp <- dt_tune_results_all[version == v &
                                              predicted_biomarker == p, ]
    setorder(dt_tune_results_temp, version, rmspe_mean)
    
    # Extract lower bound of top model
    top_mod_rmspe <- unlist(dt_tune_results_temp[version == v & predicted_biomarker == p,][, ..cols_rmspe][1])
    
    # ENSEMBLE 1: model architecture not considered
    
    # Select subset of models not statistically different from top model; 
    dt_mods_nonsignf <- data.frame(matrix(nrow=0, ncol = ncol(dt_tune_results_temp))) 
    
    # compare rmspe of the top models with other models and find the ones whose rmspes are not statistically different from the top 
    for (i in 2:nrow(dt_tune_results_temp)){
      test_mod_rmspe <- unlist(dt_tune_results_temp[, ..cols_rmspe][i])
      nonparam_t_test <- wilcox.test(top_mod_rmspe, test_mod_rmspe, exact = FALSE)
      p_value <- nonparam_t_test$p.value
      if (p_value > 0.05){
        mod_nonsignf <- dt_tune_results_temp[i,]
        dt_mods_nonsignf <- rbind(dt_mods_nonsignf, mod_nonsignf)
      }else{
        dt_mods_nonsignf <- dt_mods_nonsignf
      }
      # print(paste0("Completed row: ", i))
    }
    
    top_mod <- dt_tune_results_temp[1,] 
    # combine subset of models not statistically different from the top model
    dt_top_mods_nonsigf <- rbind(top_mod, dt_mods_nonsignf)
    
    # convert long to wide format
    top_mods_nonsignf <- dt_top_mods_nonsigf[, .SD, .SDcols = c("modelID", cols_rmspe)]
    top_mods_nonsignf <- dcast(melt(top_mods_nonsignf, id.vars="modelID"), variable ~ modelID)
    
    # Calculate pairwise correlation between each base model config in set
    # c is the correlations matrix
    c_mod <- cor(top_mods_nonsignf[,-1])
    
    # keep only the lower triangle by filling upper with NA
    c_mod[upper.tri(c_mod, diag=TRUE)] <- NA
    m_mod <- data.table(reshape2::melt(c_mod))
    
    # specify the column order based on rmspe
    custom_order <- dt_top_mods_nonsigf$modelID
    
    # Create a custom ordering column based on the specified starting value
    m_mod$order_col <- match(m_mod$Var1, custom_order)  
    
    # Order the table based on the custom ordering column and sort by descending absolute correlation
    ordered_m_mod <- m_mod[order(m_mod$order_col, abs(m_mod$value)), ][!is.na(value)][, -"order_col"]
    
    # Select 6 unique base model configs in rows 1-3
    # If <6 models included, continue down list until you have 6 model configs in your list
    # 6 unique base models include the top model and 5 model that are least correlated with the top model
    
    # create an empty to store the top 6 models
    ensemble_mod1 <- c()
    
    top_indx <- 1
    stop_outer_loop <- FALSE
    
    for (i in 1:nrow(ordered_m_mod)){
      # print(paste0("row: ", i))
      for (j in c("Var1", "Var2")){
        # print(paste0("column: ", j))
        current_mod <- as.character(ordered_m_mod[i, get(j)])
        while (!(current_mod %in% ensemble_mod1) & top_indx < 7){
          ensemble_mod1[top_indx] <- current_mod
          top_indx <- top_indx + 1
          # print(paste0("top model index: ", top_indx))
          if (top_indx==7){
            stop_outer_loop <- TRUE
            break
          }
        } 
        if (stop_outer_loop){
          break
        }
      }
      if (stop_outer_loop){
        break
      }
    }
    
    # extract rmspe info for the top 6 selected models
    ensemble_mod1_rmspe <- dt_tune_results_temp[dt_tune_results_temp$modelID %in% ensemble_mod1,]
    
    # remove the redundant columns
    ensemble_mod1_rmspe <- ensemble_mod1_rmspe[, -c(3:17)]

    # save selected ensemble models into dataframes
    t <- data.frame(t(ensemble_mod1))
    t$ensemble <- "ensemble1"
    t$version <- v
    t$predicted_biomarker <- p
    
    dt_ensemble_model[nrow(dt_ensemble_model)+1, ] <- t
    
    # save ensemble rmspe results
    list_ensemble_rmspe[[length(list_ensemble_rmspe)+1]] <- ensemble_mod1_rmspe
    
    
    # ENSEMBLE 2: Top 2 models of top 3 architecture
    
    # Get top mean RMSPE of each model architecture
    top_mod_rmspe2 <- dt_tune_results_temp[, head(.SD, 1), by="model"][, c("rmspe_mean", "model", "modelID")]
    top_three_mod <- top_mod_rmspe2[, head(.SD, 3)]$model
    
    # Extract top 2 models of top 3 architectures
    ensemble_mod2_rmspe <- dt_tune_results_temp[dt_tune_results_temp$model %in% top_three_mod][, head(.SD, 2), by="model"]
    ensemble_mod2_rmspe <- ensemble_mod2_rmspe[, -c(3:17)]
    
    # get the list of top models selected for ensemble 2
    ensemble_mod2 <- c()
    for (i in 1:nrow(ensemble_mod2_rmspe)){
      ensemble_mod2[i] <- as.character(ensemble_mod2_rmspe[, "modelID"][i])
    }
    
    t2 <- data.frame(t(ensemble_mod2))
    t2$ensemble <- "ensemble2"
    t2$version <- v
    t2$predicted_biomarker <- p
    
    dt_ensemble_model[nrow(dt_ensemble_model)+1, ] <- t2
    
    # save ensemble rmspe results
    list_ensemble_rmspe[[length(list_ensemble_rmspe)+1]] <- ensemble_mod2_rmspe
  }
}

dt_ensemble_model <- setDT(dt_ensemble_model)


# Assess the ensembles in cross validation
# use regualr cv not nested cv

# base_model_specs() should return a list with mod_name, hyperparameters, data for 5fld 3rpt CV

## Predict hgb ----

### data hgb only ----
rsplit_factors_hgb_only <- readRDS("./3_intermediate/rsplits/main_model/pred_hgb/rsplit_factors_hgb_only.rds")
rsplit_OH_hgb_only <- readRDS("./3_intermediate/rsplits/main_model/pred_hgb/rsplit_OH_hgb_only.rds")

#### ensemble 1 ----
em_temp <- dt_ensemble_model[ensemble == "ensemble1" &
                                                      version == "Hemoglobin only" &
                                                      predicted_biomarker == "Hemoglobin", ]

em_temp <- unlist(unname(as.list(em_temp[,1:6])))

base_mod_spec_hgb_only_pred_hgb <- list()
for(mod_id in em_temp){
  base_mod_spec_hgb_only_pred_hgb[[mod_id]] <- mod_spec_as_list(mod_id, biomarkers="hgb_only")
}

saveRDS(base_mod_spec_hgb_only_pred_hgb, "./3_intermediate/ensemble/updates/em1_base_mod_spec_hgb_only_predict_hgb.RDS")

#### ensemble 2 ----
em_temp <- dt_ensemble_model[ensemble == "ensemble2" &
                                             version == "Hemoglobin only" &
                                             predicted_biomarker == "Hemoglobin", ]

em_temp <- unlist(unname(as.list(em_temp[,1:6])))

base_mod_spec_hgb_only_pred_hgb <- list()
for(mod_id in em_temp){
  base_mod_spec_hgb_only_pred_hgb[[mod_id]] <- mod_spec_as_list(mod_id, biomarkers="hgb_only")
}

saveRDS(base_mod_spec_hgb_only_pred_hgb, "./3_intermediate/ensemble/updates/em2_base_mod_spec_hgb_only_predict_hgb.RDS")


### data hgb ferr ----
rsplit_factors_hgb_ferr <- readRDS("./3_intermediate/rsplits/main_model/pred_hgb/rsplit_factors_hgb_ferr.rds")
rsplit_OH_hgb_ferr <- readRDS("./3_intermediate/rsplits/main_model/pred_hgb/rsplit_OH_hgb_ferr.rds")

#### ensemble 1 ----

em_temp <- dt_ensemble_model[ensemble == "ensemble1" &
                                             version == "Hemoglobin and Ferritin" &
                                             predicted_biomarker == "Hemoglobin", ]

em_temp <- unlist(unname(as.list(em_temp[,1:6])))

base_mod_spec_hgb_ferr_pred_hgb <- list()
for(mod_id in em_temp){
  base_mod_spec_hgb_ferr_pred_hgb[[mod_id]] <- mod_spec_as_list(mod_id, biomarkers="hgb_ferr")
}

saveRDS(base_mod_spec_hgb_ferr_pred_hgb, "./3_intermediate/ensemble/updates/em1_base_mod_spec_hgb_ferr_predict_hgb.RDS")

#### ensemble 2 ----

em_temp <- dt_ensemble_model[ensemble == "ensemble2" &
                            version == "Hemoglobin and Ferritin" &
                            predicted_biomarker == "Hemoglobin", ]

em_temp <- unlist(unname(as.list(em_temp[,1:6])))

base_mod_spec_hgb_ferr_pred_hgb <- list()
for(mod_id in em_temp){
  base_mod_spec_hgb_ferr_pred_hgb[[mod_id]] <- mod_spec_as_list(mod_id, biomarkers="hgb_ferr")
}

saveRDS(base_mod_spec_hgb_ferr_pred_hgb, "./3_intermediate/ensemble/updates/em2_base_mod_spec_hgb_ferr_predict_hgb.RDS")



## Predict ferr ----

### data hgb only ----
rsplit_factors_hgb_only <- readRDS("./3_intermediate/rsplits/main_model/pred_ferr/rsplit_factors_hgb_only.rds")
rsplit_OH_hgb_only <- readRDS("./3_intermediate/rsplits/main_model/pred_ferr/rsplit_OH_hgb_only.rds")

#### ensemble 1 ----
em_temp <- dt_ensemble_model[ensemble == "ensemble1" &
                               version == "Hemoglobin only" &
                               predicted_biomarker == "Ferritin", ]

em_temp <- unlist(unname(as.list(em_temp[,1:6])))

base_mod_spec_hgb_only_pred_ferr <- list()
for(mod_id in em_temp){
  base_mod_spec_hgb_only_pred_ferr[[mod_id]] <- mod_spec_as_list(mod_id, biomarkers="hgb_only")
}

saveRDS(base_mod_spec_hgb_only_pred_ferr, "./3_intermediate/ensemble/updates/em1_base_mod_spec_hgb_only_predict_ferr.RDS")

#### ensemble 2 ----
em_temp <- dt_ensemble_model[ensemble == "ensemble2" &
                               version == "Hemoglobin only" &
                               predicted_biomarker == "Ferritin", ]

em_temp <- unlist(unname(as.list(em_temp[,1:6])))

base_mod_spec_hgb_only_pred_ferr <- list()
for(mod_id in em_temp){
  base_mod_spec_hgb_only_pred_ferr[[mod_id]] <- mod_spec_as_list(mod_id, biomarkers="hgb_only")
}

saveRDS(base_mod_spec_hgb_only_pred_ferr, "./3_intermediate/ensemble/updates/em2_base_mod_spec_hgb_only_predict_ferr.RDS")


### data hgb ferr ----
rsplit_factors_hgb_ferr <- readRDS("./3_intermediate/rsplits/main_model/pred_ferr/rsplit_factors_hgb_ferr.rds")
rsplit_OH_hgb_ferr <- readRDS("./3_intermediate/rsplits/main_model/pred_ferr/rsplit_OH_hgb_ferr.rds")

#### ensemble 1 ----

em_temp <- dt_ensemble_model[ensemble == "ensemble1" &
                               version == "Hemoglobin and Ferritin" &
                               predicted_biomarker == "Ferritin", ]

em_temp <- unlist(unname(as.list(em_temp[,1:6])))

base_mod_spec_hgb_ferr_pred_ferr <- list()
for(mod_id in em_temp){
  base_mod_spec_hgb_ferr_pred_ferr[[mod_id]] <- mod_spec_as_list(mod_id, biomarkers="hgb_ferr")
}

saveRDS(base_mod_spec_hgb_ferr_pred_ferr, "./3_intermediate/ensemble/updates/em1_base_mod_spec_hgb_ferr_predict_ferr.RDS")


#### ensemble 2 ----

em_temp <- dt_ensemble_model[ensemble == "ensemble2" &
                               version == "Hemoglobin and Ferritin" &
                               predicted_biomarker == "Ferritin", ]

em_temp <- unlist(unname(as.list(em_temp[,1:6])))

base_mod_spec_hgb_ferr_pred_ferr <- list()
for(mod_id in em_temp){
  base_mod_spec_hgb_ferr_pred_ferr[[mod_id]] <- mod_spec_as_list(mod_id, biomarkers="hgb_ferr")
}

saveRDS(base_mod_spec_hgb_ferr_pred_ferr, "./3_intermediate/ensemble/updates/em2_base_mod_spec_hgb_ferr_predict_ferr.RDS")

# EMSEMBLE ASSESSMENT ----

# Rscript --vanilla /home/wanjinli/iron-ml-ext-val/2_scripts/04_run_ensemble.R 1















# FINAL MODEL SELECTION -----

# Add ensemble 1 and 2 to the tune_result_all table 



# Select top model for each prediction task



# PLOT TUING RESULTS WITH ENSEMBLE ----













# . ------
# . ------
# . ------
# CHEN-YANG CODE ------
# SOME OF WHICH TO BE ADAPTED INTO ABOVE, OTHER TO BE DELETED -Alton

# # PLOT TUNING RESULTS ----
# ggplot(dt_tune_results, aes(x = model, y = rmspe_mean, fill=version, color=version))+
#   geom_sina()+
#   theme(legend.position = "bottom")+
#   ylab("Mean overall AUC\ncross validation across 15 tuning sets")+xlab("Model types")+
#   scale_x_discrete(labels = c("Elastic Net", "Elastic net\nwith interactions", "Random forest", "Regression trees", "Gradient\nboosted trees"))

plot_model_hyperparams <- function(dt_tune_results, predict_biomarkers, metric) {
  mod_names <- c("Gradient boosted trees", "Random forest", "Elastic net")
  names(mod_names)<-c("XGB", "RF", "EN")
  
  metric_upper <- toupper(metric)
  top_mods <- dt_tune_results[, list(top_mean_res = min(res_mean)), by = version]
  print(top_mods)
  dt_tune_results[, model := factor(model, levels = names(mod_names))]
  
  ggplot(dt_tune_results, aes(x = version, y = res_mean, fill=version))+
    facet_wrap(vars(model), ncol=1, labeller = labeller(model=mod_names))+
    coord_flip()+
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8)+
    geom_point(position = position_jitter(width = .15), linewidth = .5, alpha = 0.8,
               aes(color=version))+
    scale_x_discrete(name="", labels=c("",""))+
    scale_y_continuous(name=paste0("Mean ", metric, " of each candidate model configuration"), 
                       labels = percent_format(scale = 1))+  # For integers, use: labels =waiver())+#, limits=c(.1,1))+
    theme(legend.position = "bottom",
          axis.ticks.y = element_blank())+
    geom_hline(data=top_mods, aes(color=version, yintercept = top_mean_res))
  
  # rmse_tuning_no_ensemble_predict_hgb.png
  fname_svg <- paste0("./4_output/figs/", metric, "_tuning_no_ensemble_", predict_biomarkers, ".svg")
  fname_png <- paste0("./4_output/figs/", metric, "_tuning_no_ensemble_", predict_biomarkers, ".png")
  ggsave(fname_svg, width = 6, height = 5.5, unit = "in")
  ggsave(fname_png, width = 6, height = 5.5, unit = "in")

}



f1 <- fread("./3_intermediate/tune_results/main_model/rmspe_tune_results_all_predict_hgb.csv")
f3 <- fread("./3_intermediate/tune_results/main_model/rmspe_tune_results_all_predict_ferr.csv")
plot_model_hyperparams(dt_tune_results=f1, predict_biomarkers="predict_hgb", metric="RMSPE") 
plot_model_hyperparams(dt_tune_results=f3, predict_biomarkers="predict_ferr", metric="RMSPE") 

min(f1[f1$version == "Hemoglobin and Ferritin"]$res_mean)  # predict hgb
min(f1[f1$version == "Hemoglobin only"]$res_mean)
min(f3[f3$version == "Hemoglobin and Ferritin"]$res_mean)  # predict ferr
min(f3[f3$version == "Hemoglobin only"]$res_mean)





# SELECT MODELS FOR ENSEMBLE & CONSTRUCT ----
tune_results_all_predict_hgb <- fread("./3_intermediate/tune_results/main_model/rmspe_tune_results_all_predict_hgb.csv")
setorder(tune_results_all_predict_hgb, version, res_mean)  # sort by RMSPE
tune_results_all_predict_ferr <- fread("./3_intermediate/tune_results/main_model/rmspe_tune_results_all_predict_ferr.csv")
setorder(tune_results_all_predict_ferr, version, res_mean)  # sort by RMSPE

# function to get correlations
get_model_corr <- function(dt_tune_results, vers, model_name) {
  setorder(dt_tune_results, version, res_mean)
  
  Top_mod_mean_res <- unlist(dt_tune_results[version==vers & model==model_name, "res_mean"][1])
  Top_mod_res_sd <- unlist(dt_tune_results[version==vers & model==model_name, "res_sd"][1])
  top_mods <- dt_tune_results[version==vers & model==model_name & 
                                res_mean <= (Top_mod_mean_res + Top_mod_res_sd),]  # within 1 s.d. of top model

  top_mods <- top_mods[ , .SD, .SDcols = c("modelID", paste0("rmspe_", "repeat",
                                                             formatC(ceiling(1:15/5)), "_fold", formatC((1:15-1)%%5 + 1)))]
  top_mods<-dcast(melt(top_mods, id.vars="modelID"), variable ~ modelID)
  c_mod <- cor(top_mods[,-1])
  # c is the correlations matrix
  # keep only the lower triangle by filling upper with NA
  c_mod[upper.tri(c_mod, diag=TRUE)] <- NA
  m_mod <- data.table(melt(c_mod))
  ## sort by descending absolute correlation
  m_mod <- m_mod[order(abs(m_mod$value)), ]
  return(m_mod)
}

######## take a look at plot_model_hyperparams plot outputs to decide which base models to use

# Get Ensemble base models - Predict Hemoglobin ----
dt_tune_results <- tune_results_all_predict_hgb

## Version: Hemoglobin and Ferritin ----
# Take 6 models: 5 XGB + 1 RF
# Top XGB (+ 4 XGB within 1 sd of top XGB that are least correlated)
# Top RF
vers <- "Hemoglobin and Ferritin"  
corr <- get_model_corr(dt_tune_results=dt_tune_results, vers=vers, model_name="XGB")  # select 4 least correlated models
dt_tune_results[version==vers & model=="XGB", "modelID"][[1]][1]
head(corr)

######## take a look at corr and select models
mods_for_ensemble_hgb_ferr_predict_hgb <- c(dt_tune_results[version==vers & model=="XGB", "modelID"][[1]][1],  # top xgb model
                                            as.character(corr[1, Var1]),
                                            as.character(corr[1, Var2]),
                                            as.character(corr[2, Var1]),
                                            as.character(corr[2, Var2]),
                                            dt_tune_results[version==vers & model=="RF", "modelID"][[1]][1])  # top rf model
mods_for_ensemble_hgb_ferr_predict_hgb

## Version: Hemoglobin only ----
# Take 6 models: 5 XGB + 1 RF
# Top XGB (+ 4 XGB within 1 sd of top XGB that are least correlated)
# Top RF
vers <- "Hemoglobin only"  
corr <- get_model_corr(dt_tune_results=dt_tune_results, vers=vers, model_name="XGB") 
dt_tune_results[version==vers & model=="XGB", "modelID"][[1]][1]
head(corr)

########## take a look at corr and select models
mods_for_ensemble_hgb_only_predict_hgb <- c(dt_tune_results[version==vers & model=="XGB", "modelID"][[1]][1],  # top xgb model
                                            as.character(corr[1, Var1]),
                                            as.character(corr[1, Var2]),
                                            as.character(corr[2, Var2]),
                                            as.character(corr[3, Var2]),
                                            dt_tune_results[version==vers & model=="RF", "modelID"][[1]][1])
mods_for_ensemble_hgb_only_predict_hgb


# Get Ensemble base models - Predict Ferritin ----
dt_tune_results <- tune_results_all_predict_ferr

## Version: Hemoglobin and Ferritin ----
# Take 6 models: 5 EN + 1 RF
# Top EN (+ 4 EN within 1 sd of top EN that are least correlated)
# Top RF
vers <- "Hemoglobin and Ferritin"  
corr <- get_model_corr(dt_tune_results=dt_tune_results, vers=vers, model_name="EN") 
dt_tune_results[version==vers & model=="EN", "modelID"][[1]][1]
head(corr)

# take a look at corr and select models (make sure correlation does not have top model)
mods_for_ensemble_hgb_ferr_predict_ferr <- c(dt_tune_results[version==vers & model=="EN", "modelID"][[1]][1],  # top EN model
                                             as.character(corr[1, Var1]),  # row 1 col 1
                                             as.character(corr[1, Var2]),
                                             as.character(corr[2, Var1]),
                                             as.character(corr[3, Var1]),
                                             dt_tune_results[version==vers & model=="RF", "modelID"][[1]][1])
mods_for_ensemble_hgb_ferr_predict_ferr

## Version: Hemoglobin only ----
# Take 6 models: 6 RF
# Top RF (+ 5 RF within 1 sd of top RF that are least correlated)
vers <- "Hemoglobin only"  
corr <- get_model_corr(dt_tune_results=dt_tune_results, vers=vers, model_name="RF") 
dt_tune_results[version==vers & model=="RF", "modelID"][[1]][1]  # top mod
head(corr)

# take a look at corr and select models (make sure correlation does not have top model)
mods_for_ensemble_hgb_only_predict_ferr <- c(dt_tune_results[version==vers & model=="RF", "modelID"][[1]][1],  # top RF model
                                             as.character(corr[1, Var1]),
                                             as.character(corr[1, Var2]),
                                             as.character(corr[2, Var1]),
                                             as.character(corr[3, Var1]),
                                             as.character(corr[4, Var2]))

mods_for_ensemble_hgb_only_predict_ferr

# COMPILE LIST OF MODELS FOR ENSEMBLE ----

#read in splits and hyperparam sets
xgb_param_sets <- fread("./3_intermediate/hyperparameters/xgb_hyperparameters.csv")
rf_param_sets <-fread("./3_intermediate/hyperparameters/rf_hyperparameters.csv")
en_param_sets <-fread("./3_intermediate/hyperparameters/en_hyperparameters.csv")

source("./2_scripts/utility_functions.R")

intermediate_directory <- './3_intermediate/ensemble'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}

# Predict hgb: hf and h - 5XGB + 1RF
# Predict ferr: hf - 5 EN + 1 RF; h - 5 RF + 1 EN

## Predict hgb ----
rsplit_factors_hgb_ferr <- readRDS("./3_intermediate/rsplits/main_model/pred_hgb/rsplit_factors_hgb_ferr.rds")
rsplit_factors_hgb_only <- readRDS("./3_intermediate/rsplits/main_model/pred_hgb/rsplit_factors_hgb_only.rds")
rsplit_OH_hgb_ferr <- readRDS("./3_intermediate/rsplits/main_model/pred_hgb/rsplit_OH_hgb_ferr.rds")
rsplit_OH_hgb_only <- readRDS("./3_intermediate/rsplits/main_model/pred_hgb/rsplit_OH_hgb_only.rds")

base_mod_spec_hgb_ferr_predict_hgb <- list()
for(mod_id in mods_for_ensemble_hgb_ferr_predict_hgb){
  base_mod_spec_hgb_ferr_predict_hgb[[mod_id]]<-mod_spec_as_list(mod_id, biomarkers="hgb_ferr")
}
base_mod_spec_hgb_only_predict_hgb <- list()
for(mod_id in mods_for_ensemble_hgb_only_predict_hgb){
  base_mod_spec_hgb_only_predict_hgb[[mod_id]]<-mod_spec_as_list(mod_id, biomarkers="hgb_only")
}
saveRDS(base_mod_spec_hgb_ferr_predict_hgb, "./3_intermediate/ensemble/base_mod_spec_hgb_ferr_predict_hgb.RDS")
saveRDS(base_mod_spec_hgb_only_predict_hgb, "./3_intermediate/ensemble/base_mod_spec_hgb_only_predict_hgb.RDS")

## Predict ferritin ----
rsplit_factors_hgb_ferr <- readRDS("./3_intermediate/rsplits/main_model/pred_ferr/rsplit_factors_hgb_ferr.rds")
rsplit_factors_hgb_only <- readRDS("./3_intermediate/rsplits/main_model/pred_ferr/rsplit_factors_hgb_only.rds")
rsplit_OH_hgb_ferr <- readRDS("./3_intermediate/rsplits/main_model/pred_ferr/rsplit_OH_hgb_ferr.rds")
rsplit_OH_hgb_only <- readRDS("./3_intermediate/rsplits/main_model/pred_ferr/rsplit_OH_hgb_only.rds")

base_mod_spec_hgb_ferr_predict_ferr <- list()
for(mod_id in mods_for_ensemble_hgb_ferr_predict_ferr){
  base_mod_spec_hgb_ferr_predict_ferr[[mod_id]]<-mod_spec_as_list(mod_id, biomarkers="hgb_ferr")
}
base_mod_spec_hgb_only_predict_ferr <- list()
for(mod_id in mods_for_ensemble_hgb_only_predict_ferr){
  base_mod_spec_hgb_only_predict_ferr[[mod_id]]<-mod_spec_as_list(mod_id, biomarkers="hgb_only")
}
saveRDS(base_mod_spec_hgb_ferr_predict_ferr, "./3_intermediate/ensemble/base_mod_spec_hgb_ferr_predict_ferr.RDS")
saveRDS(base_mod_spec_hgb_only_predict_ferr, "./3_intermediate/ensemble/base_mod_spec_hgb_only_predict_ferr.RDS")

