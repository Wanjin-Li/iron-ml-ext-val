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

# TUNING RESULTS ----
# Assessing model selection results
prediction_tasks <- c("predict_hgb", "predict_ferr")
# metric_patterns <- c("*rmse.csv", "*mae.csv")
metric_patterns <- c("*rmspe.csv")

for (prediction_task in prediction_tasks) {
  for (metric_pattern in metric_patterns) {
    if (prediction_task == "predict_hgb") {
      # Read in tuning results
      tune_results_data_hgb_only <- list.files(path="./3_intermediate/tune_results/main_model/predict_hgb/data_hgb_only",
                                               pattern=metric_pattern,
                                               full.names = TRUE) %>% map_df(~fread(.))
      
      tune_results_data_hgb_ferr <- list.files(path="./3_intermediate/tune_results/main_model/predict_hgb/data_hgb_ferr",
                                               pattern=metric_pattern,
                                               full.names = TRUE) %>% map_df(~fread(.))
      
    } else if (prediction_task == "predict_ferr") {
      # Read in tuning results
      tune_results_data_hgb_only <- list.files(path="./3_intermediate/tune_results/main_model/predict_ferr/data_hgb_only",
                                               pattern=metric_pattern,
                                               full.names = TRUE) %>% map_df(~fread(.))
      
      tune_results_data_hgb_ferr <- list.files(path="./3_intermediate/tune_results/main_model/predict_ferr/data_hgb_ferr",
                                               pattern=metric_pattern,
                                               full.names = TRUE) %>% map_df(~fread(.))
    }
    tune_results_all <- rbind(cbind(tune_results_data_hgb_only, version="Hemoglobin only"),
                              cbind(tune_results_data_hgb_ferr, version="Hemoglobin and Ferritin"))
    
    
    metric <- str_replace(metric_pattern, "\\*", "") %>% str_replace(., ".csv", "_")  # *rmse.csv -> rmse_
    
    tune_results_all[ , res_sd := sd(.SD), .SDcols = paste0(metric, "repeat",
                                                            formatC(ceiling(1:15/5)), "_fold", formatC((1:15-1)%%5 + 1)), 
                                                            by=1:nrow(tune_results_all)]
    tune_results_all[ , res_lb := res_mean - res_sd]
    tune_results_all[ , res_ub := res_mean + res_sd]
    tune_results_all[, modelID := paste0(model, ".", hyperparam_idx)]
    
    # *rmse.csv, *mae.csv
    setorder(tune_results_all, version, res_mean)  # lowest value at top (lower error better)
    
    # rmse_tune_results_all_predict_hgb.csv
    file_out <- paste0("./3_intermediate/tune_results/main_model/",
                       metric,
                       "tune_results_all_",
                       prediction_task,
                       ".csv")
    # ex: "./3_intermediate/tune_results/main_model/rmspe_tune_results_all_predict_hgb.csv"
    fwrite(tune_results_all, file_out)  
  }
}

# # PLOT TUNING RESULTS FOR ALL HYPERPARAMETER SETS ----
# ggplot(tune_results_all, aes(x = model, y = res_mean, fill=version, color=version))+
#   geom_sina()+
#   theme(legend.position = "bottom")+
#   ylab("Mean overall AUC\ncross validation across 15 tuning sets")+xlab("Model types")+
#   scale_x_discrete(labels = c("Elastic Net", "Elastic net\nwith interactions", "Random forest", "Regression trees", "Gradient\nboosted trees"))

plot_model_hyperparams <- function(tune_results_all, predict_biomarkers, metric) {
  mod_names <- c("Gradient boosted trees", "Random forest", "Elastic net")
  names(mod_names)<-c("XGB", "RF", "EN")
  
  metric_upper <- toupper(metric)
  top_mods <- tune_results_all[, list(top_mean_res = min(res_mean)), by = version]
  print(top_mods)
  tune_results_all[, model := factor(model, levels = names(mod_names))]
  
  ggplot(tune_results_all, aes(x = version, y = res_mean, fill=version))+
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
    geom_hline(data=top_mods, aes(color=version, yintercept = top_mean_res))
  
  # rmse_tuning_no_ensemble_predict_hgb.png
  fname_svg <- paste0("./4_output/figs/", metric, "_tuning_no_ensemble_", predict_biomarkers, ".svg")
  fname_png <- paste0("./4_output/figs/", metric, "_tuning_no_ensemble_", predict_biomarkers, ".png")
  ggsave(fname_svg, width = 6, height = 5.5, unit = "in")
  ggsave(fname_png, width = 6, height = 5.5, unit = "in")
  
}



f1 <- fread("./3_intermediate/tune_results/main_model/rmspe_tune_results_all_predict_hgb.csv")
f3 <- fread("./3_intermediate/tune_results/main_model/rmspe_tune_results_all_predict_ferr.csv")
plot_model_hyperparams(tune_results_all=f1, predict_biomarkers="predict_hgb", metric="RMSPE") 
plot_model_hyperparams(tune_results_all=f3, predict_biomarkers="predict_ferr", metric="RMSPE") 

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
get_model_corr <- function(tune_results_all, vers, model_name) {
  setorder(tune_results_all, version, res_mean)
  
  Top_mod_mean_res <- unlist(tune_results_all[version==vers & model==model_name, "res_mean"][1])
  Top_mod_res_sd <- unlist(tune_results_all[version==vers & model==model_name, "res_sd"][1])
  top_mods <- tune_results_all[version==vers & model==model_name & 
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
tune_results_all <- tune_results_all_predict_hgb

## Version: Hemoglobin and Ferritin ----
# Take 6 models: 5 XGB + 1 RF
# Top XGB (+ 4 XGB within 1 sd of top XGB that are least correlated)
# Top RF
vers <- "Hemoglobin and Ferritin"  
corr <- get_model_corr(tune_results_all=tune_results_all, vers=vers, model_name="XGB")  # select 4 least correlated models
tune_results_all[version==vers & model=="XGB", "modelID"][[1]][1]
head(corr)

######## take a look at corr and select models
mods_for_ensemble_hgb_ferr_predict_hgb <- c(tune_results_all[version==vers & model=="XGB", "modelID"][[1]][1],  # top xgb model
                                            as.character(corr[1, Var1]),
                                            as.character(corr[1, Var2]),
                                            as.character(corr[2, Var1]),
                                            as.character(corr[2, Var2]),
                                            tune_results_all[version==vers & model=="RF", "modelID"][[1]][1])  # top rf model
mods_for_ensemble_hgb_ferr_predict_hgb

## Version: Hemoglobin only ----
# Take 6 models: 5 XGB + 1 RF
# Top XGB (+ 4 XGB within 1 sd of top XGB that are least correlated)
# Top RF
vers <- "Hemoglobin only"  
corr <- get_model_corr(tune_results_all=tune_results_all, vers=vers, model_name="XGB") 
tune_results_all[version==vers & model=="XGB", "modelID"][[1]][1]
head(corr)

########## take a look at corr and select models
mods_for_ensemble_hgb_only_predict_hgb <- c(tune_results_all[version==vers & model=="XGB", "modelID"][[1]][1],  # top xgb model
                                            as.character(corr[1, Var1]),
                                            as.character(corr[1, Var2]),
                                            as.character(corr[2, Var2]),
                                            as.character(corr[3, Var2]),
                                            tune_results_all[version==vers & model=="RF", "modelID"][[1]][1])
mods_for_ensemble_hgb_only_predict_hgb


# Get Ensemble base models - Predict Ferritin ----
tune_results_all <- tune_results_all_predict_ferr

## Version: Hemoglobin and Ferritin ----
# Take 6 models: 5 EN + 1 RF
# Top EN (+ 4 EN within 1 sd of top EN that are least correlated)
# Top RF
vers <- "Hemoglobin and Ferritin"  
corr <- get_model_corr(tune_results_all=tune_results_all, vers=vers, model_name="EN") 
tune_results_all[version==vers & model=="EN", "modelID"][[1]][1]
head(corr)

# take a look at corr and select models (make sure correlation does not have top model)
mods_for_ensemble_hgb_ferr_predict_ferr <- c(tune_results_all[version==vers & model=="EN", "modelID"][[1]][1],  # top EN model
                                            as.character(corr[1, Var1]),  # row 1 col 1
                                            as.character(corr[1, Var2]),
                                            as.character(corr[2, Var1]),
                                            as.character(corr[3, Var1]),
                                            tune_results_all[version==vers & model=="RF", "modelID"][[1]][1])
mods_for_ensemble_hgb_ferr_predict_ferr

## Version: Hemoglobin only ----
# Take 6 models: 6 RF
# Top RF (+ 5 RF within 1 sd of top RF that are least correlated)
vers <- "Hemoglobin only"  
corr <- get_model_corr(tune_results_all=tune_results_all, vers=vers, model_name="RF") 
tune_results_all[version==vers & model=="RF", "modelID"][[1]][1]  # top mod
head(corr)

# take a look at corr and select models (make sure correlation does not have top model)
mods_for_ensemble_hgb_only_predict_ferr <- c(tune_results_all[version==vers & model=="RF", "modelID"][[1]][1],  # top RF model
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

