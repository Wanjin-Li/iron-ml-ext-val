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

# EXTRACT TUNING RESULTS ----
# Assessing model selection results

#function takes all the csv files matching the pattern, concatenates them into a single data.table
#. and adds 2 columns for version and predicted_biomarker
extract_tune_files <- function(train_biomarker, prediction_task){
  dt <- list.files(path="./3_intermediate/tune_results/",
                   pattern = paste0("^base_mod_tune_", train_biomarker,"_", prediction_task),
                   full.names = TRUE) |>
    map_df(~fread(.))
  
  #Add columns for version and predicted_biomarker
  dt[, version := ifelse(version == "data_hgb_only", "Hemoglobin only", "Hemoglobin and Ferritin")]
  dt[, predicted_biomarker := ifesle(prediction_task == "predict_hgb", "Hemoglobin", "Ferritin")]
  
  return(dt)
}

# Extract into 4 data.tables
dt_tune_results_hgb_only_predict_hgb <- extract_tune_files("data_hgb_only", "predict_hgb")
dt_tune_results_hgb_only_predict_ferr <- extract_tune_files("data_hgb_only", "predict_ferr")
dt_tune_results_hgb_ferr_predict_hgb <- extract_tune_files("data_hgb_ferr", "predict_hgb")
dt_tune_results_hgb_ferr_predict_ferr <- extract_tune_files("data_hgb_ferr", "predict_ferr")

# merge into one big datatable
dt_tune_results <- rbindlist(list(dt_tune_results_hgb_only_predict_hgb,
                                   dt_tune_results_hgb_only_predict_ferr,
                                   dt_tune_results_hgb_ferr_predict_hgb,
                                   dt_tune_results_hgb_ferr_predict_ferr))

#RMSPE col names
cols_rmspe <- paste0("rmspe_rpt",
                     rep(1:3, each = 5),
                     "_fold",
                     rep(1:5, times = 3))

# Compute upper and lower CI bounds for each model configuration
# from the standard deviation
# using students t distribution with 14 degrees of freedom
dt_tune_results[, rmspe_sd := sd(.SD), .SDcols = cols_rmspe]
dt_tune_results[, rmspe_upper := rmspe_sd + qt(0.975, 14)*rmspe_sd/sqrt(15)]
dt_tune_results[, rmspe_lower := rmspe_sd - qt(0.975, 14)*rmspe_sd/sqrt(15)]

#save
fwrite(dt_tune_results, file = "./3_intermediate/dt_tune_results_all.csv")











# ENSEMBLE MODEL SELECTION -----


# put all this in loop to do for each versuib/prediction task

for (version in c()){
  for (predicted_biomarker in c()){
    dt_tune_results_temp <- dt_tune_results[version == "Hemoglobin and Ferritin" &
                                              predicted_biomarker == "Hemoglobin",]
    
    # Extract lower bound of top model
    
    # Select subset of models not statistically different from top model
    
    # ENSEMBLE 1: model architecture not considered
    
    # Calculate pairwise correlation between each base model config in set
    
    # Select unique base model configs in rows 1-3
    
    # If <6 models included, continue down list until you have 6 model configs in your list
    
    
    # ENSEMBLE 2: Top 2 models of top 3 archetecture
    
    # Get top mean RMSPE of each model architecture
    
    # Extract top 2 models of top 3 architectures
    
    
    # Assess the ensembles in cross validation
    

    # base_model_specs() should return a list with mod_name, hyperparameters, data for 5fld 3rpt CV
    
    # run_ensemble_assess()
    
    
    
  }
  }



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
# ggplot(dt_tune_results, aes(x = model, y = res_mean, fill=version, color=version))+
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
    geom_point(position = position_jitter(width = .15), size = .5, alpha = 0.8,
               aes(color=version))+
    scale_x_discrete(name="", labels=c("",""))+
    scale_y_continuous(name=paste0("Mean ", metric, " of each candidate model configuration"), 
                       labels = scales::percent_format(scale = 1))+  # For integers, use: labels =waiver())+#, limits=c(.1,1))+
    theme(legend.position = "bottom", #legend.spacing.x = unit(0.1, 'cm'),
          axis.text.x=element_text(size=10),
          strip.text.x = element_text(size = 15),  # facet grid text
          #legend.text=element_text(size=8),
          legend.margin = margin(-8,0,-8,0),
          axis.ticks.y = element_blank())+
    geom_hline(data=top_mods, aes(color=version, yintercept = top_mean_res))
  
    return(p)
  
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

