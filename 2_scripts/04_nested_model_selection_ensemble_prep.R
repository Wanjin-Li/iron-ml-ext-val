# This script prepares the dataset and workflow for model selection 
# using nested cross-validation for ensemble models.

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
filepath = "./3_intermediate/tune_results/nested_model/inner_fold/"

extract_tune_files_nested <- function(path, prediction_task, train_biomarker, outer_fold){
  dt <- list.files(path,
                   pattern = paste0("^base_mod_tune_", prediction_task, "_", train_biomarker, "_", outer_fold),
                   full.names = TRUE) |>
    map_df(~fread(.))

  #Add columns for version and predicted_biomarker
  dt[, version := ifelse(train_biomarker == "data_hgb_only", "Hemoglobin only", "Hemoglobin and Ferritin")]
  dt[, predicted_biomarker := ifelse(prediction_task == "predict_hgb", "Hemoglobin", "Ferritin")]
  
  setorder(dt, hyperparam_idx)
  return(dt)
}


# calculate rmspe mean and sd
calc_rmspe_stats <- function(dt_hgb_only_predict_hgb,
                             dt_hgb_only_predict_ferr,
                             dt_hgb_ferr_predict_hgb,
                             dt_hgb_ferr_predict_ferr){
  
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


# Extract results from each algorithm into 4 data.tables
predict_outcome <- c("predict_hgb", "predict_ferr")
train_version <- c("data_hgb_only", "data_hgb_ferr")
outer_fold_id <- c("outer_fold_1", "outer_fold_2", "outer_fold_3")

list_dt_tune_results_hgb_only_predict_hgb <- list()
list_dt_tune_results_hgb_ferr_predict_hgb <- list()
list_dt_tune_results_hgb_only_predict_ferr <- list()
list_dt_tune_results_hgb_ferr_predict_ferr <- list()

for (fold_id in outer_fold_id){
  for (outcome in predict_outcome){
    for (version in train_version){
      temp_dt_tune_results <- extract_tune_files_nested(filepath, outcome, version, fold_id)
      
      if (version == "data_hgb_only" && outcome == "predict_hgb"){
        list_dt_tune_results_hgb_only_predict_hgb[[fold_id]] <- temp_dt_tune_results
      } else if (version == "data_hgb_ferr" && outcome == "predict_hgb"){
        list_dt_tune_results_hgb_ferr_predict_hgb[[fold_id]] <- temp_dt_tune_results
      } else if (version == "data_hgb_only" && outcome == "predict_ferr"){
        list_dt_tune_results_hgb_only_predict_ferr[[fold_id]] <- temp_dt_tune_results
      } else if (version == "data_hgb_ferr" && outcome == "predict_ferr"){
        list_dt_tune_results_hgb_ferr_predict_ferr[[fold_id]] <- temp_dt_tune_results
      }
    }
  }
}


range(list_dt_tune_results_hgb_only_predict_hgb[["outer_fold_1"]]$rmspe_mean)
range(list_dt_tune_results_hgb_only_predict_hgb[["outer_fold_2"]]$rmspe_mean)
range(list_dt_tune_results_hgb_only_predict_hgb[["outer_fold_3"]]$rmspe_mean)

list_tune_results_all <- list()
list_tune_results_all_predict_hgb <- list()
list_tune_results_all_predict_ferr <- list()

for (fold_id in outer_fold_id){
  temp_results <- calc_rmspe_stats(list_dt_tune_results_hgb_only_predict_hgb[[fold_id]],
                                   list_dt_tune_results_hgb_only_predict_ferr[[fold_id]],
                                   list_dt_tune_results_hgb_ferr_predict_hgb[[fold_id]],
                                   list_dt_tune_results_hgb_ferr_predict_ferr[[fold_id]])
  temp_results_pred_hgb <- temp_results[predicted_biomarker=="Hemoglobin", ]
  temp_results_pred_ferr <- temp_results[predicted_biomarker=="Ferritin", ]
  
  list_tune_results_all[[fold_id]] <- temp_results
  list_tune_results_all_predict_hgb[[fold_id]] <- temp_results_pred_hgb
  list_tune_results_all_predict_ferr[[fold_id]] <- temp_results_pred_ferr
  
}



# SAVE RESULTS ---- 
tune_results_path <- "./3_intermediate/tune_results/nested_model/"

for (fold_id in outer_fold_id){
  fwrite(list_tune_results_all[[fold_id]], file = paste0(tune_results_path, "rmspe_tune_results_all_", fold_id, ".csv"))
  fwrite(list_tune_results_all_predict_hgb[[fold_id]], file = paste0(tune_results_path, "rmspe_tune_results_all_predict_hgb_", fold_id, ".csv"))
  fwrite(list_tune_results_all_predict_ferr[[fold_id]], file = paste0(tune_results_path, "rmspe_tune_results_all_predict_ferr_", fold_id, ".csv"))
  
}



# PLOT TUNING RESULTS ----

plot_model_hyperparams <- function(dt_tune_results, predict_biomarkers, metric) {
  mod_names <- c("Elastic net", "Random forest", "Gradient boosted trees", "Catboost")
  names(mod_names)<-c("EN", "RF", "XGB", "CB")
  
  metric_upper <- toupper(metric)
  top_mods <- dt_tune_results[, list(top_mean_rmspe = min(rmspe_mean)), by = version]
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
  
}


for (fold_id in outer_fold_id){
  # call plot function
  p1_hgb <- plot_model_hyperparams(list_tune_results_all_predict_hgb[[fold_id]], predict_biomarkers="predict_hgb", metric="RMSPE") 
  p1_ferr <- plot_model_hyperparams(list_tune_results_all_predict_ferr[[fold_id]], predict_biomarkers="predict_ferr", metric="RMSPE") 
  
  # combine plots
  p1_combined <- ggpubr::ggarrange(p1_hgb, p1_ferr, labels = c("A", "B"), ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom") 
  
  ggsave(paste0("./4_output/updates/figs/combined_RMSPE_tuning_no_ensemble_", fold_id, ".png"), plot = p1_combined, width = 10, height = 8, unit = "in")
  ggsave(paste0("./4_output/updates/figs/combined_RMSPE_tuning_no_ensemble_", fold_id, ".svg"), plot = p1_combined, width = 10, height = 8, unit = "in")
  
}



# ENSEMBLE MODEL SELECTION -----
list_ensemble_model <- list()

for (fold_id in outer_fold_id){
  tune_results_all <- list_tune_results_all[[fold_id]]
  
  # put all this in loop to do for each version/prediction task
  setorder(tune_results_all, version, rmspe_mean)
  
  
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
      dt_tune_results_temp <- tune_results_all[version == v &
                                                 predicted_biomarker == p, ]
      setorder(dt_tune_results_temp, version, rmspe_mean)
      
      # Extract lower bound of top model
      top_mod_rmspe <- unlist(dt_tune_results_temp[version == v & predicted_biomarker == p,][, ..cols_rmspe][1])
      
      ## ENSEMBLE 1: Select subset of models not statistically different from top model ----
      # model architecture not considered
      dt_mods_nonsignf <- data.frame(matrix(nrow=0, ncol = ncol(dt_tune_results_temp))) 
      
      for (i in 2:nrow(dt_tune_results_temp)){
        test_mod_rmspe <- unlist(dt_tune_results_temp[, ..cols_rmspe][i])
        nonparam_t_test <- wilcox.test(top_mod_rmspe, test_mod_rmspe, exact = FALSE)
        p_value <- nonparam_t_test$p.value
        if (p_value > 0.05){ # relax threshold of significance from 0.05 to 0.03
          mod_nonsignf <- dt_tune_results_temp[i,]
          dt_mods_nonsignf <- rbind(dt_mods_nonsignf, mod_nonsignf)
        }else{
          dt_mods_nonsignf <- dt_mods_nonsignf
        }
      }
      
      # if the 5th model cannot be found using the threshold for significance at 0.05
      if (nrow(dt_mods_nonsignf) < 5){
        # Initialize the threshold for significance
        threshold <- 0.05
        
        while (nrow(dt_mods_nonsignf) < 5 && threshold > 0) {
          # reset the dataframe
          dt_mods_nonsignf <- data.frame(matrix(nrow=0, ncol = ncol(dt_tune_results_temp))) 
          
          # compare rmspe of the top models with other models and find the ones whose rmspes are not statistically different from the top 
          for (i in 2:nrow(dt_tune_results_temp)){
            test_mod_rmspe <- unlist(dt_tune_results_temp[, ..cols_rmspe][i])
            nonparam_t_test <- wilcox.test(top_mod_rmspe, test_mod_rmspe, exact = FALSE)
            p_value <- nonparam_t_test$p.value
            if (p_value > threshold){ # relax threshold of significance from 0.05 to 0.03
              mod_nonsignf <- dt_tune_results_temp[i,]
              dt_mods_nonsignf <- rbind(dt_mods_nonsignf, mod_nonsignf)
              if (nrow(dt_mods_nonsignf)>=5){
                break
              }
            }
          }
          threshold <- threshold-0.01
        }
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
      # list_ensemble_rmspe[[length(list_ensemble_rmspe)+1]] <- ensemble_mod1_rmspe
      
      
      # ENSEMBLE 2: Top 2 models of top 3 architecture ----
      
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
      # list_ensemble_rmspe[[length(list_ensemble_rmspe)+1]] <- ensemble_mod2_rmspe
    }
  }
  
  list_ensemble_model[[fold_id]] <- dt_ensemble_model
  
}

print(list_ensemble_model)



# EXTRACT ENSEMBLE MODEL CONFIGURATION ----
# base_model_specs() should return a list with mod_name, hyperparameters, data for 5fld 3rpt CV

mod_spec_path <- "./3_intermediate/ensemble/updates/nested_model/"
ensembles <- c("ensemble1", "ensemble2")

## Predict hgb ----
rsplits_path <- "./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/"

### data hgb only ----

for (fold_id in outer_fold_id){
  rsplit_factors_hgb_only <- readRDS(paste0(rsplits_path, "rsplit_factors_hgb_only_", fold_id, ".rds"))
  rsplit_OH_hgb_only <- readRDS(paste0(rsplits_path, "rsplit_OH_hgb_only_", fold_id, ".rds"))
  
  dt_ensemble_model <- list_ensemble_model[[fold_id]]
  setDT(dt_ensemble_model)
  
  # loop over each ensemble and process
  for (ensemble_id in ensembles){
    em_temp <- dt_ensemble_model[ensemble == ensemble_id &
                                   version == "Hemoglobin only" &
                                   predicted_biomarker == "Hemoglobin", ]
    
    em_temp <- unlist(unname(as.list(em_temp[,1:6])))
    base_mod_spec <- list()
    for(mod_id in em_temp){
      base_mod_spec[[mod_id]] <- mod_spec_as_list(mod_id, biomarkers="hgb_only")
    }
    saveRDS(base_mod_spec, paste0(mod_spec_path, ensemble_id, "_base_mod_spec_hgb_only_predict_hgb_", fold_id, ".RDS"))
  }
}



### data hgb ferr ----

for (fold_id in outer_fold_id){
  rsplit_factors_hgb_ferr <- readRDS(paste0(rsplits_path, "rsplit_factors_hgb_ferr_", fold_id, ".rds"))
  rsplit_OH_hgb_ferr <- readRDS(paste0(rsplits_path, "rsplit_OH_hgb_ferr_", fold_id, ".rds"))
  
  dt_ensemble_model <- list_ensemble_model[[fold_id]]
  setDT(dt_ensemble_model)
  
  for (ensemble_id in ensembles){
    em_temp <- dt_ensemble_model[ensemble == ensemble_id &
                                   version == "Hemoglobin and Ferritin" &
                                   predicted_biomarker == "Hemoglobin", ]
    
    em_temp <- unlist(unname(as.list(em_temp[,1:6])))
    base_mod_spec <- list()
    for(mod_id in em_temp){
      base_mod_spec[[mod_id]] <- mod_spec_as_list(mod_id, biomarkers="hgb_ferr")
    }
    saveRDS(base_mod_spec, paste0(mod_spec_path, ensemble_id, "_base_mod_spec_hgb_ferr_predict_hgb_", fold_id, ".RDS"))
  }
}




## Predict ferr ----
rsplits_path <- "./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/"

### data hgb only ----

for (fold_id in outer_fold_id){
  rsplit_factors_hgb_only <- readRDS(paste0(rsplits_path, "rsplit_factors_hgb_only_", fold_id, ".rds"))
  rsplit_OH_hgb_only <- readRDS(paste0(rsplits_path, "rsplit_OH_hgb_only_", fold_id, ".rds"))
  
  dt_ensemble_model <- list_ensemble_model[[fold_id]]
  setDT(dt_ensemble_model)
  
  for (ensemble_id in ensembles){
    em_temp <- dt_ensemble_model[ensemble == ensemble_id &
                                   version == "Hemoglobin only" &
                                   predicted_biomarker == "Ferritin", ]
    
    em_temp <- unlist(unname(as.list(em_temp[,1:6])))
    base_mod_spec <- list()
    for(mod_id in em_temp){
      base_mod_spec[[mod_id]] <- mod_spec_as_list(mod_id, biomarkers="hgb_only")
    }
    saveRDS(base_mod_spec, paste0(mod_spec_path, ensemble_id, "_base_mod_spec_hgb_only_predict_ferr_", fold_id, ".RDS"))
  }
}


### data hgb ferr ----

for (fold_id in outer_fold_id){
  rsplit_factors_hgb_ferr <- readRDS(paste0(rsplits_path, "rsplit_factors_hgb_ferr_", fold_id, ".rds"))
  rsplit_OH_hgb_ferr <- readRDS(paste0(rsplits_path, "rsplit_OH_hgb_ferr_", fold_id, ".rds"))
  
  dt_ensemble_model <- list_ensemble_model[[fold_id]]
  setDT(dt_ensemble_model)
  
  for (ensemble_id in ensembles){
    em_temp <- dt_ensemble_model[ensemble == ensemble_id &
                                   version == "Hemoglobin and Ferritin" &
                                   predicted_biomarker == "Ferritin", ]
    
    em_temp <- unlist(unname(as.list(em_temp[,1:6])))
    base_mod_spec <- list()
    for(mod_id in em_temp){
      base_mod_spec[[mod_id]] <- mod_spec_as_list(mod_id, biomarkers="hgb_ferr")
    }
    saveRDS(base_mod_spec, paste0(mod_spec_path, ensemble_id, "_base_mod_spec_hgb_ferr_predict_ferr_", fold_id, ".RDS"))
  }
}





