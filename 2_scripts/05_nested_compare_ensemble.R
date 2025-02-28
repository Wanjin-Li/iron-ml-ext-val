# This script compares ensemble models developed using top-performing models selected from nested cross-validation.

library(ggplot2)
library(tidyverse)
source('https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R')
theme_set(theme_bw())

# COMPARE ENSEMBLE TO INDIVIDUAL MODELS ----
tune_results_path <- "./3_intermediate/tune_results/nested_model/"
ensemble_path <- "./3_intermediate/ensemble/updates/nested_model/"

top_mods_ensemble <- list()
ensemble_name <- c("ensemble1", "ensemble2")
predict_outcome <- c("predict_hgb", "predict_ferr")
data_version <- c("hgb_only", "hgb_ferr")
outer_fold_id <- c("outer_fold_1", "outer_fold_2", "outer_fold_3")

list_top_mods_ensemble <- list()

for (fold_id in outer_fold_id){
  tune_results_all_predict_hgb <- fread(paste0(tune_results_path, "rmspe_tune_results_all_predict_hgb_", fold_id, ".csv"))
  tune_results_all_predict_ferr <- fread(paste0(tune_results_path, "rmspe_tune_results_all_predict_ferr_", fold_id, ".csv"))
  
  for (outcome_id in predict_outcome){
    for (version_id in data_version){
      for (en_id in ensemble_name) {
        base_mod_spec <- readRDS(paste0(ensemble_path, en_id, "_base_mod_spec_", version_id, "_", outcome_id, "_", fold_id, ".RDS"))
        ensemble_assess_results <- fread(paste0(ensemble_path, "ensemble_assess_results_", en_id, "_", version_id, "_", outcome_id, "_", fold_id, "_average.csv"))
        tbl_name <- paste0(en_id, "_", version_id, "_", outcome_id)
        
        if (outcome_id == "predict_hgb"){
          modified_en_id <-  paste0(toupper(substr(en_id, 1, 1)), 
                                    substr(en_id, 2, nchar(en_id)))
          if (version_id == "hgb_only"){
            modified_version <- "Hemoglobin only"
          } else if (version_id == "hgb_ferr"){
            modified_version <- "Hemoglobin and Ferritin"
          }
          
          results <- rbind(
            tune_results_all_predict_hgb[modelID %in% names(base_mod_spec) &  # get names of base models
                                           version==modified_version, .SD,
                                         .SDcols=c("version", "modelID", 
                                                   paste0("rmspe_rpt", formatC(ceiling(1:15/5)), "_fold", formatC((1:15-1)%%5 + 1)), "rmspe_mean")],
            cbind(version=modified_version,
                  modelID=modified_en_id,
                  ensemble_assess_results))
          
          top_mods_ensemble[[tbl_name]] <- results
          
        } else if (outcome_id == "predict_ferr"){
          modified_en_id <-  paste0(toupper(substr(en_id, 1, 1)), 
                                    substr(en_id, 2, nchar(en_id)))
          if (version_id == "hgb_only"){
            modified_version <- "Hemoglobin only"
          } else if (version_id == "hgb_ferr"){
            modified_version <- "Hemoglobin and Ferritin"
          }
          
          results <- rbind(
            tune_results_all_predict_ferr[modelID %in% names(base_mod_spec) &  # get names of base models
                                            version==modified_version, .SD,
                                          .SDcols=c("version", "modelID", 
                                                    paste0("rmspe_rpt", formatC(ceiling(1:15/5)), "_fold", formatC((1:15-1)%%5 + 1)), "rmspe_mean")],
            cbind(version=modified_version,
                  modelID=modified_en_id,
                  ensemble_assess_results))
          
          top_mods_ensemble[[tbl_name]] <- results
        }
        
        list_top_mods_ensemble[[fold_id]] <- top_mods_ensemble
      }
    }
  }
}

# helper function to find the top-performing model
label_top_mod <- function(dt_top_modes_ensemble){
  top_models_ens <- dt_top_modes_ensemble[, top:=ifelse(rmspe_mean==min(rmspe_mean), "Top", ""), by=version]
  plt_top_models_ens <- melt(top_models_ens[,.SD,.SDcols=!c("rmspe_mean")],
                             id.vars=c("version","modelID","top"))
  return(plt_top_models_ens)
}

# convert table from wide to long format
context <- c("ensemble1_hgb_only_predict_hgb",
             "ensemble2_hgb_only_predict_hgb",
             "ensemble1_hgb_ferr_predict_hgb",
             "ensemble2_hgb_ferr_predict_hgb",
             "ensemble1_hgb_only_predict_ferr",
             "ensemble2_hgb_only_predict_ferr",
             "ensemble1_hgb_ferr_predict_ferr",
             "ensemble2_hgb_ferr_predict_ferr")

list_dt_top_mods_ensemble_predict_hgb <- list()
list_dt_top_mods_ensemble_predict_ferr <- list()


for (fold_id in outer_fold_id){
  top_mods_ensemble <- list_top_mods_ensemble[[fold_id]]
    
  label_top_models_ensemble_list <- lapply(top_mods_ensemble, label_top_mod)
    
  # unlist
  dt_label_top_models_ensemble <- do.call(rbind, label_top_models_ensemble_list)
  dt_label_top_models_ensemble[, predict_outcome := NA_character_]
  dt_label_top_models_ensemble[1:420, predict_outcome := "Hemoglobin"]
  dt_label_top_models_ensemble[421:840, predict_outcome := "Ferritin"]
    
  # predict hemoglobin
  dt_top_mods_ensemble_predict_hgb <- dt_label_top_models_ensemble[predict_outcome == "Hemoglobin"]
  dt_top_mods_ensemble_predict_hgb[, ensemble := NA_character_]
  dt_top_mods_ensemble_predict_hgb[1:105, ensemble := "Ensemble1"]
  dt_top_mods_ensemble_predict_hgb[106:210, ensemble := "Ensemble2"]
  dt_top_mods_ensemble_predict_hgb[211:315, ensemble := "Ensemble1"]
  dt_top_mods_ensemble_predict_hgb[316:420, ensemble := "Ensemble2"]
  
  dt_top_mods_ensemble_predict_hgb[, facet_var := interaction(dt_top_mods_ensemble_predict_hgb$version, dt_top_mods_ensemble_predict_hgb$ensemble)]
  dt_top_mods_ensemble_predict_hgb[, facet_var := factor(dt_top_mods_ensemble_predict_hgb$facet_var, 
                                                         levels = c(
                                                           "Hemoglobin only.Ensemble1",
                                                           "Hemoglobin only.Ensemble2",
                                                           "Hemoglobin and Ferritin.Ensemble1",
                                                           "Hemoglobin and Ferritin.Ensemble2"
                                                         ))]
  
  # predict ferritin
  dt_top_mods_ensemble_predict_ferr <- dt_label_top_models_ensemble[predict_outcome == "Ferritin"]
  dt_top_mods_ensemble_predict_ferr[, ensemble := NA_character_]
  dt_top_mods_ensemble_predict_ferr[1:105, ensemble := "Ensemble1"]
  dt_top_mods_ensemble_predict_ferr[106:210, ensemble := "Ensemble2"]
  dt_top_mods_ensemble_predict_ferr[211:315, ensemble := "Ensemble1"]
  dt_top_mods_ensemble_predict_ferr[316:420, ensemble := "Ensemble2"]
  
  dt_top_mods_ensemble_predict_ferr[, facet_var := interaction(dt_top_mods_ensemble_predict_ferr$version, dt_top_mods_ensemble_predict_ferr$ensemble)]
  dt_top_mods_ensemble_predict_ferr[, facet_var := factor(dt_top_mods_ensemble_predict_ferr$facet_var, 
                                                          levels = c(
                                                            "Hemoglobin only.Ensemble1",
                                                            "Hemoglobin only.Ensemble2",
                                                            "Hemoglobin and Ferritin.Ensemble1",
                                                            "Hemoglobin and Ferritin.Ensemble2"
                                                          ))]
  
  # save result tables to the list
  list_dt_top_mods_ensemble_predict_hgb[[fold_id]] <- dt_top_mods_ensemble_predict_hgb
  list_dt_top_mods_ensemble_predict_ferr[[fold_id]] <- dt_top_mods_ensemble_predict_ferr
  
}



# PLOT ----
for (fold_id in outer_fold_id){
  plot_predict_hgb <- ggplot(list_dt_top_mods_ensemble_predict_hgb[[fold_id]], aes(x = modelID, y = value, fill=top)) +
    facet_wrap(~ facet_var, nrow = 4, scales="free_y") +coord_flip()+
    geom_point(position = position_jitter(width = .15), size = .5, alpha = 0.8,
               aes(color=top))+
    stat_summary(fun = mean, geom = "point",
                 position = position_nudge(x = .2, y = 0)) +
    stat_summary(fun.data = mean_se, geom = "errorbar",
                 position = position_nudge(x = .2, y = 0))+
    xlab("")+
    scale_y_continuous(name="RMSPE for 15 tuning sets (Mean, standard error, and distribution)", 
                       labels = scales::percent_format(scale=1)) +
    theme(legend.position = "None")
  
  plot_predict_ferr <- ggplot(list_dt_top_mods_ensemble_predict_ferr[[fold_id]], aes(x = modelID, y = value, fill=top)) +
    facet_wrap(~ facet_var, nrow = 4, scales="free_y") +coord_flip()+
    geom_point(position = position_jitter(width = .15), size = .5, alpha = 0.8,
               aes(color=top))+
    stat_summary(fun = mean, geom = "point",
                 position = position_nudge(x = .2, y = 0)) +
    stat_summary(fun.data = mean_se, geom = "errorbar",
                 position = position_nudge(x = .2, y = 0))+
    xlab("")+
    scale_y_continuous(name="RMSPE for 15 tuning sets (Mean, standard error, and distribution)", 
                       labels = scales::percent_format(scale=1)) +
    theme(legend.position = "None")
  
  combined_plot <- ggpubr::ggarrange(plot_predict_hgb, plot_predict_ferr,
                                     labels = c("A", "B"), ncol = 2, nrow = 1) 
  
  # save plot
  ggsave(paste0("./4_output/updates/figs/combined_RMSPE_tuning_ensemble_", fold_id, ".png"), plot = combined_plot, width = 12, height = 8, unit = "in")
  
}



# EXTRACT RMSPE OF THE TOP-PERFORMING MODEL ----
top_mods_path <- "./3_intermediate/ensemble/updates/nested_model/"

# predict hgb + hgb only

## __________OUTER FOLD 1____________
list_top_mods_ensemble$outer_fold_1$ensemble1_hgb_only_predict_hgb[top=="Top"]
list_top_mods_ensemble$outer_fold_1$ensemble2_hgb_only_predict_hgb[top=="Top"]

top_mods_ensemble_hgb_only_pred_hgb_outer_fold_1 <- list_top_mods_ensemble$outer_fold_1$ensemble2_hgb_only_predict_hgb
fwrite(top_mods_ensemble_hgb_only_pred_hgb_outer_fold_1, paste0(top_mods_path, "top_mods_assess_ensemble2_hgb_only_predict_hgb_outer_fold_1.csv"))

##___________________________________

## __________OUTER FOLD 2_____________
list_top_mods_ensemble$outer_fold_2$ensemble1_hgb_only_predict_hgb[top=="Top"]
list_top_mods_ensemble$outer_fold_2$ensemble2_hgb_only_predict_hgb[top=="Top"]

top_mods_ensemble_hgb_only_pred_hgb_outer_fold_2 <- list_top_mods_ensemble$outer_fold_2$ensemble1_hgb_only_predict_hgb
fwrite(top_mods_ensemble_hgb_only_pred_hgb_outer_fold_2, paste0(top_mods_path, "top_mods_assess_XGB.4771_hgb_only_predict_hgb_outer_fold_2.csv"))

##___________________________________

## __________OUTER FOLD 3_____________
list_top_mods_ensemble$outer_fold_3$ensemble1_hgb_only_predict_hgb[top=="Top"]
list_top_mods_ensemble$outer_fold_3$ensemble2_hgb_only_predict_hgb[top=="Top"]

top_mods_ensemble_hgb_only_pred_hgb_outer_fold_3 <- list_top_mods_ensemble$outer_fold_3$ensemble1_hgb_only_predict_hgb
fwrite(top_mods_ensemble_hgb_only_pred_hgb_outer_fold_3, paste0(top_mods_path, "top_mods_assess_CB.171_hgb_only_predict_hgb_outer_fold_3.csv"))

##___________________________________


# predict hgb + hgb ferr

## __________OUTER FOLD 1____________
list_top_mods_ensemble$outer_fold_1$ensemble1_hgb_ferr_predict_hgb[top=="Top"]
list_top_mods_ensemble$outer_fold_1$ensemble2_hgb_ferr_predict_hgb[top=="Top"]

top_mods_ensemble_hgb_ferr_pred_hgb_outer_fold_1 <- list_top_mods_ensemble$outer_fold_1$ensemble1_hgb_ferr_predict_hgb
fwrite(top_mods_ensemble_hgb_ferr_pred_hgb_outer_fold_1, paste0(top_mods_path, "top_mods_assess_XGB.4308_hgb_ferr_predict_hgb_outer_fold_1.csv"))

##___________________________________

## __________OUTER FOLD 2_____________
list_top_mods_ensemble$outer_fold_2$ensemble1_hgb_ferr_predict_hgb[top=="Top"]
list_top_mods_ensemble$outer_fold_2$ensemble2_hgb_ferr_predict_hgb[top=="Top"]

top_mods_ensemble_hgb_ferr_pred_hgb_outer_fold_2 <- list_top_mods_ensemble$outer_fold_2$ensemble1_hgb_ferr_predict_hgb
fwrite(top_mods_ensemble_hgb_ferr_pred_hgb_outer_fold_2, paste0(top_mods_path, "top_mods_assess_ensemble1_hgb_ferr_predict_hgb_outer_fold_2.csv"))

##___________________________________

## __________OUTER FOLD 3_____________
list_top_mods_ensemble$outer_fold_3$ensemble1_hgb_ferr_predict_hgb[top=="Top"]
list_top_mods_ensemble$outer_fold_3$ensemble2_hgb_ferr_predict_hgb[top=="Top"]

top_mods_ensemble_hgb_ferr_pred_hgb_outer_fold_3 <- list_top_mods_ensemble$outer_fold_3$ensemble1_hgb_ferr_predict_hgb
fwrite(top_mods_ensemble_hgb_ferr_pred_hgb_outer_fold_3, paste0(top_mods_path, "top_mods_assess_XGB.4791_hgb_ferr_predict_hgb_outer_fold_3.csv"))

##___________________________________

# predict ferr + hgb only

## __________OUTER FOLD 1____________
list_top_mods_ensemble$outer_fold_1$ensemble1_hgb_only_predict_ferr[top=="Top"]
list_top_mods_ensemble$outer_fold_1$ensemble2_hgb_only_predict_ferr[top=="Top"]

top_mods_ensemble_hgb_only_pred_ferr_outer_fold_1 <- list_top_mods_ensemble$outer_fold_1$ensemble1_hgb_only_predict_ferr
fwrite(top_mods_ensemble_hgb_only_pred_ferr_outer_fold_1, paste0(top_mods_path, "top_mods_assess_EN.1_hgb_only_predict_ferr_outer_fold_1.csv"))

##___________________________________

## __________OUTER FOLD 2_____________
list_top_mods_ensemble$outer_fold_2$ensemble1_hgb_only_predict_ferr[top=="Top"]
list_top_mods_ensemble$outer_fold_2$ensemble2_hgb_only_predict_ferr[top=="Top"]

top_mods_ensemble_hgb_only_pred_ferr_outer_fold_2 <- list_top_mods_ensemble$outer_fold_2$ensemble1_hgb_only_predict_ferr
fwrite(top_mods_ensemble_hgb_only_pred_ferr_outer_fold_2, paste0(top_mods_path, "top_mods_assess_CB.507_hgb_only_predict_ferr_outer_fold_2.csv"))

##___________________________________

## __________OUTER FOLD 3_____________
list_top_mods_ensemble$outer_fold_3$ensemble1_hgb_only_predict_ferr[top=="Top"]
list_top_mods_ensemble$outer_fold_3$ensemble2_hgb_only_predict_ferr[top=="Top"]

top_mods_ensemble_hgb_only_pred_ferr_outer_fold_3 <- list_top_mods_ensemble$outer_fold_3$ensemble2_hgb_only_predict_ferr
fwrite(top_mods_ensemble_hgb_only_pred_ferr_outer_fold_3, paste0(top_mods_path, "top_mods_assess_ensemble2_hgb_only_predict_ferr_outer_fold_3.csv"))

##___________________________________


# predict ferr + hgb ferr

## __________OUTER FOLD 1____________
list_top_mods_ensemble$outer_fold_1$ensemble1_hgb_ferr_predict_ferr[top=="Top"]
list_top_mods_ensemble$outer_fold_1$ensemble2_hgb_ferr_predict_ferr[top=="Top"]

top_mods_ensemble_hgb_ferr_pred_ferr_outer_fold_1 <- list_top_mods_ensemble$outer_fold_1$ensemble1_hgb_ferr_predict_ferr
fwrite(top_mods_ensemble_hgb_ferr_pred_ferr_outer_fold_1, paste0(top_mods_path, "top_mods_assess_CB.801_hgb_ferr_predict_ferr_outer_fold_1.csv"))

##___________________________________

## __________OUTER FOLD 2_____________
list_top_mods_ensemble$outer_fold_2$ensemble1_hgb_ferr_predict_ferr[top=="Top"]
list_top_mods_ensemble$outer_fold_2$ensemble2_hgb_ferr_predict_ferr[top=="Top"]

top_mods_ensemble_hgb_ferr_pred_ferr_outer_fold_2 <- list_top_mods_ensemble$outer_fold_2$ensemble1_hgb_ferr_predict_ferr
fwrite(top_mods_ensemble_hgb_ferr_pred_ferr_outer_fold_2, paste0(top_mods_path, "top_mods_assess_ensemble1_hgb_ferr_predict_ferr_outer_fold_2.csv"))

##___________________________________

## __________OUTER FOLD 3_____________
list_top_mods_ensemble$outer_fold_3$ensemble1_hgb_ferr_predict_ferr[top=="Top"]
list_top_mods_ensemble$outer_fold_3$ensemble2_hgb_ferr_predict_ferr[top=="Top"]

top_mods_ensemble_hgb_ferr_pred_ferr_outer_fold_3 <- list_top_mods_ensemble$outer_fold_3$ensemble1_hgb_ferr_predict_ferr
fwrite(top_mods_ensemble_hgb_ferr_pred_ferr_outer_fold_3, paste0(top_mods_path, "top_mods_assess_CB.1168_hgb_ferr_predict_ferr_outer_fold_3.csv"))
##___________________________________




