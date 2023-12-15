
library(ggplot2)
library(tidyverse)
source('https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R')
theme_set(theme_bw())

# Compare ensemble to individual top models ----

## Predict hgb ----
base_mod_spec_hgb_ferr_predict_hgb <- readRDS("./3_intermediate/ensemble/base_mod_spec_hgb_ferr_predict_hgb.RDS")
base_mod_spec_hgb_only_predict_hgb <- readRDS("./3_intermediate/ensemble/base_mod_spec_hgb_only_predict_hgb.RDS")

ensemble_hgb_ferr_predict_hgb <- fread("./3_intermediate/ensemble/ensemble_assess_results_5XGB1RF_hgb_ferr_predict_hgb_average.csv")
basemod_hgb_ferr_predict_hgb <- fread("./3_intermediate/ensemble/ensemble_basemods_assess_results_5XGB1RF_hgb_ferr_predict_hgb_average.csv")

ensemble_hgb_only_predict_hgb <- fread("./3_intermediate/ensemble/ensemble_assess_results_5XGB1RF_hgb_only_predict_hgb_average.csv")
basemod_hgb_only_predict_hgb <- fread("./3_intermediate/ensemble/ensemble_basemods_assess_results_5XGB1RF_hgb_only_predict_hgb_average.csv")

## Predict ferritin ----
base_mod_spec_hgb_ferr_predict_ferr <- readRDS("./3_intermediate/ensemble/base_mod_spec_hgb_ferr_predict_ferr.RDS")
base_mod_spec_hgb_only_predict_ferr <- readRDS("./3_intermediate/ensemble/base_mod_spec_hgb_only_predict_ferr.RDS")

ensemble_hgb_ferr_predict_ferr <- fread("./3_intermediate/ensemble/ensemble_assess_results_5EN1RF_hgb_ferr_predict_ferr_average.csv")
basemod_hgb_ferr_predict_ferr <- fread("./3_intermediate/ensemble/ensemble_basemods_assess_results_5EN1RF_hgb_ferr_predict_ferr_average.csv")

ensemble_hgb_only_predict_ferr <- fread("./3_intermediate/ensemble/ensemble_assess_results_6RF_hgb_only_predict_ferr_average.csv")
basemod_hgb_only_predict_ferr <- fread("./3_intermediate/ensemble/ensemble_basemods_assess_results_6RF_hgb_only_predict_ferr_average.csv")

##  check results
# EPSILON <- 1e-10
# a<- basemod_hgb_ferr_predict_ferr
# rmspe <- (sqrt(mean(((a$rmspe_repeat1_fold1.fu_outcome - a$rmspe_repeat1_fold1.prediction) / (a$rmspe_repeat1_fold1.fu_outcome + EPSILON))**2))) * 100

tune_results_all<-fread("./3_intermediate/tune_results/main_model/rmspe_tune_results_all_predict_hgb.csv")

top_mods_ensemble_predict_hgb <- rbind(
  tune_results_all[modelID %in% names(base_mod_spec_hgb_ferr_predict_hgb) &  # get names of base models
                     version=="Hemoglobin and Ferritin", .SD,
                   .SDcols=c("version", "modelID", paste0("rmspe_repeat", formatC(ceiling(1:15/5)), "_fold", formatC((1:15-1)%%5 + 1)), "res_mean")],

  cbind(version="Hemoglobin and Ferritin",
        modelID="Ensemble",
        ensemble_hgb_ferr_predict_hgb),

  tune_results_all[modelID %in% names(base_mod_spec_hgb_only_predict_hgb) &
                     version=="Hemoglobin only", .SD,
                   .SDcols=c("version", "modelID", paste0("rmspe_repeat", formatC(ceiling(1:15/5)), "_fold", formatC((1:15-1)%%5 + 1)), "res_mean")],
  cbind(version="Hemoglobin only",
        modelID="Ensemble",
        ensemble_hgb_only_predict_hgb)
)

tune_results_all<-fread("./3_intermediate/tune_results/main_model/rmspe_tune_results_all_predict_ferr.csv")

top_mods_ensemble_predict_ferr <- rbind(
  tune_results_all[modelID %in% names(base_mod_spec_hgb_ferr_predict_ferr) &  # get names of base models
                     version=="Hemoglobin and Ferritin", .SD,
                   .SDcols=c("version", "modelID", paste0("rmspe_repeat", formatC(ceiling(1:15/5)), "_fold", formatC((1:15-1)%%5 + 1)), "res_mean")],

  cbind(version="Hemoglobin and Ferritin",
        modelID="Ensemble",
        ensemble_hgb_ferr_predict_ferr),

  tune_results_all[modelID %in% names(base_mod_spec_hgb_only_predict_ferr) &
                     version=="Hemoglobin only", .SD,
                   .SDcols=c("version", "modelID", paste0("rmspe_repeat", formatC(ceiling(1:15/5)), "_fold", formatC((1:15-1)%%5 + 1)), "res_mean")],
  cbind(version="Hemoglobin only",
        modelID="Ensemble",
        ensemble_hgb_only_predict_ferr)
)

# Plotting top models ----
## Hgb ----
top_mods_ensemble <- top_mods_ensemble_predict_hgb
top_mods_ensemble[, top:=ifelse(res_mean==min(res_mean),"Top",""), by=version]  # label lowest RMSPE as "Top" model

top_mods_ensemble_plt<-melt(top_mods_ensemble[,.SD,.SDcols=!c("res_mean")],
                            id.vars=c("version","modelID","top"))

p1 <- ggplot(top_mods_ensemble_plt, aes(x = modelID, y = value, fill=top,))+
  facet_wrap(vars(version), nrow=2, scales="free_y")+coord_flip()+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8)+
  geom_point(position = position_jitter(width = .15), size = .5, alpha = 0.8,
             aes(color=top))+
  stat_summary(fun = mean, geom = "point",
               position = position_nudge(x = .2, y = 0)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_nudge(x = .2, y = 0))+
  xlab("")+
  scale_y_continuous(name="RMSPE for 15 tuning sets (Mean, standard error, and distribution)", labels = scales::percent_format(scale=1))+
  theme(legend.position = "None")

#ggsave("./4_output/figs/RMSPE_top_models_predict_hgb.svg", width = 6, height = 4.5, unit = "in")
#ggsave("./4_output/figs/RMSPE_top_models_predict_hgb.png", width = 6, height = 4.5, unit = "in")

## Ferritin ----
top_mods_ensemble <- top_mods_ensemble_predict_ferr
top_mods_ensemble[, top:=ifelse(res_mean==min(res_mean),"Top",""), by=version]  # label lowest RMSPE as "Top" model

top_mods_ensemble_plt<-melt(top_mods_ensemble[,.SD,.SDcols=!c("res_mean")],
                            id.vars=c("version","modelID","top"))

p2 <- ggplot(top_mods_ensemble_plt, aes(x = modelID, y = value, fill=top,))+
  facet_wrap(vars(version), nrow=2, scales="free_y")+coord_flip()+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8)+
  geom_point(position = position_jitter(width = .15), size = .5, alpha = 0.8,
             aes(color=top))+
  stat_summary(fun = mean, geom = "point",
               position = position_nudge(x = .2, y = 0)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_nudge(x = .2, y = 0))+
  xlab("")+
  scale_y_continuous(name="RMSPE for 15 tuning sets (Mean, standard error, and distribution)", labels = scales::percent_format(scale=1))+
  theme(legend.position = "None")

#ggsave("./4_output/figs/RMSPE_top_models_predict_ferr.svg", width = 6, height = 4.5, unit = "in")
#ggsave("./4_output/figs/RMSPE_top_models_predict_ferr.png", width = 6, height = 4.5, unit = "in")


ggpubr::ggarrange(p1, p2, labels = c("A", "B"), ncol = 2, nrow = 1) 
ggsave("./4_output/figs/suppfig5_RMSPE_top_models.svg", width = 12, height = 9, unit = "in")

ggsave("./4_output/figs/suppfig5_RMSPE_top_models.png", width = 12, height = 9, unit = "in")
