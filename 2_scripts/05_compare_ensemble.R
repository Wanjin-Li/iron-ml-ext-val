# This script compares ensemble models developed using top-performing models selected from regular cross-validation.


library(ggplot2)
library(tidyverse)
source('https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R')
theme_set(theme_bw())

# COMPARE ENSEMBLE TO INDIVIDUAL MODELS ----
tune_results_all_predict_hgb <-fread("./3_intermediate/tune_results/base_model/rmspe_tune_results_all_predict_hgb.csv")
tune_results_all_predict_ferr <-fread("./3_intermediate/tune_results/base_model/rmspe_tune_results_all_predict_ferr.csv")


## Predict hgb ----
### data hgb only ----
#### ensemble 1 ----
em1_base_mod_spec_hgb_only_predict_hgb <- readRDS("./3_intermediate/ensemble/updates/em1_base_mod_spec_hgb_only_predict_hgb.RDS")
em1_results_hgb_only_predict_hgb <- fread("./3_intermediate/ensemble/updates/ensemble_assess_results_ensemble1_hgb_only_predict_hgb_average.csv")

# Merge base model results with ensemble results
top_mods_ensemble1_hgb_only_predict_hgb <- rbind(
  tune_results_all_predict_hgb[modelID %in% names(em1_base_mod_spec_hgb_only_predict_hgb) &  # get names of base models
                     version=="Hemoglobin only", .SD,
                   .SDcols=c("version", "modelID", 
                             paste0("rmspe_rpt", formatC(ceiling(1:15/5)), "_fold", formatC((1:15-1)%%5 + 1)), "rmspe_mean")],
  cbind(version="Hemoglobin only",
        modelID="Ensemble1",
        em1_results_hgb_only_predict_hgb))

#### ensemble 2 ----
em2_base_mod_spec_hgb_only_predict_hgb <- readRDS("./3_intermediate/ensemble/updates/em2_base_mod_spec_hgb_only_predict_hgb.RDS")
em2_results_hgb_only_predict_hgb <- fread("./3_intermediate/ensemble/updates/ensemble_assess_results_ensemble2_hgb_only_predict_hgb_average.csv")

# Merge base model results with ensemble results
top_mods_ensemble2_hgb_only_predict_hgb <- rbind(
  tune_results_all_predict_hgb[modelID %in% names(em2_base_mod_spec_hgb_only_predict_hgb) &  # get names of base models
                                 version=="Hemoglobin only", .SD,
                               .SDcols=c("version", "modelID", paste0("rmspe_rpt", formatC(ceiling(1:15/5)), "_fold", formatC((1:15-1)%%5 + 1)), "rmspe_mean")],
  
  cbind(version="Hemoglobin only",
        modelID="Ensemble2",
        em2_results_hgb_only_predict_hgb))

### data hgb ferr ----
#### ensemble 1 ----
em1_base_mod_spec_hgb_ferr_predict_hgb <- readRDS("./3_intermediate/ensemble/updates/em1_base_mod_spec_hgb_ferr_predict_hgb.RDS")
em1_results_hgb_ferr_predict_hgb <- fread("./3_intermediate/ensemble/updates/ensemble_assess_results_ensemble1_hgb_ferr_predict_hgb_average.csv")

# Merge base model results with ensemble results
top_mods_ensemble1_hgb_ferr_predict_hgb <- rbind(
  tune_results_all_predict_hgb[modelID %in% names(em1_base_mod_spec_hgb_ferr_predict_hgb) &  # get names of base models
                                 version=="Hemoglobin and Ferritin", .SD,
                               .SDcols=c("version", "modelID", paste0("rmspe_rpt", formatC(ceiling(1:15/5)), "_fold", formatC((1:15-1)%%5 + 1)), "rmspe_mean")],
  
  cbind(version="Hemoglobin and Ferritin",
        modelID="Ensemble1",
        em1_results_hgb_ferr_predict_hgb))

#### ensemble 2 ----
em2_base_mod_spec_hgb_ferr_predict_hgb <- readRDS("./3_intermediate/ensemble/updates/em2_base_mod_spec_hgb_ferr_predict_hgb.RDS")
em2_results_hgb_ferr_predict_hgb <- fread("./3_intermediate/ensemble/updates/ensemble_assess_results_ensemble2_hgb_ferr_predict_hgb_average.csv")


# Merge base model results with ensemble results
top_mods_ensemble2_hgb_ferr_predict_hgb <- rbind(
  tune_results_all_predict_hgb[modelID %in% names(em2_base_mod_spec_hgb_ferr_predict_hgb) &  # get names of base models
                                 version=="Hemoglobin and Ferritin", .SD,
                               .SDcols=c("version", "modelID", paste0("rmspe_rpt", formatC(ceiling(1:15/5)), "_fold", formatC((1:15-1)%%5 + 1)), "rmspe_mean")],
  
  cbind(version="Hemoglobin and Ferritin",
        modelID="Ensemble2",
        em2_results_hgb_ferr_predict_hgb))


## Predict ferr ----
### data hgb only ----
#### ensemble 1 ----
em1_base_mod_spec_hgb_only_predict_ferr <- readRDS("./3_intermediate/ensemble/updates/em1_base_mod_spec_hgb_only_predict_ferr.RDS")
em1_results_hgb_only_predict_ferr <- fread("./3_intermediate/ensemble/updates/ensemble_assess_results_ensemble1_hgb_only_predict_ferr_average.csv")

# Merge base model results with ensemble results
top_mods_ensemble1_hgb_only_predict_ferr <- rbind(
  tune_results_all_predict_ferr[modelID %in% names(em1_base_mod_spec_hgb_only_predict_ferr) &  # get names of base models
                                 version=="Hemoglobin only", .SD,
                               .SDcols=c("version", "modelID", paste0("rmspe_rpt", formatC(ceiling(1:15/5)), "_fold", formatC((1:15-1)%%5 + 1)), "rmspe_mean")],
  
  cbind(version="Hemoglobin only",
        modelID="Ensemble1",
        em1_results_hgb_only_predict_ferr))


#### ensemble 2 ----
em2_base_mod_spec_hgb_only_predict_ferr <- readRDS("./3_intermediate/ensemble/updates/em2_base_mod_spec_hgb_only_predict_ferr.RDS")
em2_results_hgb_only_predict_ferr <- fread("./3_intermediate/ensemble/updates/ensemble_assess_results_ensemble2_hgb_only_predict_ferr_average.csv")

# Merge base model results with ensemble results
top_mods_ensemble2_hgb_only_predict_ferr <- rbind(
  tune_results_all_predict_ferr[modelID %in% names(em2_base_mod_spec_hgb_only_predict_ferr) &  # get names of base models
                                 version=="Hemoglobin only", .SD,
                               .SDcols=c("version", "modelID", paste0("rmspe_rpt", formatC(ceiling(1:15/5)), "_fold", formatC((1:15-1)%%5 + 1)), "rmspe_mean")],
  
  cbind(version="Hemoglobin only",
        modelID="Ensemble2",
        em2_results_hgb_only_predict_ferr))

### data hgb ferr ----
#### ensemble 1 ----
em1_base_mod_spec_hgb_ferr_predict_ferr <- readRDS("./3_intermediate/ensemble/updates/em1_base_mod_spec_hgb_ferr_predict_ferr.RDS")
em1_results_hgb_ferr_predict_ferr <- fread("./3_intermediate/ensemble/updates/ensemble_assess_results_ensemble1_hgb_ferr_predict_ferr_average.csv")

# Merge base model results with ensemble results
top_mods_ensemble1_hgb_ferr_predict_ferr <- rbind(
  tune_results_all_predict_ferr[modelID %in% names(em1_base_mod_spec_hgb_ferr_predict_ferr) &  # get names of base models
                                  version=="Hemoglobin and Ferritin", .SD,
                                .SDcols=c("version", "modelID", paste0("rmspe_rpt", formatC(ceiling(1:15/5)), "_fold", formatC((1:15-1)%%5 + 1)), "rmspe_mean")],
  
  cbind(version="Hemoglobin and Ferritin",
        modelID="Ensemble1",
        em1_results_hgb_ferr_predict_ferr))


#### ensemble 2 ----
em2_base_mod_spec_hgb_ferr_predict_ferr <- readRDS("./3_intermediate/ensemble/updates/em2_base_mod_spec_hgb_ferr_predict_ferr.RDS")
em2_results_hgb_ferr_predict_ferr <- fread("./3_intermediate/ensemble/updates/ensemble_assess_results_ensemble2_hgb_ferr_predict_ferr_average.csv")

# Merge base model results with ensemble results
top_mods_ensemble2_hgb_ferr_predict_ferr <- rbind(
  tune_results_all_predict_ferr[modelID %in% names(em2_base_mod_spec_hgb_ferr_predict_ferr) &  # get names of base models
                                  version=="Hemoglobin and Ferritin", .SD,
                                .SDcols=c("version", "modelID", paste0("rmspe_rpt", formatC(ceiling(1:15/5)), "_fold", formatC((1:15-1)%%5 + 1)), "rmspe_mean")],
  
  cbind(version="Hemoglobin and Ferritin",
        modelID="Ensemble2",
        em2_results_hgb_ferr_predict_ferr))



# PLOTTING TOP MODELS ----

## Predict hgb ----
### data hgb only ----
#### ensemble 1 ----
top_mods_ensemb1_hgb_only_hgb <- top_mods_ensemble1_hgb_only_predict_hgb
top_mods_ensemb1_hgb_only_hgb[, top:=ifelse(rmspe_mean==min(rmspe_mean),"Top",""), by=version]  # label lowest RMSPE as "Top" model

top_mods_ensemb1_hgb_only_hgb_plt <- melt(top_mods_ensemb1_hgb_only_hgb[,.SD,.SDcols=!c("rmspe_mean")],
                            id.vars=c("version","modelID","top"))

#### ensemble 2 ----

top_mods_ensemb2_hgb_only_hgb <- top_mods_ensemble2_hgb_only_predict_hgb
top_mods_ensemb2_hgb_only_hgb[, top:=ifelse(rmspe_mean==min(rmspe_mean),"Top",""), by=version]  # label lowest RMSPE as "Top" model

top_mods_ensemb2_hgb_only_hgb_plt <- melt(top_mods_ensemb2_hgb_only_hgb[,.SD,.SDcols=!c("rmspe_mean")],
                              id.vars=c("version","modelID","top"))

### data hgb ferr ----
#### ensemble 1 ----
top_mods_ensemb1_hgb_ferr_hgb <- top_mods_ensemble1_hgb_ferr_predict_hgb
top_mods_ensemb1_hgb_ferr_hgb[, top:=ifelse(rmspe_mean==min(rmspe_mean),"Top",""), by=version]  # label lowest RMSPE as "Top" model

top_mods_ensemb1_hgb_ferr_hgb_plt <- melt(top_mods_ensemb1_hgb_ferr_hgb[,.SD,.SDcols=!c("rmspe_mean")],
                               id.vars=c("version","modelID","top"))

#### ensemble 2 ----

top_mods_ensemb2_hgb_ferr_hgb <- top_mods_ensemble2_hgb_ferr_predict_hgb
top_mods_ensemb2_hgb_ferr_hgb[, top:=ifelse(rmspe_mean==min(rmspe_mean),"Top",""), by=version]  # label lowest RMSPE as "Top" model

top_mods_ensemb2_hgb_ferr_hgb_plt <- melt(top_mods_ensemb2_hgb_ferr_hgb[,.SD,.SDcols=!c("rmspe_mean")],
                               id.vars=c("version","modelID","top"))

# Plot ----

min_range_y <- min(min(top_mods_ensemb1_hgb_only_hgb_plt$value), min(top_mods_ensemb2_hgb_only_hgb_plt$value), 
                   min(top_mods_ensemb1_hgb_ferr_hgb_plt$value), min(top_mods_ensemb2_hgb_ferr_hgb_plt$value))

max_range_y <- max(max(top_mods_ensemb1_hgb_only_hgb_plt$value), max(top_mods_ensemb2_hgb_only_hgb_plt$value), 
                   max(top_mods_ensemb1_hgb_ferr_hgb_plt$value), max(top_mods_ensemb2_hgb_ferr_hgb_plt$value))


plot_em1_pred_hgb_hgb_only <- ggplot(top_mods_ensemb1_hgb_only_hgb_plt, aes(x = modelID, y = value, fill=top)) +
  facet_wrap(vars(version), nrow=2, scales="free_y") +coord_flip()+
  geom_point(position = position_jitter(width = .15), size = .5, alpha = 0.8,
             aes(color=top))+
  stat_summary(fun = mean, geom = "point",
               position = position_nudge(x = .2, y = 0)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_nudge(x = .2, y = 0))+
  xlab("")+
  scale_y_continuous(name="RMSPE for 15 tuning sets (Mean, standard error, and distribution)", 
                     labels = scales::percent_format(scale=1),
                     limits = c(min_range_y, max_range_y)) +
  theme(axis.title.x = element_blank(),  # Remove x-axis label
        axis.text.x = element_blank(),
        legend.position = "None")


plot_em2_pred_hgb_hgb_only <- ggplot(top_mods_ensemb2_hgb_only_hgb_plt, aes(x = modelID, y = value, fill=top)) +
  facet_wrap(vars(version), nrow=2, scales="free_y") +coord_flip()+
  geom_point(position = position_jitter(width = .15), size = .5, alpha = 0.8,
             aes(color=top))+
  stat_summary(fun = mean, geom = "point",
               position = position_nudge(x = .2, y = 0)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_nudge(x = .2, y = 0))+
  xlab("")+
  scale_y_continuous(name="RMSPE for 15 tuning sets (Mean, standard error, and distribution)", 
                     labels = scales::percent_format(scale=1),
                     limits = c(min_range_y, max_range_y))+
  theme(axis.title.x = element_blank(),  # Remove x-axis label
        axis.text.x = element_blank(),
        legend.position = "None")


plot_em1_pred_hgb_hgb_ferr <- ggplot(top_mods_ensemb1_hgb_ferr_hgb_plt, aes(x = modelID, y = value, fill=top)) +
  facet_wrap(vars(version), nrow=2, scales="free_y") +coord_flip()+
  geom_point(position = position_jitter(width = .15), size = .5, alpha = 0.8,
             aes(color=top))+
  stat_summary(fun = mean, geom = "point",
               position = position_nudge(x = .2, y = 0)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_nudge(x = .2, y = 0))+
  xlab("")+
  scale_y_continuous(name="RMSPE for 15 tuning sets (Mean, standard error, and distribution)", 
                     labels = scales::percent_format(scale=1),
                     limits = c(min_range_y, max_range_y))+
  theme(axis.title.x = element_blank(),  # Remove x-axis label
        axis.text.x = element_blank(), 
        legend.position = "None")


plot_em2_pred_hgb_hgb_ferr <- ggplot(top_mods_ensemb2_hgb_ferr_hgb_plt, aes(x = modelID, y = value, fill=top)) +
  facet_wrap(vars(version), nrow=2, scales="free_y") +coord_flip()+
  geom_point(position = position_jitter(width = .15), size = .5, alpha = 0.8,
             aes(color=top))+
  stat_summary(fun = mean, geom = "point",
               position = position_nudge(x = .2, y = 0)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_nudge(x = .2, y = 0))+
  xlab("")+
  scale_y_continuous(name="RMSPE for 15 tuning sets (Mean, standard error, and distribution)", 
                     labels = scales::percent_format(scale=1),
                     limits = c(min_range_y, max_range_y))+
  theme(legend.position = "None")

# 
# ggpubr::ggarrange(plot_em1_pred_hgb_hgb_only, plot_em2_pred_hgb_hgb_only, 
#                   plot_em1_pred_hgb_hgb_ferr, plot_em2_pred_hgb_hgb_ferr,
#                   labels = c("A"), ncol = 1, nrow = 4) 
# 
# 



## Predict ferr ----
### data hgb only ----
#### ensemble 1 ----
top_mods_ensemb1_hgb_only_ferr <- top_mods_ensemble1_hgb_only_predict_ferr
top_mods_ensemb1_hgb_only_ferr[, top:=ifelse(rmspe_mean==min(rmspe_mean),"Top",""), by=version]  # label lowest RMSPE as "Top" model

top_mods_ensemb1_hgb_only_ferr_plt <- melt(top_mods_ensemb1_hgb_only_ferr[,.SD,.SDcols=!c("rmspe_mean")],
                               id.vars=c("version","modelID","top"))

#### ensemble 2 ----

top_mods_ensemb2_hgb_only_ferr <- top_mods_ensemble2_hgb_only_predict_ferr
top_mods_ensemb2_hgb_only_ferr[, top:=ifelse(rmspe_mean==min(rmspe_mean),"Top",""), by=version]  # label lowest RMSPE as "Top" model

top_mods_ensemb2_hgb_only_ferr_plt <- melt(top_mods_ensemb2_hgb_only_ferr[,.SD,.SDcols=!c("rmspe_mean")],
                               id.vars=c("version","modelID","top"))

### data hgb ferr ----
#### ensemble 1 ----
top_mods_ensemb1_hgb_ferr_ferr <- top_mods_ensemble1_hgb_ferr_predict_ferr
top_mods_ensemb1_hgb_ferr_ferr[, top:=ifelse(rmspe_mean==min(rmspe_mean),"Top",""), by=version]  # label lowest RMSPE as "Top" model

top_mods_ensemb1_hgb_ferr_ferr_plt <- melt(top_mods_ensemb1_hgb_ferr_ferr[,.SD,.SDcols=!c("rmspe_mean")],
                               id.vars=c("version","modelID","top"))

#### ensemble 2 ----

top_mods_ensemb2_hgb_ferr_ferr <- top_mods_ensemble2_hgb_ferr_predict_ferr
top_mods_ensemb2_hgb_ferr_ferr[, top:=ifelse(rmspe_mean==min(rmspe_mean),"Top",""), by=version]  # label lowest RMSPE as "Top" model

top_mods_ensemb2_hgb_ferr_ferr_plt <- melt(top_mods_ensemb2_hgb_ferr_ferr[,.SD,.SDcols=!c("rmspe_mean")],
                               id.vars=c("version","modelID","top"))





# plot

min_range_y <- min(min(top_mods_ensemb1_hgb_only_ferr_plt$value), min(top_mods_ensemb2_hgb_only_ferr_plt$value), 
                   min(top_mods_ensemb1_hgb_ferr_ferr_plt$value), min(top_mods_ensemb2_hgb_ferr_ferr_plt$value))

max_range_y <- max(max(top_mods_ensemb1_hgb_only_ferr_plt$value), max(top_mods_ensemb2_hgb_only_ferr_plt$value), 
                   max(top_mods_ensemb1_hgb_ferr_ferr_plt$value), max(top_mods_ensemb2_hgb_ferr_ferr_plt$value))

plot_em1_pred_ferr_hgb_only <- ggplot(top_mods_ensemb1_hgb_only_ferr_plt, aes(x = modelID, y = value, fill=top,)) +
  facet_wrap(vars(version), nrow=2, scales="free_y") +coord_flip()+
  geom_point(position = position_jitter(width = .15), size = .5, alpha = 0.8,
             aes(color=top))+
  stat_summary(fun = mean, geom = "point",
               position = position_nudge(x = .2, y = 0)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_nudge(x = .2, y = 0))+
  xlab("")+
  scale_y_continuous(name="RMSPE for 15 tuning sets (Mean, standard error, and distribution)", 
                     labels = scales::percent_format(scale=1),
                     limits = c(min_range_y, max_range_y))+
  theme(axis.title.x = element_blank(),  # Remove x-axis label
        axis.text.x = element_blank(), 
        legend.position = "None")


plot_em2_pred_ferr_hgb_only <- ggplot(top_mods_ensemb2_hgb_only_ferr_plt, aes(x = modelID, y = value, fill=top,)) +
  facet_wrap(vars(version), nrow=2, scales="free_y") +coord_flip()+
  geom_point(position = position_jitter(width = .15), size = .5, alpha = 0.8,
             aes(color=top))+
  stat_summary(fun = mean, geom = "point",
               position = position_nudge(x = .2, y = 0)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_nudge(x = .2, y = 0))+
  xlab("")+
  scale_y_continuous(name="RMSPE for 15 tuning sets (Mean, standard error, and distribution)", 
                     labels = scales::percent_format(scale=1),
                     limits = c(min_range_y, max_range_y))+
  theme(axis.title.x = element_blank(),  # Remove x-axis label
        axis.text.x = element_blank(), 
        legend.position = "None")


plot_em1_pred_ferr_hgb_ferr <- ggplot(top_mods_ensemb1_hgb_ferr_ferr_plt, aes(x = modelID, y = value, fill=top,)) +
  facet_wrap(vars(version), nrow=2, scales="free_y") +coord_flip()+
  geom_point(position = position_jitter(width = .15), size = .5, alpha = 0.8,
             aes(color=top))+
  stat_summary(fun = mean, geom = "point",
               position = position_nudge(x = .2, y = 0)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_nudge(x = .2, y = 0))+
  xlab("")+
  scale_y_continuous(name="RMSPE for 15 tuning sets (Mean, standard error, and distribution)", 
                     labels = scales::percent_format(scale=1),
                     limits = c(min_range_y, max_range_y))+
  theme(axis.title.x = element_blank(),  # Remove x-axis label
        axis.text.x = element_blank(), 
        legend.position = "None")


plot_em2_pred_ferr_hgb_ferr <- ggplot(top_mods_ensemb2_hgb_ferr_ferr_plt, aes(x = modelID, y = value, fill=top,)) +
  facet_wrap(vars(version), nrow=2, scales="free_y") +coord_flip()+
  geom_point(position = position_jitter(width = .15), size = .5, alpha = 0.8,
             aes(color=top))+
  stat_summary(fun = mean, geom = "point",
               position = position_nudge(x = .2, y = 0)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_nudge(x = .2, y = 0))+
  xlab("")+
  scale_y_continuous(name="RMSPE for 15 tuning sets (Mean, standard error, and distribution)", 
                     labels = scales::percent_format(scale=1),
                     limits = c(min_range_y, max_range_y))+
  theme(legend.position = "None")


# Combine plots ----
p2_combined <- ggpubr::ggarrange(plot_em1_pred_hgb_hgb_only, plot_em1_pred_ferr_hgb_only,
                                 plot_em2_pred_hgb_hgb_only, plot_em2_pred_ferr_hgb_only,
                                 plot_em1_pred_hgb_hgb_ferr, plot_em1_pred_ferr_hgb_ferr,
                                 plot_em2_pred_hgb_hgb_ferr, plot_em2_pred_ferr_hgb_ferr,
                                 labels = c("A", "B"), ncol = 2, nrow = 4) 
p2_combined
ggsave("./4_output/updates/figs/combined_RMSPE_tuning_ensemble.png", plot = p2_combined, width = 12, height = 8, unit = "in")


# Extract rmspe of the top models ----

# predict hgb + hgb only: ensemble 1
fwrite(top_mods_ensemble1_hgb_only_predict_hgb, "./3_intermediate/ensemble/updates/top_mods_assess_ensemble1_hgb_only_predict_hgb_full.csv")

# predict hgb + hgb ferr: ensemble 1
fwrite(top_mods_ensemble1_hgb_ferr_predict_hgb, "./3_intermediate/ensemble/updates/top_mods_assess_ensemble1_hgb_ferr_predict_hgb_full.csv")

# predict ferr + hgb only: CB.1171
fwrite(top_mods_ensemble1_hgb_only_predict_ferr, "./3_intermediate/ensemble/updates/top_mods_assess_CB.1171_hgb_only_predict_ferr_full.csv")

# predict ferr + hgb ferr: CB.1222
fwrite(top_mods_ensemble1_hgb_ferr_predict_ferr, "./3_intermediate/ensemble/updates/top_mods_assess_CB.1222_hgb_ferr_predict_ferr_full.csv")


