library(data.table)
library(pROC)
library(ggplot2)
library(scales)
library(dplyr)
source("2_scripts/utility_functions.R")
library(gridExtra)
theme_set(theme_bw())

intermediate_directory <- './3_intermediate/feature_importance/main_model/'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory, recursive = TRUE)
}


args = commandArgs(trailingOnly=TRUE)  # for taking in inputs
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}

i <- as.integer(args[1])  # input from bash script

## Load top model specs ----
### Predict hgb ----
# ensemble models
base_mod_spec_hgb_ferr_predict_hgb <- readRDS("./3_intermediate/ensemble/base_mod_spec_hgb_ferr_predict_hgb.RDS")
dt_hf_h <- fread("./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_factors_hgb_ferr.csv")

base_mod_spec_hgb_only_predict_hgb <- readRDS("./3_intermediate/ensemble/base_mod_spec_hgb_only_predict_hgb.RDS")
dt_h_h <- fread("./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_factors_hgb_only.csv")

### Predict ferritin ----
# single models
base_mod_spec_hgb_ferr_predict_ferr <- readRDS("./3_intermediate/ensemble/base_mod_spec_hgb_ferr_predict_ferr.RDS")
base_mod_spec_hgb_only_predict_ferr <- readRDS("./3_intermediate/ensemble/base_mod_spec_hgb_only_predict_ferr.RDS")

base_mod_spec_hgb_ferr_predict_ferr <- list("EN.1" = base_mod_spec_hgb_ferr_predict_ferr$EN.1)  # select the single model
base_mod_spec_hgb_only_predict_ferr <- list("RF.125" = base_mod_spec_hgb_only_predict_ferr$RF.125)

dt_hf_f <- fread("./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_factors_hgb_ferr.csv")
dt_h_f <- fread("./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_factors_hgb_only.csv")




# Predict hgb top models
# hgb_ferr: Ensemble (6 base models)
# hgb only: Ensemble (6 base models)

# Predict ferritin top model
# hgb ferr: EN.1
# hgb only: RF.125

# add specs to a list
list_of_specs <- list(base_mod_spec_hgb_ferr_predict_hgb, 
                      base_mod_spec_hgb_only_predict_hgb,
                      base_mod_spec_hgb_ferr_predict_ferr,  # single model
                      base_mod_spec_hgb_only_predict_ferr)  # single model

list_of_dts <- list(dt_hf_h, dt_h_h, dt_hf_f, dt_h_f)

list_of_configs <- c("5XGB1RF", "5XGB1RF", "1EN", "1RF")

list_of_versions <- c("hgb_ferr_predict_hgb", 
                      "hgb_only_predict_hgb",
                      "hgb_ferr_predict_ferr",
                      "hgb_only_predict_ferr")

i <- 1
print(list_of_specs[[i]])
print(list_of_dts[[i]])
print(list_of_configs[i])
print(list_of_versions[i])

ensemble_feature_importance(base_model_specs=list_of_specs[[i]],
                            dt=list_of_dts[[i]],
                            path = "./3_intermediate/feature_importance/main_model/",
                            configs=list_of_configs[i],
                            version=list_of_versions[[i]]) 











# PROCESS DATA

featimp_hf_h <- fread("./3_intermediate/feature_importance/main_model/5XGB1RF_hgb_ferr_predict_hgb.csv")
featimp_h_h <- fread("./3_intermediate/feature_importance/main_model/5XGB1RF_hgb_only_predict_hgb.csv")
featimp_hf_f <- fread("./3_intermediate/feature_importance/main_model/1EN_hgb_ferr_predict_ferr.csv")
featimp_h_f <- fread("./3_intermediate/feature_importance/main_model/1RF_hgb_only_predict_ferr.csv")

# featname_lookup <- fread("./1_data/feature_name_lookup.csv")





## featimp_hf_h ----

plot_feature_importance <- function(df, path_to_save) {
  
  baselines <- df[feature == "baseline"]
  
  colnames(baselines)[4] <- paste0("bl_",colnames(baselines)[4])
  df <-df[feature != "baseline"]
  
  setDT(df)[baselines,
            RMSPE_percentage_change := (res - bl_res),
            on=.(rpt, fold)]
  
  df <- df %>%
    mutate(
      feature = case_when(
        feature == "rbc_loss_last_12_months" ~ "RBC loss last 12 months",
        feature == "rbc_loss_last_24_months" ~ "RBC loss last 24 months",
        feature == "days_since_last_rbc_loss" ~ "Days since last RBC loss",
        feature == "days_since_last_drbc_loss" ~ "Days since last DRBC loss",
        feature == "cum_lifetime_donations" ~ "Cumulative lifetime donations",
        feature == "index_hgb" ~ "Index HGB",
        feature == "blood_type" ~ "Blood type",
        feature == "index_ferritin" ~ "Index ferritin",
        feature == "index_log_ferritin" ~ "Index log ferritin",
        feature == "time_to_fu" ~ "Time to follow up",
        feature == "sex" ~ "Sex",
        feature == "age" ~ "Age",
        TRUE ~ feature  # keep other elements unchanged
      )
    )
  
  
  
  ggplot(df)+
    geom_boxplot(aes(x=reorder(feature, RMSPE_percentage_change, FUN = median), y=RMSPE_percentage_change))+
    coord_flip()+geom_hline(yintercept=0, color="red")+
    scale_y_continuous(name = "Change in RMSPE")+  #  (RMSPE - baseline RMPSE)
    xlab("")
  
  # ggsave(path_to_save,
  #        width = 5, height = 6, units = "in")
  
  fig <- ggplot(df)+
    geom_boxplot(aes(x=reorder(feature, RMSPE_percentage_change, FUN = median), y=RMSPE_percentage_change))+
    coord_flip()+geom_hline(yintercept=0, color="red")+
    scale_y_continuous(name = "Change in RMSPE")+  #  (RMSPE - baseline RMPSE)
    xlab("")
  
  return(fig)
}

fig_a <- plot_feature_importance(df=featimp_hf_h, path_to_save="./4_output/figs/fig2a_feature_importance_hgb_ferr_pred_hgb.svg")
fig_b <- plot_feature_importance(df=featimp_h_h, path_to_save="./4_output/figs/fig2b_feature_importance_hgb_only_pred_hgb.svg")
fig_c <- plot_feature_importance(df=featimp_hf_f, path_to_save="./4_output/figs/fig2c_feature_importance_hgb_ferr_pred_ferr.svg")
fig_d <- plot_feature_importance(df=featimp_h_f, path_to_save="./4_output/figs/fig2d_feature_importance_hgb_only_pred_ferr.svg")


combined_plot <- grid.arrange(fig_a, fig_b, fig_c, fig_d,
                              ncol = 2, nrow = 2)

ggsave(plot = combined_plot, "./4_output/figs/fig2_all.svg", width = 8, height = 6, units = "in")



# ## Combined fig ----
featimp_all <- rbind(
  cbind(mod = "Hemoglobin and Ferritin predict hemoglobin", featimp_hf_h),
  cbind(mod = "Hemoglobin and Ferritin predict ferritin", featimp_hf_f),
  cbind(mod = "Hemoglobin only predict hemoglobin", featimp_h_h),
  cbind(mod = "Hemoglobin only predict ferritin", featimp_h_f)
)

baselines <- featimp_all[feature == "baseline"]

colnames(baselines)[5] <- paste0("bl_",colnames(baselines)[5])
featimp_all <-featimp_all[feature != "baseline"]

setDT(featimp_all)[baselines,
                   RMSPE_percentage_change := (res - bl_res),
                   on=.(mod, rpt, fold)]



ggplot(featimp_all)+
  geom_boxplot(aes(x=reorder(feature, RMSPE_percentage_change, FUN = median), y=RMSPE_percentage_change))+
  facet_grid(cols = vars(mod))+
  coord_flip()+geom_hline(yintercept=0, color="red")+
  scale_y_continuous(name = "Change in RMSPE")+
  xlab("")

ggsave("./4_output/figs/feat_imp_all.svg",
       width = 5, height = 4, units = "in")

