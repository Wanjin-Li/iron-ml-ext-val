library(ggplot2)
library(tidyverse)
source('https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R')
theme_set(theme_bw())

source("./2_scripts/utility_functions.R")


#TOP OVERALL MODEL ----
hf_h <- fread("./3_intermediate/ensemble/top_model_assess_5XGB1RF_hgb_ferr_predict_hgb_average.csv")
h_h <- fread("./3_intermediate/ensemble/top_model_assess_5XGB1RF_hgb_only_predict_hgb_average.csv")

hf_f <- fread("./3_intermediate/ensemble/top_model_assess_1EN_hgb_ferr_predict_ferr_average.csv")
h_f <- fread("./3_intermediate/ensemble/top_model_assess_1RF_hgb_only_predict_ferr_average.csv")

# remove donor_idx col that was added for ensemble models
hf_h <- hf_h[, -c("donor_idx")]
h_h <- h_h[, -c("donor_idx")]

## get RMSPE over each repeat_fold then average the 15 RMSPE values ----
get_rmspe_across_folds <- function(df) {
  sum_rmspe <- 0
  
  EPSILON <- 1e-10
  for (one_repeat in 1:3) {
    for (one_fold in 1:5) {
      dt_inner_fold_preds <- df[df$rpt == one_repeat & df$fold == one_fold ]
      rmspe <- (sqrt(mean(((dt_inner_fold_preds$fu_outcome - dt_inner_fold_preds$prediction) / (dt_inner_fold_preds$fu_outcome + EPSILON))**2))) * 100
      sum_rmspe <- sum_rmspe + rmspe
    }
  }
  avg_rmspe <- sum_rmspe / 15
  return(avg_rmspe)
}

hf_h_rmspe <- get_rmspe_across_folds(hf_h)
h_h_rmspe <- get_rmspe_across_folds(h_h)
hf_f_rmspe <- get_rmspe_across_folds(hf_f)
h_f_rmspe <- get_rmspe_across_folds(h_f)


# ## Bind results from all models into dataframe ----
# dt_outer_preds_all_repeats <- rbind(
#   
#   cbind(task="Predict Hemoglobin", rbind(
#   cbind(version="Hemoglobin and Ferritin", hf_h), cbind(version="Hemoglobin only", h_h))),
#   
#   cbind(task="Predict Ferritin", rbind(
#     cbind(version="Hemoglobin and Ferritin", hf_f), cbind(version="Hemoglobin only", h_f)))
# )




# get result for sanbs, vitalant
# plot bar graph fig 1
