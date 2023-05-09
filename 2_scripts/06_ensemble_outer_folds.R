library(ggplot2)
library(tidyverse)
source('https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R')
theme_set(theme_bw())

source("./2_scripts/utility_functions.R")

# ASSESS TOP MODELS (hgb+ferr and hgb only) on outer folds ----

intermediate_directory <- './3_intermediate/ensemble'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}

args = commandArgs(trailingOnly=TRUE)  # for taking in inputs
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}

i <- as.integer(args[1])  # input from bash script

## Load top model specs ----

### Predict hgb ----
base_mod_spec_hgb_ferr_predict_hgb <- readRDS("./3_intermediate/ensemble/base_mod_spec_hgb_ferr_predict_hgb.RDS")
base_mod_spec_hgb_only_predict_hgb <- readRDS("./3_intermediate/ensemble/base_mod_spec_hgb_only_predict_hgb.RDS")

### Predict ferritin ----
base_mod_spec_hgb_ferr_predict_ferr <- readRDS("./3_intermediate/ensemble/base_mod_spec_hgb_ferr_predict_ferr.RDS")
base_mod_spec_hgb_only_predict_ferr <- readRDS("./3_intermediate/ensemble/base_mod_spec_hgb_only_predict_ferr.RDS")

# Predict hgb top models
# hgb_ferr: Ensemble (6 base models)
# hgb only: Ensemble (6 base models)

# Predict ferritin top model
# hgb ferr: EN.1
# hgb only: RF.125

# add specs to a list
list_of_specs <- list(base_mod_spec_hgb_ferr_predict_hgb, 
                      base_mod_spec_hgb_only_predict_hgb,
                      base_mod_spec_hgb_ferr_predict_ferr$EN.1,  # single model
                      base_mod_spec_hgb_only_predict_ferr$RF.125)  # single model

list_of_configs <- c("5XGB1RF", "5XGB1RF", "1EN", "1RF")

list_of_versions <- c("hgb_ferr_predict_hgb", 
                      "hgb_only_predict_hgb",
                      "hgb_ferr_predict_ferr",
                      "hgb_only_predict_ferr")

print(list_of_specs[[i]])
print(list_of_configs[i])
print(list_of_versions[i])

if (i == 3 || i == 4) {
  outer_fold_assess(base_mod_spec = list_of_specs[[i]], 
                    ensemble_config = list_of_configs[i],
                    version=list_of_versions[i])
} else if (i == 1 || i == 2) {
  outer_fold_assess_ensemble(base_model_specs = list_of_specs[[i]], 
                             ensemble_config = list_of_configs[i],
                             version=list_of_versions[i])
}




