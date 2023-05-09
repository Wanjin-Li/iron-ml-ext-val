Before code submission, check style of code with lintr package: https://github.com/r-lib/lintr

We used R v4.2.2 (https://www.r-project.org/), glmnet v4.1-6, randomForest v4.7-1.1, and xgboost v1.6.0.1.

# How to run
There are two types of analyses: Main model vs. Complex model

# Development and external validation of Main model
- Train on RISE data and validate on SANBS and Vitalant.

Under `2_scripts` directory:

### Preprocess RISE training data and get ensemble internal validation results
1. ./01_preprocess_rise.R 
1. ./02_train_base_models_rise.R 
1. ./train_main_models.R (launch with `./bash/{02_train_rf.sh | 02_train_en.sh | 02_train_xgb.sh}` to launch training jobs to train ML models)
1. ./03_model_selection_and_prep_ensemble.R 
1. ./04_run_ensemble.R (launch with `./bash/04_run_ensemble.sh`)
1. ./05_compare_ensemble.R
1. ./06_ensemble_outer_folds.R (launch with `./bash/06_ensemble_outer_folds.sh`)


### Preprocess SANBS external validation data
1. ./sanbs/00_preprocess_sanbs.R 
1. ./sanbs/01_preprocess_sanbs.R (launch with `./bash/01_preprocess_sanbs.sh`)


### Preprocess Vitalant external validation data
1. ./vitalant/00_preprocess_vitalant.R 
1. ./vitalant/01_preprocess_vitalant.R (launch with `./bash/01_preprocess_vitalant.sh`)


### Train Top Model on all RISE data and external validation 
1. ./07_train_top_model.R (launch with `./bash/07_train_top_model.sh`)
1. ./08_external_validation.R
1. ./09_feature_importance.R
1. ./10_plot_res.R

### Plotting
1. ./plotting_biomarker_difference.R: plots slope graphs and histograms of difference between index and follow up biomarkers
1. ./plotting_sup2_inflammation_rise.R: plots inflammation (ferritin vs. sTfR) in RISE cohort


