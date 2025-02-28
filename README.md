We used R v4.2.2 (https://www.r-project.org/), glmnet v4.1-6, randomForest v4.7-1.1, xgboost v1.6.0.1, and catboost v1.2.2.

# Development and external validation of main model
- Train on RISE data and validate on South Africa (SA), the US and the Netherlands (NL) blood donation data.

# Nested cross-validation
- To obtain an unbiased estimate of model performance.

Under `2_scripts` directory:

### Preprocess SA external validation data
1. ./sanbs/00_preprocess_sanbs.R 
1. ./sanbs/01_preprocess_sanbs.R 


### Preprocess US external validation data
1. ./vitalant/00_preprocess_vitalant.R 
1. ./vitalant/01_preprocess_vitalant.R 


### Preprocess NL external validation data
1. ./sanquin/00_sanquin_summary.R
1. ./sanquin/01_preprocess_sanquin.R


### Helper functions
1. ./utility_functions.R
1. ./02_helper_functions.R

### Preprocess RISE training data and create cross-validation data splits 
1. ./02_setup_base_mod_training.R
1. ./02_setup_nested_cv_splits.R

### Develop base and ensemble models
1. ./03_train_base_mods/R
1. ./03_train_nested_inner_fold.R
1. ./04_model_selection_ensemble_prep.R
1. ./04_run_ensemble.R
1. ./04_nested_model_selection_ensemble_prep.R
1. ./04_nested_run_ensemble.R

### Model selection
1. ./05_compare_ensemble.R
1. ./05_nested_compare_ensemble.R

### Model training
1. ./06_train_top_model.R
1. ./06_nested_train_top_model.R

### Internal validation on RISE and external validation 
1. ./07_rise_test_data_prep.R
1. ./07_rise_internal_validation.R
1. ./07_external_validation.R
1. ./07_external_validation_sanquin.R

### Plotting
1. ./04_model_selection_ensemble_prep.R: eFigure 4
1. ./05_nested_compare_ensemble.R: eFigure 5
1. ./08_plot_rmspe.R: eFigure 6
1. ./08_nested_plot_rmspe.R: Figure 3
1. ./08_nested_plot_mape.R: eFigure 8
1. ./08_nested_plot_rmse.R: eFigure 9
1. ./09_forest_plot.R and 09_subgroup_lm_sanquin.R: Figure 4
1. ./09_foest_plot_rmspe_data_prep.R and ./09_forest_plot_rmspe.R: eFigure 10
1. ./10_SHAP_plot.R: Figure 2
1. ./11_SA_nested_plot_rmspe.R: eFigure 7
1. ./plotting_biomarker_difference.R: eFigure 2 and eFigure 3

### Additional post-doc analyses
1. ./08_nested_plot_rmspe.R
1. ./11_SA_single_donation_data_prep.R
1. ./11_SA_single_donation_data_prep.sq.R
1. ./11_SA_rise_internal_validation_single_donation.R
1. ./11_SA_external_validation.R
1. ./11_SA_external_validation_sq.R
1. ./11_SA_nested_plot_rmspe.R
1. ./12_additional_analysis_sq.R
1. ./12_additional_analysis0_sq.R

Due to privacy concerns, we are unable tp share the preprocessing scripts for RISE data.
