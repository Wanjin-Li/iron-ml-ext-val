# This script plots Figure 2: SHAP summary plot 

library(data.table)
library(shapviz)
library(dplyr)
library(catboost)
library(kernelshap)
library(ggplot2)
library(gridExtra)
library(patchwork)
library("cowplot")


###
# Assess feature importance on final model selection
# Compute SHAP values using Shapviz library and kernelshap
# https://cran.r-project.org/web/packages/shapviz/vignettes/basic_use.html
# 
# Top models for predicting hgb => ensemble
#            for predicitng ferr => CatBoost
###

# Load all fitted models ----
models_list <- list.files(path = "~/iron-ml-ext-val/3_intermediate/trained_models/updates", pattern = "\\.rds$", full.names = TRUE)
fitted_models <- lapply(models_list, readRDS)
names(fitted_models) <- gsub("\\.rds$", "", basename(models_list))

# Wrapper for Catboost to compute shap values ----
shapviz.reformat.varnames <- function(X) {
  custom_mapping <- c(
    "sex" = "Sex",
    "blood_type" = "Blood Type",
    "abo_rh" = "Blood Type",
    "age" = "Age",
    "cum_lifetime_donations" = "Cumulative Lifetime Donations",
    "rbc_loss_last_12_months" = "RBC Loss Last 12 Months",
    "rbc_loss_last_24_months" = "RBC Loss Last 24 Months",
    "days_since_last_rbc_loss" = "Days Since Last RBC Loss",
    "days_since_last_drbc_loss" = "Days Since Last DRBC Loss",
    "index_ferritin" = "Index Ferritin",
    "index_hgb" = "Index HGB",
    "index_log_ferritin" = "Index Log Ferritin",
    "time_to_fu" = "Time to Follow-Up",
    "sexM" = "Sex"
  )
  valid_mapping_keys <- intersect(names(X), names(custom_mapping))
  names(X) <- custom_mapping[valid_mapping_keys]
  
  return(X)
}

shapviz.catboost.Model <- function(object, X_pred, X = X_pred, collapse = NULL, ...) {
  if (!inherits(X_pred, "catboost.Pool")) {
    X_pred <- catboost.load_pool(X_pred)
  }
  S <- catboost.get_feature_importance(object, X_pred, type = "ShapValues", ...)
  pp <- ncol(X_pred) + 1
  baseline <- S[1, pp]
  S <- S[, -pp, drop = FALSE]
  X <- shapviz.reformat.varnames(X)
  colnames(S) <- colnames(X)
  shapviz(S, X = X, baseline = baseline, collapse = collapse)
}

# ------------------------------------------------------------

# Predicing ferritin ----
## Hgb and Ferritin ----
mdset_factors_hgb_ferr <- fread("./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_factors_hgb_ferr.csv")
hgb_ferr_predict_ferr_model <- fitted_models$hgb_ferr_predict_ferr_1CB_CB.1222

# Function to return shapviz object for Catboost individual model
cb_shap <- function(dataset, model) {
  char_columns <- colnames(dataset)[unlist(dataset[, lapply(.SD, is.character),][1,])]
  char_columns <- setdiff(char_columns, "RandID")
  dataset <- dataset %>%
    mutate_at(vars(char_columns), as.factor)

  dataset <- dataset[, -"RandID"]

  x_train <- dataset[, -"fu_log_ferritin"]
  y_train <- dataset$fu_log_ferritin

  train_pool <- catboost.load_pool(data = x_train, label = y_train)

  shp <- shapviz(model, X_pred = train_pool, X = x_train)
  return(shp)
}

shp_hf_f <- cb_shap(mdset_factors_hgb_ferr, hgb_ferr_predict_ferr_model)
saveRDS(shp_hf_f, "./3_intermediate/shap/pred_ferr/shp_hgb_ferr.rds")
sv_importance(shp_hf_f)

## Hgb only ----
mdset_factors_hgb_only <- fread("./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_factors_hgb_only.csv")
hgb_only_predict_ferr_model <- fitted_models$hgb_only_predict_ferr_1CB_CB.1171

shp_ho_f <- cb_shap(mdset_factors_hgb_only, hgb_only_predict_ferr_model)
saveRDS(shp_ho_f, "./3_intermediate/shap/pred_ferr/shp_hgb_only.rds")
sv_importance(shp_ho_f)


# Predicting hemoglobin ----
# For this, we compute the SHAP values of each model in the ensemble and take their average

# Function to return kernelshap object for XGBoost individual model
xgb_shap <- function(dataset, model) {
  dataset <- dataset[, -c("RandID", "fu_hgb")]
  
  X <- data.matrix(dataset)
  bg_X <- data.matrix(dataset[sample(nrow(dataset), 300), ])
  
  ks <- kernelshap(model, X, bg_X = bg_X)
}

# Generates kernelshap object for all models in the ensemble and saves them
ks_pred_HGB <- function(dataset, version, mods) {
  
  for (i in seq_along(mods)) {
    mod <- mods[[i]]
    ks <- xgb_shap(dataset, mod)
    saveRDS(ks, paste0("./3_intermediate/shap/pred_hgb/", version, "_ks", i, ".rds"))
  }
  
}

# Function to return shapviz object for Catboost individual model (predicting HGB)
cb_shap_hgb <- function(dataset, model) {
  char_columns <- colnames(dataset)[unlist(dataset[, lapply(.SD, is.character),][1,])]
  char_columns <- setdiff(char_columns, "RandID")
  dataset <- dataset %>%
    mutate_at(vars(char_columns), as.factor)
  
  dataset <- dataset[, -"RandID"]
  
  x_train <- dataset[, -"fu_hgb"]
  y_train <- dataset$fu_hgb
  
  train_pool <- catboost.load_pool(data = x_train, label = y_train)
  
  shp <- shapviz(model, X_pred = train_pool, X = x_train)
}

## Hgb and ferr ----
pred_hgb__hgb_ferr <- fread("./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_OH_hgb_ferr.csv")

mod1 <- fitted_models$hgb_ferr_predict_hgb_4XGB2CB_XGB.4371
mod2 <- fitted_models$hgb_ferr_predict_hgb_4XGB2CB_XGB.4381
mod3 <- fitted_models$hgb_ferr_predict_hgb_4XGB2CB_XGB.4592
mod4 <- fitted_models$hgb_ferr_predict_hgb_4XGB2CB_XGB.4760

mod_list <- list(mod1, mod2, mod3, mod4)
ks_pred_HGB(pred_hgb__hgb_ferr, "hgb_ferr", mod_list)

## CB
pred_hgb__hgb_ferr_factors <- fread("./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_factors_hgb_ferr.csv")
mod5 <- fitted_models$hgb_ferr_predict_hgb_4XGB2CB_CB.11
mod6 <- fitted_models$hgb_ferr_predict_hgb_4XGB2CB_CB.850
version <- "hgb_ferr"

cb5 <- cb_shap_hgb(pred_hgb__hgb_ferr_factors, mod5)
saveRDS(cb5, paste0("./3_intermediate/shap/pred_hgb/", version, "_CB.11.rds"))

cb6 <- cb_shap_hgb(pred_hgb__hgb_ferr_factors, mod6)
saveRDS(cb6, paste0("./3_intermediate/shap/pred_hgb/", version, "_CB.850.rds"))

## Hgb only ----
pred_hgb__hgb_only <- fread("./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_OH_hgb_only.csv")

mod1 <- fitted_models$hgb_only_predict_hgb_5XGB1CB_XGB.3207
mod2 <- fitted_models$hgb_only_predict_hgb_5XGB1CB_XGB.3463
mod3 <- fitted_models$hgb_only_predict_hgb_5XGB1CB_XGB.4008
mod4 <- fitted_models$hgb_only_predict_hgb_5XGB1CB_XGB.4058
mod5 <- fitted_models$hgb_only_predict_hgb_5XGB1CB_XGB.4166

mod_list <- list(mod1, mod2, mod3, mod4, mod5)
ks_pred_HGB(pred_hgb__hgb_only, "hgb_only", mod_list)

## CB
pred_hgb__hgb_only_factors <- fread("./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_factors_hgb_only.csv")
cb_mod <- fitted_models$hgb_only_predict_hgb_5XGB1CB_CB.536
version <- "hgb_only"

cb6 <- cb_shap_hgb(pred_hgb__hgb_only_factors, cb_mod)
saveRDS(cb6, paste0("./3_intermediate/shap/pred_hgb/", version, "_CB.536.rds"))

# Format SHAP values ----

## Predicting hgb ----

# Since we used OH encoding to train the models, SHAP values were calculated on OH. 
# To get categorical SHAP data, we need to sum binary values of each category
# 2 categories: blood type and sex

convert_OH_to_Cat_SHAP <- function(dt_shap) {
  cols_to_sum <- c("blood_typeA+", "blood_typeAB-", "blood_typeAB+", "blood_typeB-", "blood_typeB+", "blood_typeO-", "blood_typeO+")
  dt_shap[, blood_type := rowSums(.SD), .SDcols = cols_to_sum]
  dt_shap <- dt_shap[, setdiff(names(dt_shap), cols_to_sum), with=FALSE]
  # For sex, only 2 possible values so SHAP values for sexM are representative for the category already
  dt_shap <- dt_shap[, -"(Intercept)"]
  return(dt_shap)
}

compute_avg_SHAP <- function(shapvalues_list, version) {
  processed_shap_values_list <- list()
  for (i in seq_along(shapvalues_list)) {
    shap_val <- shapvalues_list[[i]]
    dt_shap <- as.data.table(shap_val$S)
    
    if (ncol(dt_shap) > 12) {
      dt_shap <- convert_OH_to_Cat_SHAP(dt_shap)
      # Rename column names
      dt_shap <- shapviz.reformat.varnames(dt_shap)
    }
    
    if (version == "hgb_ferr") {
      # Reorder so columns are same order in all data tables
      desired_order <- c("RBC Loss Last 12 Months", "RBC Loss Last 24 Months", "Days Since Last RBC Loss",
                         "Days Since Last DRBC Loss", "Cumulative Lifetime Donations", "Index HGB",
                         "Blood Type", "Age", "Sex",
                         "Index Ferritin", "Index Log Ferritin", "Time to Follow-Up")
      
    } else {
      desired_order <- c("RBC Loss Last 12 Months", "RBC Loss Last 24 Months", "Days Since Last RBC Loss",
                         "Days Since Last DRBC Loss", "Cumulative Lifetime Donations", "Index HGB",
                         "Blood Type", "Age", "Sex", "Time to Follow-Up")
    }
    
    dt_shap <- dt_shap[, ..desired_order]
    processed_shap_values_list[[i]] <- dt_shap
  }
  
  if (version == "hgb_ferr") {
    avg_shapvalues <- data.table(matrix(NA, nrow = 2625, ncol = 12))
    names(avg_shapvalues) <- c("RBC Loss Last 12 Months", "RBC Loss Last 24 Months", "Days Since Last RBC Loss",
                               "Days Since Last DRBC Loss", "Cumulative Lifetime Donations", "Index HGB",
                               "Blood Type", "Age", "Sex",
                               "Index Ferritin", "Index Log Ferritin", "Time to Follow-Up")
    n = 12
  } else {
    avg_shapvalues <- data.table(matrix(NA, nrow = 3488, ncol = 10))
    names(avg_shapvalues) <- c("RBC Loss Last 12 Months", "RBC Loss Last 24 Months", "Days Since Last RBC Loss",
                               "Days Since Last DRBC Loss", "Cumulative Lifetime Donations", "Index HGB",
                               "Blood Type", "Age", "Sex", "Time to Follow-Up")
    n = 10
  }
  
  for (i in 1:n) { # 12 for hgb ferr
    sum_shapvalues <- NULL
    
    for (j in 1:length(processed_shap_values_list)) {
      
      curr_mod <- processed_shap_values_list[[j]]
      
      if (is.null(sum_shapvalues)) {
        sum_shapvalues <- curr_mod[, ..i]  # Use ..i to refer to the column index stored in i
      } else {
        sum_shapvalues <- sum_shapvalues + curr_mod[, ..i]  # Use ..i here as well
      }
      
    }
    
    mean_shapvalues <- sum_shapvalues / 6 # Divide by 6 because 6 models
    avg_shapvalues[, i] <- mean_shapvalues
  }
  
  return(avg_shapvalues)
}

### Hgb and ferr ----

shapvalues_list_hgb_ferr <- list()

# Load data
for (mod in c('XGB.4371', 'XGB.4381', 'XGB.4592', 'XGB.4760', 'CB.11', 'CB.850')) {
  shapvalues_list_hgb_ferr[[mod]] <- readRDS(paste0("./3_intermediate/shap/pred_hgb/hgb_ferr_", mod, ".rds"))
}

# Save avg_shapvalues ----
avg_shapvalues_hgb_ferr <- compute_avg_SHAP(shapvalues_list_hgb_ferr, "hgb_ferr")
saveRDS(avg_shapvalues_hgb_ferr, "./3_intermediate/shap/pred_hgb/hgb_ferr_avg_SHAP_values.rds")

X <- shapviz.reformat.varnames(pred_hgb__hgb_ferr_factors[, -c("fu_hgb", "RandID")])
shp_hf_h <- shapviz(as.matrix(avg_shapvalues_hgb_ferr), X = X)
saveRDS(shp_hf_h, "./3_intermediate/shap/pred_hgb/shp_hgb_ferr.rds")

### Hgb only ----

shapvalues_list_hgb_only <- list()

# Load data
for (mod in c('XGB.3207', 'XGB.3463', 'XGB.4008', 'XGB.4058', 'XGB.4166', 'CB.536')) {
  shapvalues_list_hgb_only[[mod]] <- readRDS(paste0("./3_intermediate/shap/pred_hgb/hgb_only_", mod, ".rds"))
}

# Save avg_shapvalues ----
avg_shapvalues_hgb_only <- compute_avg_SHAP(shapvalues_list_hgb_only, "hgb_only")
saveRDS(avg_shapvalues_hgb_only, "./3_intermediate/shap/pred_hgb/hgb_only_avg_SHAP_values.rds")

X <- shapviz.reformat.varnames(pred_hgb__hgb_only_factors[, -c("fu_hgb", "RandID")])
shp_ho_h <- shapviz(as.matrix(avg_shapvalues_hgb_only), X = X)
saveRDS(shp_ho_h, "./3_intermediate/shap/pred_hgb/shp_hgb_only.rds")

# VISUALIZE ----

# Plots total
shp_hf_f <- readRDS("./3_intermediate/shap/pred_ferr/shp_hgb_ferr.rds")
shp_ho_f <- readRDS("./3_intermediate/shap/pred_ferr/shp_hgb_only.rds")
shp_hf_h <- readRDS("./3_intermediate/shap/pred_hgb/shp_hgb_ferr.rds")
shp_ho_h <- readRDS("./3_intermediate/shap/pred_hgb/shp_hgb_only.rds")

plot_shp_hf_f <- sv_importance(shp_hf_f, show_numbers = TRUE, kind = "beeswarm")
plot_shp_ho_f <- sv_importance(shp_ho_f, show_numbers = TRUE, kind = "beeswarm")
plot_shp_hf_h <- sv_importance(shp_hf_h, show_numbers = TRUE, kind = "beeswarm")
plot_shp_ho_h <- sv_importance(shp_ho_h, show_numbers = TRUE, kind = "beeswarm")

# Save individual plots
ggsave("./3_intermediate/shap/pred_ferr__hgb_ferr_SHAP_plot.png",
       plot = plot_shp_hf_f,
       width = 6.5,
       height = 4.5,
       units = "in")

ggsave("./3_intermediate/shap/pred_ferr__hgb_only_SHAP_plot.png",
       plot = plot_shp_ho_f,
       width = 6.5,
       height = 4.5,
       units = "in")

ggsave("./3_intermediate/shap/pred_hgb__hgb_ferr_SHAP_plot.png",
       plot = plot_shp_hf_h,
       width = 6.5,
       height = 4.5,
       units = "in")

ggsave("./3_intermediate/shap/pred_hgb__hgb_only_SHAP_plot.png",
       plot = plot_shp_ho_h,
       width = 6.5,
       height = 4.5,
       units = "in")

# Combine them into 1
plot_shp_hf_h <- plot_shp_hf_h +
  labs(title = "Using Baseline Hemoglobin and Ferritin", x = "", y = "Predict Follow-up Hemoglobin\n") +
  theme_set(theme_bw()) +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5), # Centered, bold, and larger title
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.margin = unit(c(0.5, 0, 0, 0.5), "cm"),
    legend.position = "none",
    text = element_text(family = "Arial")
  )

plot_shp_ho_h <- plot_shp_ho_h + 
  labs(title = "Using Baseline Hemoglobin Only", x = "", y = "\n") +
  theme_set(theme_bw()) +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5), # Centered, bold, and larger title
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.margin = unit(c(0.5, 0.5, 0, 0), "cm"),
    legend.position = "none",
    text = element_text(family = "Arial")
  )

plot_shp_hf_f <- plot_shp_hf_f +
  labs(title = "", x = "SHAP value", y = "Predict Follow-up Log10 Ferritin\n") +
  theme_set(theme_bw()) +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5), # Centered, bold, and larger title
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.margin = unit(c(0, 0, 0.5, 0.5), "cm"),
    legend.position = "none",
    text = element_text(family = "Arial")
  )

plot_shp_ho_f <- plot_shp_ho_f + 
  labs(title = "", x = "SHAP value", y = "\n") +
  theme_set(theme_bw()) +
  theme(
    plot.margin = unit(c(0, 0.5, 0.5, 0), "cm"),
    legend.position = "none",
    text = element_text(family = "Arial")
  )

# Arrange the plots in a 2x2 grid
combined_plot <- (plot_shp_hf_h + plot_shp_ho_h + plot_shp_hf_f + plot_shp_ho_f)
combined_plot <- combined_plot + plot_layout(guides = 'collect') &
  theme(
    legend.position = "right",
    legend.justification = c(0, 1), 
    legend.box.margin = ggplot2::margin(t = 80, r = 10, b = 10, l = 10)  # Adjust margins around legend box if needed
)

ggsave("./3_intermediate/shap/all_SHAP_plot_1.png",
       plot = combined_plot,
       width = 14,
       height = 8.5,
       units = "in",
       dpi = 300,
       device = "png")

# Legends
shp_tmp <- copy(shp_ho_f)
shp_S_tmp <- as.data.table(shp_tmp$S)
shp_X_tmp <- as.data.table(shp_tmp$X)

# setnames(shp_S_tmp, "Sex (high = male)", "Sex")
# setnames(shp_X_tmp, "Sex (high = male)", "Sex")

shp_tmp$S <- shp_S_tmp
shp_tmp$X <- shp_X_tmp

# Sex Legend
plot <- sv_dependence(shp_tmp, v = "Blood Type", color_var = "Sex")
sex_legend <- get_legend(plot)

# Blood legend
blood_plot <- sv_dependence(shp_tmp, v = "Sex", color_var = "Blood Type")
blood_legend <- get_legend(blood_plot)

# Create new plot window 
grid.newpage()                               
grid.draw(blood_legend) 

ggsave("./3_intermediate/shap/sex_legend.png",
       plot = sex_legend,
       width = 5,
       height = 5,
       units = "in",
       dpi = 300)

ggsave("./3_intermediate/shap/blood_legend.png",
       plot = blood_legend,
       width = 5,
       height = 5,
       units = "in",
       dpi = 300)


# Get plot without the legend
combined_plot_no_legend <- (plot_shp_hf_h + plot_shp_ho_h + plot_shp_hf_f + plot_shp_ho_f) +
  plot_layout(guides = 'collect') & 
  theme(
    legend.position = "none"  # This removes the legend from combined_plot
  )

# Extract the existing legend
plot_shp_hf_h_legend <- plot_shp_hf_h +
  labs(title = "Using Baseline Hemoglobin and Ferritin", x = "", y = "Predict Follow-up Hemoglobin\n") +
  theme_set(theme_bw()) +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5), # Centered, bold, and larger title
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.margin = unit(c(0.5, 0, 0, 0.5), "cm"),
    text = element_text(family = "Arial")
  )
existing_legend <- get_legend(plot_shp_hf_h_legend)

# Wrap the legends as elements to be combined with patchwork
wrapped_sex_legend <- wrap_elements(sex_legend)
wrapped_blood_legend <- wrap_elements(blood_legend)
wrapped_existing_legend <- wrap_elements(existing_legend)

# Stack legends
stacked_legends <- wrap_elements(wrapped_existing_legend / wrapped_sex_legend / wrapped_blood_legend)
stacked_legends_grob <- ggplotGrob(
  stacked_legends
)

grid.newpage()                               
grid.draw(stacked_legends) 

# Overlay the stacked legends onto the combined plot
final_plot <- (combined_plot_no_legend | stacked_legends) +  
  plot_layout(widths = c(8, 1))  # Apply to the full layout, adjust this width as needed


grid.newpage()                               
grid.draw(final_plot) 

# Save
ggsave("./3_intermediate/shap/all_SHAP_plot_with_legends.png",
       plot = final_plot,
       width = 14,
       height = 8.5,
       units = "in",
       dpi = 300,
       device = "png")



ggsave("./3_intermediate/shap/all_SHAP_plot_with_legends.pdf",
       plot = final_plot,
       width = 14,
       height = 8.5,
       units = "in",
       dpi = 300,
       device = "pdf",
       family = "sans")



