# This script performs addition analysis 1 and 2 for NL.

library(xgboost) 
library(randomForest)
library(glmnet)
library(dplyr)
library(data.table)
library(stringr)
library(catboost) 
library(dvmisc)
library(tidyverse)
library(stringr)

setwd("~/Amber/Canada")

#load data
sq_hgb_only <- read.csv("~/Amber/Canada/private/hgb_only_sanquin.csv")
sq_hgb_ferr <- read.csv("~/Amber/Canada/private/hgb_ferr_sanquin.csv")

# RMSPE function ----
EPSILON <-  1e-10  # prevent division by zero

# get the average rmspe
get_rmspe <- function(df) {
  r<-(sqrt(mean(((df$fu_outcome - df$prediction) / (df$fu_outcome  + EPSILON))**2))) * 100
  return(r)
}


# Sanquin ----  
versions <- c("hgb_ferr_predict_hgb", "hgb_only_predict_hgb", "hgb_ferr_predict_ferr", "hgb_only_predict_ferr")


for (version in versions) {
  print(version)
  sanquin_model_res_path <- list.files(path="./external_validation_sanquin", pattern=version, full.names = TRUE) 
  
  if (length(sanquin_model_res_path) == 1) {  # single model
    model_res_filename <- gsub("^.*/", "", sanquin_model_res_path)  # gets the last element after splitting by "/"
    
    if (grepl(versions[1], model_res_filename, fixed = TRUE)) {
      sq_hf_h <- fread(sanquin_model_res_path)
      sq_hf_h_rmspe <- get_rmspe(sq_hf_h)
      
    } else if (grepl(versions[2], model_res_filename, fixed = TRUE)) {
      sq_h_h <- fread(sanquin_model_res_path)
      sq_h_h_rmspe <- get_rmspe(sq_h_h)
      
    } else if (grepl(versions[3], model_res_filename, fixed = TRUE)) {
      sq_hf_f <- fread(sanquin_model_res_path)
      sq_hf_f_rmspe <- get_rmspe(sq_hf_f)
      
    } else if (grepl(versions[4], model_res_filename, fixed = TRUE)) {
      sq_h_f <- fread(sanquin_model_res_path)
      sq_h_f_rmspe <- get_rmspe(sq_h_f)
      
    }
  } else {  # ensemble model has more than 1 model result to load
    # add prediction of these models together then get average
    sum_df <- fread(sanquin_model_res_path[1])
    sum_df <- sum_df - sum_df  # get empty dataframe
    for (single_model_path in sanquin_model_res_path) {  # combine model results
      single_model_res <- fread(single_model_path)
      sum_df <- sum_df + single_model_res
    }
    avg_df <- sum_df / (length(sanquin_model_res_path))
    
    if (grepl(versions[1], sanquin_model_res_path[1], fixed = TRUE)) {
      sq_hf_h <- avg_df
      sq_hf_h_rmspe <- get_rmspe(avg_df)
      
    } else if (grepl(versions[2], sanquin_model_res_path[1], fixed = TRUE)) {
      sq_h_h <- avg_df
      sq_h_h_rmspe <- get_rmspe(avg_df)
      
    } else if (grepl(versions[3], sanquin_model_res_path[1], fixed = TRUE)) {
      sq_hf_f <- avg_df
      sq_hf_f_rmspe <- get_rmspe(avg_df)
      
    } else if (grepl(versions[4], sanquin_model_res_path[1], fixed = TRUE)) {
      sq_h_f <- avg_df
      sq_h_f_rmspe <- get_rmspe(avg_df)
    }
  }
}



# Extract original test outer fold data to obtain sex information

# Ferritin prediction
test_sq_hf_f <- fread("./path/hgb_ferr_sanquin.csv") # replace with the correct path to extract the corresponding Sanquin dataset used for external validation
test_sq_h_f <- fread("./path/hgb_only_sanquin.csv")

# Hemoglobin prediction
test_sq_hf_h <- fread("./path/hgb_ferr_sanquin.csv")
test_sq_h_h <- fread("./path/hgb_only_sanquin.csv")

# Combine original test data with prediction

# Ferritin prediction
combined_sq_hf_f <- cbind(test_sq_hf_f, sq_hf_f)
combined_sq_h_f <- cbind(test_sq_h_f, sq_h_f)

# Hemoglobin prediction

combined_sq_hf_h <- cbind(test_sq_hf_h, sq_hf_h)
combined_sq_h_h <- cbind(test_sq_h_h, sq_h_h)


# Remove redundant columns
# Ferritin prediction
combined_sq_hf_f <- combined_sq_hf_f[, .(DonorID, sex, prediction, fu_outcome)]
combined_sq_h_f <- combined_sq_h_f[, .(DonorID, sex, prediction, fu_outcome)]

# Hemoglobin prediction
combined_sq_hf_h <- combined_sq_hf_h[, .(DonorID, sex, prediction, fu_outcome)]
combined_sq_h_h <- combined_sq_h_h[, .(DonorID, sex, prediction, fu_outcome)]


# Helper function: count the number of predicted OR observed ferritin below or above the set threshold separately for females and males
count_ferritin <- function(dt, outcome, evaluation, sex){
  if (sex == "female"){
    dt <- dt[sex=="F"]
    if (outcome == "observed" & evaluation == "< 12"){
      count = nrow(dt[fu_outcome < log10(12)])
    } else if (outcome == "observed" & evaluation == ">= 12"){
      count = nrow(dt[!(fu_outcome < log10(12))])
    } else if (outcome == "predicted" & evaluation == "< 12"){
      count = nrow(dt[prediction  < log10(12)])
    } else if (outcome == "predicted" & evaluation == ">= 12"){
      count = nrow(dt[!(prediction  < log10(12))])
    }
  } else if (sex == "male") {
    dt <- dt[sex=="M"]
    if (outcome == "observed" & evaluation == "< 25"){
      count = nrow(dt[fu_outcome < log10(25)])
    } else if (outcome == "observed" & evaluation == ">= 25"){
      count = nrow(dt[!(fu_outcome < log10(25))])
    } else if (outcome == "predicted" & evaluation == "< 25"){
      count = nrow(dt[prediction  < log10(25)])
    } else if (outcome == "predicted" & evaluation == ">= 25"){
      count = nrow(dt[!(prediction  < log10(25))])
    }
  }
  
  return(count)
}


# Calculate the number of observed OR predicted return ferritin below or above the threshold

# Results table

######################## Females ############################################################
low_abs_iron_ferritin_f_dt <- data.table(
  Country = c("NL", "NL"),
  Outer_fold = c(rep("Full dataset", 2)),
  Dataset = c("Hemoglobin and ferritin", "Hemoglobin only"),
  `Observed ferritin < 12 ng/mL` = 0, # initialize as 0
  `Observed ferritin >= 12 ng/mL` = 0,
  `Predicted ferritin < 12 ng/mL` = 0,
  `Predicted ferritin >= 12 ng/mL` = 0)

# Dataset: Hemoglobin and ferritin; Sex: Female; Predict: Ferritin

# NL
low_abs_iron_ferritin_f_dt[1, `Observed ferritin < 12 ng/mL` := count_ferritin(combined_sq_hf_f, "observed", "< 12", "female")]
low_abs_iron_ferritin_f_dt[1, `Observed ferritin >= 12 ng/mL` := count_ferritin(combined_sq_hf_f, "observed", ">= 12", "female")]
low_abs_iron_ferritin_f_dt[1, `Predicted ferritin < 12 ng/mL` := count_ferritin(combined_sq_hf_f, "predicted", "< 12", "female")]
low_abs_iron_ferritin_f_dt[1, `Predicted ferritin >= 12 ng/mL` := count_ferritin(combined_sq_hf_f, "predicted", ">= 12", "female")]


# Dataset: Hemoglobin only; Sex: Female; Predict: Ferritin

# NL
low_abs_iron_ferritin_f_dt[2, `Observed ferritin < 12 ng/mL` := count_ferritin(combined_sq_h_f, "observed", "< 12", "female")]
low_abs_iron_ferritin_f_dt[2, `Observed ferritin >= 12 ng/mL` := count_ferritin(combined_sq_h_f, "observed", ">= 12", "female")]
low_abs_iron_ferritin_f_dt[2, `Predicted ferritin < 12 ng/mL` := count_ferritin(combined_sq_h_f, "predicted", "< 12", "female")]
low_abs_iron_ferritin_f_dt[2, `Predicted ferritin >= 12 ng/mL` := count_ferritin(combined_sq_h_f, "predicted", ">= 12", "female")]

# Further calculation
low_abs_iron_ferritin_f_dt[, `Total` := 
                             `Observed ferritin < 12 ng/mL` + `Observed ferritin >= 12 ng/mL`]
low_abs_iron_ferritin_f_dt[, `% Observed ferritin < 12` := 
                             paste0(round(`Observed ferritin < 12 ng/mL` / `Total` * 100,1), "%")]
low_abs_iron_ferritin_f_dt[, `% Observed ferritin >= 12` := 
                             paste0(round(`Observed ferritin >= 12 ng/mL` / `Total` * 100,1), "%")]
low_abs_iron_ferritin_f_dt[, `% Predicted ferritin < 12` := 
                             paste0(round(`Predicted ferritin < 12 ng/mL` / `Total` * 100,1), "%")]
low_abs_iron_ferritin_f_dt[, `% Predicted ferritin >= 12` := 
                             paste0(round(`Predicted ferritin >= 12 ng/mL` / `Total` * 100,1), "%")]


# Save the result table
fwrite(low_abs_iron_ferritin_f_dt, "./path/low_ferr_f_tbl_sq.csv") # replace with the correct path

# Calculate the number of BOTH observed AND predicted return ferritin below or above the threshold

# Low ferritin cut-off at 12 ng/mL
cross_low_ferritin_12_f_dt <- data.table(
  `Predicted ferritin` = c("< 12 ng/mL", ">= 12 ng/mL"),
  `Observed ferritin < 12 ng/mL` = 0,
  `Observed ferritin >= 12 ng/mL` = 0,
  Country = c(rep("NL", 4)),
  Outer_fold = c(rep("Full dataset", 4)),
  Dataset = c(rep("Hemoglobin and ferritin", 2),
              rep("Hemoglobin only", 2))
)



# Low ferritin cut-off at 25 ng/mL
cross_low_ferritin_25_f_dt <- data.table(
  `Predicted ferritin` = c("< 25 ng/mL", ">= 25 ng/mL"),
  `Observed ferritin < 25 ng/mL` = 0,
  `Observed ferritin >= 25 ng/mL` = 0,
  Country = c(rep("NL", 4)),
  Outer_fold = c(rep("Full dataset", 4)),
  Dataset = c(rep("Hemoglobin and ferritin", 2),
              rep("Hemoglobin only", 2))
)



# Helper function: count the number of BOTH predicted AND observed ferritin below or above the set threshold separately for females and males

count_both_ferritin <- function(dt, evaluation_obs, evaluation_pred, sex){
  if (sex == "female"){
    dt <- dt[sex=="F"]
  } else if (sex == "male"){
    dt <- dt[sex=="M"]
  } else if (sex == "both"){
    dt <- dt
  }
  
  # Low ferritin cut-off at 12 ng/mL
  if (evaluation_obs == "< 12" & evaluation_pred == "< 12"){
    count = nrow(dt[fu_outcome < log10(12) & prediction < log10(12)])
  } else if (evaluation_obs == ">= 12" & evaluation_pred == "< 12"){
    count = nrow(dt[(!(fu_outcome < log10(12))) & prediction < log10(12)])
  } else if (evaluation_obs == "< 12" & evaluation_pred == ">= 12"){
    count = nrow(dt[fu_outcome < log10(12) & (!(prediction < log10(12)))])
  } else if (evaluation_obs == ">= 12" & evaluation_pred == ">= 12"){
    count = nrow(dt[(!(fu_outcome < log10(12))) & (!(prediction < log10(12)))])
  } 
  
  # Low ferritin cut-off at 25 ng/mL
  
  else if (evaluation_obs == "< 25" & evaluation_pred == "< 25"){
    count = nrow(dt[fu_outcome < log10(25) & prediction < log10(25)])
  } else if (evaluation_obs == ">= 25" & evaluation_pred == "< 25"){
    count = nrow(dt[(!(fu_outcome < log10(25))) & prediction < log10(25)])
  } else if (evaluation_obs == "< 25" & evaluation_pred == ">= 25"){
    count = nrow(dt[fu_outcome < log10(25) & (!(prediction < log10(25)))])
  } else if (evaluation_obs == ">= 25" & evaluation_pred == ">= 25"){
    count = nrow(dt[(!(fu_outcome < log10(25))) & (!(prediction < log10(25)))])
  } 
  
  return(count)
}

# Help function: calculate the cross-tabulated results iteratively
cross_ferritin_cal <- function(cross_dt, predicted_ferritin, country, outer_fold, train_dt, observed_column, dt2, evaluation_obs, evaluation_pred, sex) {
  cross_dt[
    `Predicted ferritin` == predicted_ferritin &
      Country == country &
      Outer_fold == outer_fold &
      Dataset == train_dt,
    (observed_column) := count_both_ferritin(
      dt = dt2, 
      evaluation_obs = evaluation_obs, 
      evaluation_pred = evaluation_pred, 
      sex = sex
    )
  ]
}

# For loop to iterate through rows and update the table
cross_low_ferritin_f_list <- list(cross_low_ferritin_12_f_dt, cross_low_ferritin_25_f_dt)


for (list_idx in 1:length(cross_low_ferritin_f_list)){
  low_ferritin_dt <- cross_low_ferritin_f_list[[list_idx]]
  
  for (i in seq_len(nrow(low_ferritin_dt))) {
    # Extract row information
    row <- low_ferritin_dt[i]
    predicted <- row$`Predicted ferritin`
    country <- row$Country
    outer_fold <- row$Outer_fold
    train_dataset <- row$Dataset
    
    if (country=="NL"){
      if (train_dataset=="Hemoglobin and ferritin"){
        dataset <- combined_sq_hf_f
      } else if (train_dataset=="Hemoglobin only"){
        dataset <- combined_sq_h_f
      }
    }

    # Low ferritin cut-off at 12 ng/mL
    
    if (predicted == "< 12 ng/mL") {
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin < 12 ng/mL",
        evaluation_obs = "< 12", evaluation_pred = "< 12", sex = "female"
      )
      
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin >= 12 ng/mL",
        evaluation_obs = ">= 12", evaluation_pred = "< 12", sex = "female"
      )
    } else if (predicted == ">= 12 ng/mL") {
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin < 12 ng/mL",
        evaluation_obs = "< 12", evaluation_pred = ">= 12", sex = "female"
      )
      
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin >= 12 ng/mL",
        evaluation_obs = ">= 12", evaluation_pred = ">= 12", sex = "female"
      )
    }
    
    # Low ferritin cut-off at 25 ng/mL
    
    if (predicted == "< 25 ng/mL") {
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin < 25 ng/mL",
        evaluation_obs = "< 25", evaluation_pred = "< 25", sex = "female"
      )
      
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin >= 25 ng/mL",
        evaluation_obs = ">= 25", evaluation_pred = "< 25", sex = "female"
      )
    } else if (predicted == ">= 25 ng/mL") {
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin < 25 ng/mL",
        evaluation_obs = "< 25", evaluation_pred = ">= 25", sex = "female"
      )
      
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin >= 25 ng/mL",
        evaluation_obs = ">= 25", evaluation_pred = ">= 25", sex = "female"
      )
    }
  }
}


# Save the result table
fwrite(cross_low_ferritin_f_list[[1]], "./path/cross_low_ferr_12_f_tbl_sq.csv") # replace with the correct path
fwrite(cross_low_ferritin_f_list[[2]], "./path/cross_low_ferr_25_f_tbl_sq.csv") # replace with the correct path

######################## Males ##################################################

low_abs_iron_ferritin_m_dt <- data.table(
  Country = c("NL", "NL"),
  Outer_fold = c(rep("Full dataset", 2)),
  Dataset = c("Hemoglobin and ferritin", "Hemoglobin only"),
  `Observed ferritin < 25 ng/mL` = 0, # initialize as 0
  `Observed ferritin >= 25 ng/mL` = 0,
  `Predicted ferritin < 25 ng/mL` = 0,
  `Predicted ferritin >= 25 ng/mL` = 0)

# Dataset: Hemoglobin and ferritin; Sex: Male; Predict: Ferritin
# NL
low_abs_iron_ferritin_m_dt[1, `Observed ferritin < 25 ng/mL` := count_ferritin(combined_s_hf_f, "observed", "< 25", "male")]
low_abs_iron_ferritin_m_dt[1, `Observed ferritin >= 25 ng/mL` := count_ferritin(combined_s_hf_f, "observed", ">= 25", "male")]
low_abs_iron_ferritin_m_dt[1, `Predicted ferritin < 25 ng/mL` := count_ferritin(combined_s_hf_f, "predicted", "< 25", "male")]
low_abs_iron_ferritin_m_dt[1, `Predicted ferritin >= 25 ng/mL` := count_ferritin(combined_s_hf_f, "predicted", ">= 25", "male")]

# Dataset: Hemoglobin only; Sex: Male; Predict: Ferritin
# NL
low_abs_iron_ferritin_m_dt[2, `Observed ferritin < 25 ng/mL` := count_ferritin(combined_s_h_f, "observed", "< 25", "male")]
low_abs_iron_ferritin_m_dt[2, `Observed ferritin >= 25 ng/mL` := count_ferritin(combined_s_h_f, "observed", ">= 25", "male")]
low_abs_iron_ferritin_m_dt[2, `Predicted ferritin < 25 ng/mL` := count_ferritin(combined_s_h_f, "predicted", "< 25", "male")]
low_abs_iron_ferritin_m_dt[2, `Predicted ferritin >= 25 ng/mL` := count_ferritin(combined_s_h_f, "predicted", ">= 25", "male")]

# Further calculation
low_abs_iron_ferritin_m_dt[, `Total` := 
                             `Observed ferritin < 25 ng/mL` + `Observed ferritin >= 25 ng/mL`]
low_abs_iron_ferritin_m_dt[, `% Observed ferritin < 25` := 
                             paste0(round(`Observed ferritin < 25 ng/mL` / `Total` * 100,1), "%")]
low_abs_iron_ferritin_m_dt[, `% Observed ferritin >= 25` := 
                             paste0(round(`Observed ferritin >= 25 ng/mL` / `Total` * 100,1), "%")]
low_abs_iron_ferritin_m_dt[, `% Predicted ferritin < 25` := 
                             paste0(round(`Predicted ferritin < 25 ng/mL` / `Total` * 100,1), "%")]
low_abs_iron_ferritin_m_dt[, `% Predicted ferritin >= 25` := 
                             paste0(round(`Predicted ferritin >= 25 ng/mL` / `Total` * 100,1), "%")]


# Save the result table
fwrite(low_abs_iron_ferritin_m_dt, "./path/low_ferr_m_tbl_sq.csv") # replace with the correct path

# Calculate the number of BOTH observed AND predicted return ferritin below or above the threshold

# Low ferritin cut-off at 12 ng/mL
cross_low_ferritin_12_m_dt <- data.table(
  `Predicted ferritin` = c("< 12 ng/mL", ">= 12 ng/mL"),
  `Observed ferritin < 12 ng/mL` = 0,
  `Observed ferritin >= 12 ng/mL` = 0,
  Country = c(rep("NL", 4)),
  Outer_fold = c(rep("Full dataset", 4)),
  Dataset = c(rep("Hemoglobin and ferritin", 2),
              rep("Hemoglobin only", 2))
)



# Low ferritin cut-off at 25 ng/mL
cross_low_ferritin_25_m_dt <- data.table(
  `Predicted ferritin` = c("< 25 ng/mL", ">= 25 ng/mL"),
  `Observed ferritin < 25 ng/mL` = 0,
  `Observed ferritin >= 25 ng/mL` = 0,
  Country = c(rep("NL", 4)),
  Outer_fold = c(rep("Full dataset", 4)),
  Dataset = c(rep("Hemoglobin and ferritin", 2),
              rep("Hemoglobin only", 2))
)


# For loop to iterate through rows and update the table
cross_low_ferritin_m_list <- list(cross_low_ferritin_12_m_dt, cross_low_ferritin_25_m_dt)


for (list_idx in 1:length(cross_low_ferritin_m_list)){
  low_ferritin_dt <- cross_low_ferritin_m_list[[list_idx]]
  
  for (i in seq_len(nrow(low_ferritin_dt))) {
    # Extract row information
    row <- low_ferritin_dt[i]
    predicted <- row$`Predicted ferritin`
    country <- row$Country
    outer_fold <- row$Outer_fold
    train_dataset <- row$Dataset
    
    if (country=="NL"){
      if (train_dataset=="Hemoglobin and ferritin"){
        dataset <- combined_sq_hf_f
      } else if (train_dataset=="Hemoglobin only"){
        dataset <- combined_sq_h_f
      }
    }
    
    # Low ferritin cut-off at 12 ng/mL
    
    if (predicted == "< 12 ng/mL") {
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin < 12 ng/mL",
        evaluation_obs = "< 12", evaluation_pred = "< 12", sex = "male"
      )
      
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin >= 12 ng/mL",
        evaluation_obs = ">= 12", evaluation_pred = "< 12", sex = "male"
      )
    } else if (predicted == ">= 12 ng/mL") {
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin < 12 ng/mL",
        evaluation_obs = "< 12", evaluation_pred = ">= 12", sex = "male"
      )
      
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin >= 12 ng/mL",
        evaluation_obs = ">= 12", evaluation_pred = ">= 12", sex = "male"
      )
    }
    
    # Low ferritin cut-off at 25 ng/mL
    
    if (predicted == "< 25 ng/mL") {
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin < 25 ng/mL",
        evaluation_obs = "< 25", evaluation_pred = "< 25", sex = "male"
      )
      
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin >= 25 ng/mL",
        evaluation_obs = ">= 25", evaluation_pred = "< 25", sex = "male"
      )
    } else if (predicted == ">= 25 ng/mL") {
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin < 25 ng/mL",
        evaluation_obs = "< 25", evaluation_pred = ">= 25", sex = "male"
      )
      
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin >= 25 ng/mL",
        evaluation_obs = ">= 25", evaluation_pred = ">= 25", sex = "male"
      )
    }
  }
}


# Save the result table
fwrite(cross_low_ferritin_m_list[[1]], "./path/cross_low_ferr_12_m_tbl_sq.csv") # replace with the correct path
fwrite(cross_low_ferritin_m_list[[2]], "./path/cross_low_ferr_25_m_tbl_sq.csv") # replace with the correct path

######################## All ##################################################
cross_low_ferritin_12_dt <- data.table(
  `Predicted ferritin` = c("< 12 ng/mL", ">= 12 ng/mL"),
  `Observed ferritin < 12 ng/mL` = 0,
  `Observed ferritin >= 12 ng/mL` = 0,
  Country = c(rep("NL", 4)),
  Outer_fold = c(rep("Full dataset", 4)),
  Dataset = c(rep("Hemoglobin and ferritin", 2),
              rep("Hemoglobin only", 2))
)



# Low ferritin cut-off at 25 ng/mL
cross_low_ferritin_25_dt <- data.table(
  `Predicted ferritin` = c("< 25 ng/mL", ">= 25 ng/mL"),
  `Observed ferritin < 25 ng/mL` = 0,
  `Observed ferritin >= 25 ng/mL` = 0,
  Country = c(rep("NL", 4)),
  Outer_fold = c(rep("Full dataset", 4)),
  Dataset = c(rep("Hemoglobin and ferritin", 2),
              rep("Hemoglobin only", 2))
)

cross_low_ferritin_list <- list(cross_low_ferritin_12_dt, cross_low_ferritin_25_dt)


for (list_idx in 1:length(cross_low_ferritin_list)){
  low_ferritin_dt <- cross_low_ferritin_list[[list_idx]]
  
  for (i in seq_len(nrow(low_ferritin_dt))) {
    # Extract row information
    row <- low_ferritin_dt[i]
    predicted <- row$`Predicted ferritin`
    country <- row$Country
    outer_fold <- row$Outer_fold
    train_dataset <- row$Dataset
    
    if (country=="NL"){
      if (train_dataset=="Hemoglobin and ferritin"){
        dataset <- combined_sq_hf_f
      } else if (train_dataset=="Hemoglobin only"){
        dataset <- combined_sq_h_f
      }
    }
    
    # Low ferritin cut-off at 12 ng/mL
    
    if (predicted == "< 12 ng/mL") {
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin < 12 ng/mL",
        evaluation_obs = "< 12", evaluation_pred = "< 12", sex = "both"
      )
      
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin >= 12 ng/mL",
        evaluation_obs = ">= 12", evaluation_pred = "< 12", sex = "both"
      )
    } else if (predicted == ">= 12 ng/mL") {
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin < 12 ng/mL",
        evaluation_obs = "< 12", evaluation_pred = ">= 12", sex = "both"
      )
      
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin >= 12 ng/mL",
        evaluation_obs = ">= 12", evaluation_pred = ">= 12", sex = "both"
      )
    }
    
    # Low ferritin cut-off at 25 ng/mL
    
    if (predicted == "< 25 ng/mL") {
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin < 25 ng/mL",
        evaluation_obs = "< 25", evaluation_pred = "< 25", sex = "both"
      )
      
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin >= 25 ng/mL",
        evaluation_obs = ">= 25", evaluation_pred = "< 25", sex = "both"
      )
    } else if (predicted == ">= 25 ng/mL") {
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin < 25 ng/mL",
        evaluation_obs = "< 25", evaluation_pred = ">= 25", sex = "both"
      )
      
      cross_ferritin_cal(
        cross_dt = low_ferritin_dt, predicted_ferritin = predicted, country = country, outer_fold = outer_fold,
        train_dt = train_dataset, dt2 = dataset, observed_column = "Observed ferritin >= 25 ng/mL",
        evaluation_obs = ">= 25", evaluation_pred = ">= 25", sex = "both"
      )
    }
  }
}


# Save the result table
fwrite(cross_low_ferritin_list[[1]], "./path/cross_low_ferr_12_tbl_sq.csv") # replace with the correct path
fwrite(cross_low_ferritin_list[[2]], "./path/cross_low_ferr_25_tbl_sq.csv") # replace with the correct path




# _________________________Additional analysis 1 finished _________________________________________________________________________

# Additional analysis 2: To calculate RMSPE after averaging RMSPE for each individual #############################################


# Calculate RMSPE per donor

# Ferritin prediction
RMSPE_by_donor_pred_f_dt <- data.table(
  Country = c("NL", "NL"),
  Outer_fold = c(rep("Full dataset", 2)),
  Dataset = c("Hemoglobin and ferritin", "Hemoglobin only"),
  Outcome = c(rep("Ferritin", 2)),
  Unadj_RMSPE = c(15.06, 19.09),
  PP_RMSPE = 0)


# Helper function: To calculate average RMSPE per donor first and then take the average RMSPE across all donors
calc_rmspe_per_donor <- function(data, epsilon = EPSILON){
  
  # Step 1: Calculate Percent Error
  data[, PercentError := ((fu_outcome - prediction) / (fu_outcome + epsilon)) * 100]
  
  # Step 2: Square the Percent Error
  data[, SquaredPercentError := PercentError^2]
  
  # Step 3: Compute Mean Squared Percent Error (MSPE) per Donor
  donor_rmspe <- data[, .(MeanSquaredPercentError = mean(SquaredPercentError)), by = DonorID]
  
  # Step 4: Calculate RMSPE for each Donor
  donor_rmspe[, RMSPE := sqrt(MeanSquaredPercentError)]
  
  # Step 5: Compute the Average RMSPE across all Donors
  overall_average_rmspe <- round(mean(donor_rmspe$RMSPE),2)
  
  return(overall_average_rmspe)
}


pred_f_datasets <- list(
  list(Country = "NL", Outer_fold = "Full dataset", Dataset = "Hemoglobin and ferritin", data = combined_sq_hf_f),
  list(Country = "NL", Outer_fold = "Full dataset", Dataset = "Hemoglobin only", data = combined_sq_h_f)
)

# Loop through the datasets to update the table
for (ds in pred_f_datasets) {
  RMSPE_by_donor_pred_f_dt[
    Country == ds$Country & Outer_fold == ds$Outer_fold & Dataset == ds$Dataset,
    PP_RMSPE := calc_rmspe_per_donor(ds$data)
  ]
}


# Save the result
fwrite(RMSPE_by_donor_pred_f_dt, "./path/per_person_rmspe_pred_f_sq.csv")

# Hemoglobin prediction

RMSPE_by_donor_pred_h_dt <- data.table(
  Country = c("NL", "NL"),
  Outer_fold = c(rep("Full dataset", 2)),
  Dataset = c("Hemoglobin and ferritin", "Hemoglobin only"),
  Outcome = c(rep("Hemoglobin", 2)),
  Unadj_RMSPE = c(6.27, 5.87),
  PP_RMSPE = 0)


pred_h_datasets <- list(
  list(Country = "NL", Outer_fold = "Full dataset", Dataset = "Hemoglobin and ferritin", data = combined_sq_hf_h),
  list(Country = "NL", Outer_fold = "Full dataset", Dataset = "Hemoglobin only", data = combined_sq_h_h)
)

# Loop through the datasets to update the table
for (ds in pred_h_datasets) {
  RMSPE_by_donor_pred_h_dt[
    Country == ds$Country & Outer_fold == ds$Outer_fold & Dataset == ds$Dataset,
    PP_RMSPE := calc_rmspe_per_donor(ds$data)
  ]
}


# Save the result
fwrite(RMSPE_by_donor_pred_h_dt, "./path/per_person_rmspe_pred_h_sq.csv")
