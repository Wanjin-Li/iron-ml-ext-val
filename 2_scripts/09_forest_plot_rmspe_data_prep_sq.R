# This script prepares the subgroup dataset of Sanquin.

library(ggplot2)
library(gridExtra)  # for arranging plots
library(dvmisc)
library(cowplot) 
library(patchwork)
library(tidyverse)
library(stringr)
# source("./2_scripts/utility_functions.R")
theme_set(theme_bw()+theme(axis.line = element_line(colour = "black"),
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(),
                           panel.border = element_blank(),
                           panel.background = element_blank()) )


setwd("~/Amber/Canada")

# Load data ----
sq_hgb_only <- read.csv("~/Amber/Canada/private/hgb_only_sanquin.csv")
sq_hgb_ferr <- read.csv("~/Amber/Canada/private/hgb_ferr_sanquin.csv")


# RMSPE function ----
EPSILON <-  1e-10  # prevent division by zero

# get average rmspe
get_rmspe <- function(df) {
  r<-(sqrt(mean(((df$fu_outcome - df$prediction) / (df$fu_outcome  + EPSILON))**2))) * 100
  return(r)
}

# get rpse for each individual prediction
get_rmse <- function(df) {
  r<-(sqrt(((df$fu_outcome - df$prediction) / (df$fu_outcome + EPSILON))**2)) * 100
  return(r)
}


create_subgroup <- function(df){
  
  #categorise age
  
  df <- df %>% mutate(agegroup = case_when(age >= 15  & age <= 24 ~ '15-24',
                                           age > 24  & age <= 39 ~ '25-39',
                                           age > 39  & age <= 64 ~ '40-64',
                                           age > 64 ~ '64+'))
  #redefining reference categories
  df$sex <- relevel(factor(df$sex), ref = 'M')
  #df$race <- relevel(factor(df$race), ref = 'White') 
  df$agegroup <- relevel(factor(df$agegroup), ref = '15-24') 
  df$time_to_fu_quantiles <- relevel(factor(df$time_to_fu_quantiles), ref = 'q1')
  
  return(df)
}

# Sanquin ----  
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

# add column to divide time_to_fu into quartiles

sq_hgb_only$time_to_fu_quantiles <- quant_groups(sq_hgb_only$time_to_fu, 4)
levels(sq_hgb_only$time_to_fu_quantiles)
levels(sq_hgb_only$time_to_fu_quantiles)<-c('q1', 'q2', 'q3', 'q4')

sq_hgb_ferr$time_to_fu_quantiles <- quant_groups(sq_hgb_ferr$time_to_fu, 4)
levels(sq_hgb_ferr$time_to_fu_quantiles)
levels(sq_hgb_ferr$time_to_fu_quantiles)<-c('q1', 'q2', 'q3', 'q4')

# Calculate RMSPE for each prediction 
setDT(sq_hgb_ferr)
setDT(sq_hgb_only)



# hgb only dataset
## predicting  hemoglobin
sq_h_h_rmspe <- sq_hgb_only[, !("fu_log_ferritin")] # remove return log ferritin column
sq_h_h_rmspe <- cbind(sq_h_h_rmspe, sq_h_h)

## predicting  ferritin
sq_h_f_rmspe <- sq_hgb_only[, !("fu_hgb")] # remove return hgb column
sq_h_f_rmspe <- cbind(sq_h_f_rmspe, sq_h_f)

#hgb and ferr dataset
## predicting hemoglobin
sq_hf_h_rmspe <- sq_hgb_ferr[, !("fu_log_ferritin")]
sq_hf_h_rmspe <- cbind(sq_hf_h_rmspe, sq_hf_h)

## predicting ferritin
sq_hf_f_rmspe <- sq_hgb_ferr[, !("fu_hgb")]
sq_hf_f_rmspe <- cbind(sq_hf_f_rmspe, sq_hf_f)

# Create subgroups
sq_h_h_rmspe <- create_subgroup(sq_h_h_rmspe)
sq_h_f_rmspe <- create_subgroup(sq_h_f_rmspe)
sq_hf_h_rmspe <- create_subgroup(sq_hf_h_rmspe)
sq_hf_f_rmspe <- create_subgroup(sq_hf_f_rmspe)

# Calculate average RMSPE by subgroup

# get average rmspe by subgroup
get_rmspe_subgroup <- function(df, subgroup) {
  if (grepl("agegroup", subgroup)){
    subgroup_string <- str_extract(subgroup, "(?<=p).*")
    sub_df <- df[agegroup==subgroup_string]
    r_sub <- get_rmspe(sub_df)
    
  } else if (grepl("sex", subgroup)){
    subgroup_string <- str_extract(subgroup, "[FM]")
    sub_df <- df[sex==subgroup_string]
    r_sub <- get_rmspe(sub_df)
    
  } else if (grepl("race", subgroup)){ 
    r_sub <- 0 # Sanquin doesn't have race
  }
  else if (grepl("time_to_fu_quantiles", subgroup)){
    subgroup_string <- str_sub(subgroup, -2)
    sub_df <- df[time_to_fu_quantiles==subgroup_string]
    r_sub <- get_rmspe(sub_df)
    
  }
  return(r_sub)
}


# Create result table
rmspe_subgroup_dt <- as.data.table(expand.grid(
  Country = c("NL"),
  Dataset = c("Hemoglobin and ferritin", "Hemoglobin only"),
  Outcome = c("Ferritin", "Hemoglobin"),
  Variable = c("time_to_fu_quantilesq4", "time_to_fu_quantilesq3", "time_to_fu_quantilesq2", "time_to_fu_quantilesq1",
                   "sexF", "sexM",
                   "raceHispanic", "raceBlack", "raceAsian", "raceWhite",
                   "agegroup64+", "agegroup40-64", "agegroup25-39", "agegroup15-24"),
  RMSPE = 0)
)



external_datasets <- list(
  # Sanquin: predict Hb with index Hb and ferritin
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq4", data = sq_hf_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq3", data = sq_hf_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq2", data = sq_hf_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq1", data = sq_hf_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="sexF", data = sq_hf_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="sexM", data = sq_hf_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="raceHispanic", data = sq_hf_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="raceBlack", data = sq_hf_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="raceAsian", data = sq_hf_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="raceWhite", data = sq_hf_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="agegroup64+", data = sq_hf_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="agegroup40-64", data = sq_hf_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="agegroup25-39", data = sq_hf_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="agegroup15-24", data = sq_hf_h_rmspe),
  
  # NL: predict Hb with index Hb only
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq4", data = sq_h_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq3", data = sq_h_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq2", data = sq_h_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq1", data = sq_h_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="sexF", data = sq_h_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="sexM", data = sq_h_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="raceHispanic", data = sq_h_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="raceBlack", data = sq_h_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="raceAsian", data = sq_h_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="raceWhite", data = sq_h_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="agegroup64+", data = sq_h_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="agegroup40-64", data = sq_h_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="agegroup25-39", data = sq_h_h_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="agegroup15-24", data = sq_h_h_rmspe),
  
  # NL: predict ferritin with index Hb only
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="time_to_fu_quantilesq4", data = sq_h_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="time_to_fu_quantilesq3", data = sq_h_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="time_to_fu_quantilesq2", data = sq_h_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="time_to_fu_quantilesq1", data = sq_h_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="sexF", data = sq_h_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="sexM", data = sq_h_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="raceHispanic", data = sq_h_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="raceBlack", data = sq_h_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="raceAsian", data = sq_h_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="raceWhite", data = sq_h_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="agegroup64+", data = sq_h_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="agegroup40-64", data = sq_h_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="agegroup25-39", data = sq_h_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="agegroup15-24", data = sq_h_f_rmspe),
  
  # NL: predict ferritin with index Hb and ferritin
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="time_to_fu_quantilesq4", data = sq_hf_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="time_to_fu_quantilesq3", data = sq_hf_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="time_to_fu_quantilesq2", data = sq_hf_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="time_to_fu_quantilesq1", data = sq_hf_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="sexF", data = sq_hf_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="sexM", data = sq_hf_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="raceHispanic", data = sq_hf_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="raceBlack", data = sq_hf_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="raceAsian", data = sq_hf_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="raceWhite", data = sq_hf_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="agegroup64+", data = sq_hf_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="agegroup40-64", data = sq_hf_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="agegroup25-39", data = sq_hf_f_rmspe),
  list(Country = "NL", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="agegroup15-24", data = sq_hf_f_rmspe),
  
)


# Loop through the datasets to update the table
for (ds in external_datasets) {
  rmspe_subgroup_dt[
    Country == ds$Country & Dataset == ds$Dataset & Outcome == ds$Outcome & Variable == ds$Variable,
    RMSPE := get_rmspe_subgroup(ds$data, ds$Variable)
  ]
}

# Save results

fwrite(rmspe_subgroup_dt, "~/Amber/Canada/results_jan/rmspe_subgroup_sq.csv") # replace with the correct file path


