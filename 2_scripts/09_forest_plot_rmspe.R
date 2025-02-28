# This script plots eFigure 10: forest plot of RMSPE by donor subgroup categories.


# To create a forest plot representing the RMSPE for each category

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
                                           age >= 24  & age <= 39 ~ '25-39',
                                           age >= 39  & age <= 64 ~ '40-64',
                                           age >= 64 ~ '64+'))
  #redefining reference categories
  df$sex <- relevel(factor(df$sex), ref = 'M')
  df$race <- relevel(factor(df$race), ref = 'White') 
  df$agegroup <- relevel(factor(df$agegroup), ref = '15-24') 
  df$time_to_fu_quantiles <- relevel(factor(df$time_to_fu_quantiles), ref = 'q1')
  
  return(df)
}

# Load prediction results ----

## SANBS ----
for (version in versions) {
  print(version)
  sanbs_model_res_paths <- list.files(path="./3_intermediate/external_validation/updates/sanbs", pattern=version, full.names = TRUE) 
  
  if (length(sanbs_model_res_paths) == 1) {  # single model
    model_res_filename <- gsub("^.*/", "", sanbs_model_res_paths)  # gets the last element after splitting by "/"
    
    if (grepl(versions[1], model_res_filename, fixed = TRUE)) {
      s_hf_h <- fread(sanbs_model_res_paths)
      s_hf_h_rmspe <- get_rmspe(s_hf_h)
      
    } else if (grepl(versions[2], model_res_filename, fixed = TRUE)) {
      s_h_h <- fread(sanbs_model_res_paths)
      s_h_h_rmspe <- get_rmspe(s_h_h)
      
    } else if (grepl(versions[3], model_res_filename, fixed = TRUE)) {
      s_hf_f <- fread(sanbs_model_res_paths)
      s_hf_f_rmspe <- get_rmspe(s_hf_f)
      
    } else if (grepl(versions[4], model_res_filename, fixed = TRUE)) {
      s_h_f <- fread(sanbs_model_res_paths)
      s_h_f_rmspe <- get_rmspe(s_h_f)
      
    }
  } else {  # ensemble model has more than 1 model result to load
    # add prediction of these models together then get average
    sum_df <- fread(sanbs_model_res_paths[1])
    sum_df <- sum_df - sum_df  # get empty dataframe
    for (single_model_path in sanbs_model_res_paths) {  # combine model results
      single_model_res <- fread(single_model_path)
      sum_df <- sum_df + single_model_res
    }
    avg_df <- sum_df / (length(sanbs_model_res_paths))
    
    if (grepl(versions[1], sanbs_model_res_paths[1], fixed = TRUE)) {
      s_hf_h <- avg_df
      s_hf_h_rmspe <- get_rmspe(avg_df)
      
    } else if (grepl(versions[2], sanbs_model_res_paths[1], fixed = TRUE)) {
      s_h_h <- avg_df
      s_h_h_rmspe <- get_rmspe(avg_df)
      
    } else if (grepl(versions[3], sanbs_model_res_paths[1], fixed = TRUE)) {
      s_hf_f <- avg_df
      s_hf_f_rmspe <- get_rmspe(avg_df)
      
    } else if (grepl(versions[4], sanbs_model_res_paths[1], fixed = TRUE)) {
      s_h_f <- avg_df
      s_h_f_rmspe <- get_rmspe(avg_df)
      
    }
  }
}


## Vitalant ----
for (version in versions) {
  print(version)
  vitalant_model_res_paths <- list.files(path="./3_intermediate/external_validation/updates/vitalant", pattern=version, full.names = TRUE) 
  
  if (length(vitalant_model_res_paths) == 1) {  # single model
    model_res_filename <- gsub("^.*/", "", vitalant_model_res_paths)  # gets the last element after splitting by "/"
    
    if (grepl(versions[1], model_res_filename, fixed = TRUE)) {
      v_hf_h <- fread(vitalant_model_res_paths)
      v_hf_h_rmspe <- get_rmspe(v_hf_h)
      
    } else if (grepl(versions[2], model_res_filename, fixed = TRUE)) {
      v_h_h <- fread(vitalant_model_res_paths)
      v_h_h_rmspe <- get_rmspe(v_h_h)
      
    } else if (grepl(versions[3], model_res_filename, fixed = TRUE)) {
      v_hf_f <- fread(vitalant_model_res_paths)
      v_hf_f_rmspe <- get_rmspe(v_hf_f)
      
    } else if (grepl(versions[4], model_res_filename, fixed = TRUE)) {
      v_h_f <- fread(vitalant_model_res_paths)
      v_h_f_rmspe <- get_rmspe(v_h_f)
      
    }
  } else {  # ensemble model has more than 1 model result to load
    # add prediction of these models together then get average
    sum_df <- fread(vitalant_model_res_paths[1])
    sum_df <- sum_df - sum_df  # get empty dataframe
    for (single_model_path in vitalant_model_res_paths) {  # combine model results
      single_model_res <- fread(single_model_path)
      sum_df <- sum_df + single_model_res
    }
    avg_df <- sum_df / (length(vitalant_model_res_paths))
    
    if (grepl(versions[1], vitalant_model_res_paths[1], fixed = TRUE)) {
      v_hf_h <- avg_df
      v_hf_h_rmspe <- get_rmspe(avg_df)
      
    } else if (grepl(versions[2], vitalant_model_res_paths[1], fixed = TRUE)) {
      v_h_h <- avg_df
      v_h_h_rmspe <- get_rmspe(avg_df)
      
    } else if (grepl(versions[3], vitalant_model_res_paths[1], fixed = TRUE)) {
      v_hf_f <- avg_df
      v_hf_f_rmspe <- get_rmspe(avg_df)
      
    } else if (grepl(versions[4], vitalant_model_res_paths[1], fixed = TRUE)) {
      v_h_f <- avg_df
      v_h_f_rmspe <- get_rmspe(avg_df)
      
    }
  }
}

## Sanquin ----  
# Note has NAs in some of the dataframe results so remove these rows for now until Amber reruns the external validation code
for (version in versions) {
  print(version)
  sanquin_model_res_path <- list.files(path="./3_intermediate/external_validation/updates/sanquin", pattern=version, full.names = TRUE) 
  
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



# Load data ----

## SANBS ----

s_df <- fread("./3_intermediate/private/sanbs_intermediate_to_del4.csv")
s_df <- s_df[, ..hgb_ferr_var_race]

# s_df$index_hgb <- as.numeric(s_df$index_hgb) 
# s_df$fu_hgb <- as.numeric(s_df$fu_hgb)

s_hgb_ferr <- fread("./3_intermediate/private/hgb_ferr_sanbs.csv")
s_hgb_only <- fread("./3_intermediate/private/hgb_only_sanbs.csv")

# add race back in
s_hgb_ferr <- data.frame(merge(x=s_hgb_ferr, y=s_df, by=hgb_ferr_var, all.x=TRUE))
s_hgb_only <- data.frame(merge(x=s_hgb_only, y=s_df, by=hgb_only_var, all.x=TRUE))

# add column to divide time_to_fu into quartiles
s_hgb_only$time_to_fu_quantiles <- quant_groups(s_hgb_only$time_to_fu, 4)
levels(s_hgb_only$time_to_fu_quantiles)
levels(s_hgb_only$time_to_fu_quantiles)<-c('q1', 'q2', 'q3', 'q4')

s_hgb_ferr$time_to_fu_quantiles <- quant_groups(s_hgb_ferr$time_to_fu, 4)
levels(s_hgb_ferr$time_to_fu_quantiles)
levels(s_hgb_ferr$time_to_fu_quantiles)<-c('q1', 'q2', 'q3', 'q4')


## Vitalant ----
v_df <- data.frame(fread("./3_intermediate/private/vitalant_updates/vitalant_intermediate_to_del6.csv"))
v_df <- v_df[,hgb_ferr_var_race]

v_hgb_ferr <- fread("./3_intermediate/private/vitalant_updates/hgb_ferr_vitalant.csv")  
v_hgb_only <- fread("./3_intermediate/private/vitalant_updates/hgb_only_vitalant.csv")  

# add race back in
v_hgb_ferr <- data.frame(merge(x=v_hgb_ferr, y=v_df, by=hgb_ferr_var, all.x=TRUE))
v_hgb_only <- data.frame(merge(x=v_hgb_only, y=v_df, by=hgb_only_var, all.x=TRUE))

# add column to divide time_to_fu into quartiles
v_hgb_only$time_to_fu_quantiles <- quant_groups(v_hgb_only$time_to_fu, 4)
levels(v_hgb_only$time_to_fu_quantiles)
levels(v_hgb_only$time_to_fu_quantiles)<-c('q1', 'q2', 'q3', 'q4')

v_hgb_ferr$time_to_fu_quantiles <- quant_groups(v_hgb_ferr$time_to_fu, 4)
levels(v_hgb_ferr$time_to_fu_quantiles)
levels(v_hgb_ferr$time_to_fu_quantiles)<-c('q1', 'q2', 'q3', 'q4')


# Calculate RMSPE for each prediction 
setDT(s_hgb_ferr)
setDT(s_hgb_only)
setDT(v_hgb_ferr)
setDT(v_hgb_only)


# hgb only dataset
## predicting  hemoglobin
s_h_h_rmspe <- s_hgb_only[, !("fu_log_ferritin")] # remove return log ferritin column
s_h_h_rmspe <- cbind(s_h_h_rmspe, s_h_h)

v_h_h_rmspe <- v_hgb_only[, !("fu_log_ferritin")] # remove return log ferritin column
v_h_h_rmspe <- cbind(v_h_h_rmspe, v_h_h)

## predicting  ferritin
s_h_f_rmspe <- s_hgb_only[, !("fu_hgb")] # remove return hgb column
s_h_f_rmspe <- cbind(s_h_f_rmspe, s_h_f)

v_h_f_rmspe <- v_hgb_only[, !("fu_hgb")] # remove return hgb column
v_h_f_rmspe <- cbind(v_h_f_rmspe, v_h_f)

#hgb and ferr dataset
## predicting hemoglobin
s_hf_h_rmspe <- s_hgb_ferr[, !("fu_log_ferritin")]
s_hf_h_rmspe <- cbind(s_hf_h_rmspe, s_hf_h)

v_hf_h_rmspe <- v_hgb_ferr[, !("fu_log_ferritin")]
v_hf_h_rmspe <- cbind(v_hf_h_rmspe, v_hf_h)

## predicting ferritin
s_hf_f_rmspe <- s_hgb_ferr[, !("fu_hgb")]
s_hf_f_rmspe <- cbind(s_hf_f_rmspe, s_hf_f)

v_hf_f_rmspe <- v_hgb_ferr[, !("fu_hgb")]
v_hf_f_rmspe <- cbind(v_hf_f_rmspe, v_hf_f)

# Create subgroups
s_h_h_rmspe <- create_subgroup(s_h_h_rmspe)
s_h_f_rmspe <- create_subgroup(s_h_f_rmspe)
s_hf_h_rmspe <- create_subgroup(s_hf_h_rmspe)
s_hf_f_rmspe <- create_subgroup(s_hf_f_rmspe)

v_h_h_rmspe <- create_subgroup(v_h_h_rmspe)
v_h_f_rmspe <- create_subgroup(v_h_f_rmspe)
v_hf_h_rmspe <- create_subgroup(v_hf_h_rmspe)
v_hf_f_rmspe <- create_subgroup(v_hf_f_rmspe)

# Calculate average RMSPE by subgroup

# get average rmspe by subgroup
get_rmspe_subgroup <- function(df, subgroup) {
  if (grepl("agegroup", subgroup)){
    subgroup_string <- str_extract(subgroup, "(?<=p).*")
    sub_df <- df[agegroup==subgroup_string]
  } else if (grepl("sex", subgroup)){
    subgroup_string <- str_extract(subgroup, "[FM]")
    sub_df <- df[sex==subgroup_string]
  } else if (grepl("race", subgroup)){
    subgroup_string <- sub("race", "", subgroup)
    sub_df <- df[race==subgroup_string]
  } else if (grepl("time_to_fu_quantiles", subgroup)){
    subgroup_string <- str_sub(subgroup, -2)
    sub_df <- df[time_to_fu_quantiles==subgroup_string]
  }
  
  r_sub <- get_rmspe(sub_df)
  return(r_sub)
}




# Create result table
rmspe_subgroup_dt <- data.table(
  Country = rep(c("US", "SA"), each = 56),
  Dataset = rep(c("Hemoglobin only", "Hemoglobin and ferritin"), each = 28, times = 2),
  Outcome = rep(c("Hemoglobin", "Ferritin"), each = 14, times = 4),
  Variable = rep(c("time_to_fu_quantilesq4", "time_to_fu_quantilesq3", "time_to_fu_quantilesq2", "time_to_fu_quantilesq1",
                   "sexF", "sexM",
                   "raceHispanic", "raceBlack", "raceAsian", "raceWhite",
                   "agegroup64+", "agegroup40-64", "agegroup25-39", "agegroup15-24"), times=8),
  RMSPE = 0
)


external_datasets <- list(
  # SA: predict Hb with index Hb and ferritin
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq4", data = s_hf_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq3", data = s_hf_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq2", data = s_hf_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq1", data = s_hf_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="sexF", data = s_hf_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="sexM", data = s_hf_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="raceHispanic", data = s_hf_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="raceBlack", data = s_hf_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="raceAsian", data = s_hf_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="raceWhite", data = s_hf_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="agegroup64+", data = s_hf_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="agegroup40-64", data = s_hf_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="agegroup25-39", data = s_hf_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="agegroup15-24", data = s_hf_h_rmspe),
  
  # SA: predict Hb with index Hb only
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq4", data = s_h_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq3", data = s_h_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq2", data = s_h_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq1", data = s_h_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="sexF", data = s_h_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="sexM", data = s_h_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="raceHispanic", data = s_h_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="raceBlack", data = s_h_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="raceAsian", data = s_h_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="raceWhite", data = s_h_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="agegroup64+", data = s_h_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="agegroup40-64", data = s_h_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="agegroup25-39", data = s_h_h_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="agegroup15-24", data = s_h_h_rmspe),
  
  # SA: predict ferritin with index Hb only
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="time_to_fu_quantilesq4", data = s_h_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="time_to_fu_quantilesq3", data = s_h_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="time_to_fu_quantilesq2", data = s_h_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="time_to_fu_quantilesq1", data = s_h_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="sexF", data = s_h_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="sexM", data = s_h_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="raceHispanic", data = s_h_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="raceBlack", data = s_h_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="raceAsian", data = s_h_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="raceWhite", data = s_h_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="agegroup64+", data = s_h_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="agegroup40-64", data = s_h_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="agegroup25-39", data = s_h_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="agegroup15-24", data = s_h_f_rmspe),
  
  # SA: predict ferritin with index Hb and ferritin
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="time_to_fu_quantilesq4", data = s_hf_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="time_to_fu_quantilesq3", data = s_hf_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="time_to_fu_quantilesq2", data = s_hf_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="time_to_fu_quantilesq1", data = s_hf_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="sexF", data = s_hf_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="sexM", data = s_hf_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="raceHispanic", data = s_hf_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="raceBlack", data = s_hf_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="raceAsian", data = s_hf_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="raceWhite", data = s_hf_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="agegroup64+", data = s_hf_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="agegroup40-64", data = s_hf_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="agegroup25-39", data = s_hf_f_rmspe),
  list(Country = "SA", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="agegroup15-24", data = s_hf_f_rmspe),
  
  # US: predict Hb with index Hb and ferritin
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq4", data = v_hf_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq3", data = v_hf_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq2", data = v_hf_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq1", data = v_hf_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="sexF", data = v_hf_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="sexM", data = v_hf_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="raceHispanic", data = v_hf_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="raceBlack", data = v_hf_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="raceAsian", data = v_hf_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="raceWhite", data = v_hf_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="agegroup64+", data = v_hf_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="agegroup40-64", data = v_hf_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="agegroup25-39", data = v_hf_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Hemoglobin", Variable="agegroup15-24", data = v_hf_h_rmspe),
  
  # US: predict Hb with index Hb only
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq4", data = v_h_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq3", data = v_h_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq2", data = v_h_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="time_to_fu_quantilesq1", data = v_h_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="sexF", data = v_h_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="sexM", data = v_h_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="raceHispanic", data = v_h_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="raceBlack", data = v_h_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="raceAsian", data = v_h_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="raceWhite", data = v_h_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="agegroup64+", data = v_h_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="agegroup40-64", data = v_h_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="agegroup25-39", data = v_h_h_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Hemoglobin", Variable="agegroup15-24", data = v_h_h_rmspe),
  
  # US: predict ferritin with index Hb and ferritin
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="time_to_fu_quantilesq4", data = v_hf_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="time_to_fu_quantilesq3", data = v_hf_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="time_to_fu_quantilesq2", data = v_hf_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="time_to_fu_quantilesq1", data = v_hf_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="sexF", data = v_hf_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="sexM", data = v_hf_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="raceHispanic", data = v_hf_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="raceBlack", data = v_hf_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="raceAsian", data = v_hf_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="raceWhite", data = v_hf_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="agegroup64+", data = v_hf_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="agegroup40-64", data = v_hf_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="agegroup25-39", data = v_hf_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin and ferritin", Outcome = "Ferritin", Variable="agegroup15-24", data = v_hf_f_rmspe),
  
  # US: predict Hb with index Hb only
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="time_to_fu_quantilesq4", data = v_h_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="time_to_fu_quantilesq3", data = v_h_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="time_to_fu_quantilesq2", data = v_h_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="time_to_fu_quantilesq1", data = v_h_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="sexF", data = v_h_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="sexM", data = v_h_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="raceHispanic", data = v_h_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="raceBlack", data = v_h_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="raceAsian", data = v_h_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="raceWhite", data = v_h_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="agegroup64+", data = v_h_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="agegroup40-64", data = v_h_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="agegroup25-39", data = v_h_f_rmspe),
  list(Country = "US", Dataset = "Hemoglobin only", Outcome = "Ferritin", Variable="agegroup15-24", data = v_h_f_rmspe)
)

# Loop through the datasets to update the table
for (ds in external_datasets) {
  rmspe_subgroup_dt[
    Country == ds$Country & Dataset == ds$Dataset & Outcome == ds$Outcome & Variable == ds$Variable,
    RMSPE := get_rmspe_subgroup(ds$data, ds$Variable)
  ]
}

# Add NL results back
rmspe_subgroup_dt_sq <- fread("./3_intermediate/subgroup_performance/rmspe_subgroup_sq.csv")

# combine rmspe result table
rmspe_subgroup_dt <- rbind(rmspe_subgroup_dt, rmspe_subgroup_dt_sq)

# Recode values for outcome and subgroup variable  
rmspe_subgroup_dt[, Outcome := ifelse(Outcome=="Ferritin", "Log10 Ferritin", Outcome)]
rmspe_subgroup_dt[, Variable := ifelse(Variable=="time_to_fu_quantilesq1", "Time to return Q1 (shortest)", 
                                       ifelse(Variable=="time_to_fu_quantilesq2", "Time to return Q2", 
                                              ifelse(Variable=="time_to_fu_quantilesq3", "Time to return Q3",
                                                     ifelse(Variable=="time_to_fu_quantilesq4", "Time to return Q4 (longest)", Variable))))]


# add vertical lines representing overall RMSPE by country
overall_rmspe <- data.frame(
  Country = c(rep("US", 4),
              rep("SA", 4),
              rep("NL", 4)),
  Dataset = c(rep(c("Hemoglobin only", "Hemoglobin only", "Hemoglobin and ferritin", "Hemoglobin and ferritin"), 3)),
  Outcome = c(rep(c("Hemoglobin", "Log10 Ferritin", "Hemoglobin", "Log10 Ferritin"), 3)),
  xintercept = c(7.19, 22.92, 4.59, 14.88,
                 7.3, 27.55, 6.65, 18.94,
                 5.87, 19.09, 6.27, 15.06) # Example positions for vertical lines
)


forest_plt_rmspe <- ggplot(data = rmspe_subgroup_dt, 
                     aes(x = RMSPE, 
                         y = Variable, color = Country, shape = Country))+
  scale_color_manual(values=c("US"="#abdda4", "SA"="#56B4E9", "NL"="orange"))+
  facet_grid(cols = vars(Dataset), rows = vars(Outcome))+
  geom_vline(data = overall_rmspe, aes(xintercept = xintercept, color = Country), linetype = "dashed") +
  geom_point(aes(y=Variable), size=2, position = position_dodge(width = 0.7))+
  theme_bw()+
  labs(y="", x="Root Mean Squared Percent Error (RMSPE)", color = "Country", shape = "Country")+
  theme(
    legend.position = "bottom",
    legend.box = "vertical", # Stacks the legends vertically
    legend.spacing.y = unit(0.5, "lines"), # Adds space between the two legends
    strip.text = element_text(size = 10),
    axis.text = element_text(size = 9)
  ) 
 
forest_plt_rmspe

top_label <- textGrob("Baseline biomarkers included", hjust=0.2)
new_forest_plt_rmspe <- grid.arrange(forest_plt_rmspe,top=top_label, right='Follow-up outcome predicted')

new_forest_plt_rmspe

# Open PNG device with desired size and resolution
png(filename = "./4_output/forest_rmspe.png", width = 7.8, height = 6, units = "in", res = 300)
grid.draw(new_forest_plt_rmspe)
dev.off()

svg(filename = "./4_output/forest_rmspe.svg", width = 7.8, height = 6)
grid.draw(new_forest_plt_rmspe)
dev.off()

