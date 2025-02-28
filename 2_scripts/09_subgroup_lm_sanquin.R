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

# source('https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R')
# source("./2_scripts/utility_functions.R")
# theme_set(theme_bw()+theme(axis.line = element_line(colour = "black"),
#                            panel.grid.major = element_blank(),
#                            panel.grid.minor = element_blank(),
#                            panel.border = element_blank(),
#                            panel.background = element_blank()) )

# RMSPE function ----
EPSILON <-  1e-10  # prevent division by zero

# get the average rmspe
get_rmspe <- function(df) {
  r<-(sqrt(mean(((df$fu_outcome - df$prediction) / (df$fu_outcome  + EPSILON))**2))) * 100
  return(r)
}

# getrmse
get_rmse <- function(df) {
  
  r<-((df$fu_outcome - df$prediction) / (df$fu_outcome + EPSILON))**2 *100
  return(r)
}

# Performance across subgroups
# Using  a linear regression regressing error on age, sex, race, 
# 2-yr donation history (0, 1-2, 3+ in last 24 months), with interaction term for female and 0 donation history
# function returns coef table
run_lm<-function(df){
  
  #categorise age
  
  df <- df %>% mutate(agegroup = case_when(age >= 15  & age <= 24 ~ '15-24',
                                           age >= 25  & age <= 39 ~ '25-39',
                                           age >= 40  & age <= 64 ~ '40-64',
                                           age >= 64 ~ '64+'))
  #redefining reference categories
  df$sex <- relevel(factor(df$sex), ref = 'M')
  #df$race <- relevel(factor(df$race), ref = 'White') 
  df$agegroup <- relevel(factor(df$agegroup), ref = '15-24') 
  df$time_to_fu_quantiles <- relevel(factor(df$time_to_fu_quantiles), ref = 'q1')
  model <- lm(rmspe ~ agegroup + sex + race + time_to_fu_quantiles, data = df)
  #sex*dons_hist
  
  # model <- lm(rmspe ~ agegroup + sex + time_to_fu + 
  #               I(time_to_fu^2) + I(time_to_fu^3), data = df) # use a polynomial expansion of time with two or three degrees (i.e. FU + FU^2 + FU^3) to address reviewer's comment
  
  coef <- coef(model)
  conf_intervals <- confint(model)
  coef_df <- data.frame(Term = names(coef),
                        Coef= coef,
                        Conf_Lower = conf_intervals[, 1],
                        Conf_Upper = conf_intervals[, 2])
  
  
  return (coef_df)
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



# add column to divide time_to_fu into quartiles
sq_hgb_only$time_to_fu_quantiles <- quant_groups(sq_hgb_only$time_to_fu, 4)
levels(sq_hgb_only$time_to_fu_quantiles)
levels(sq_hgb_only$time_to_fu_quantiles)<-c('q1', 'q2', 'q3', 'q4')

sq_hgb_ferr$time_to_fu_quantiles <- quant_groups(sq_hgb_ferr$time_to_fu, 4)
levels(sq_hgb_ferr$time_to_fu_quantiles)
levels(sq_hgb_ferr$time_to_fu_quantiles)<-c('q1', 'q2', 'q3', 'q4')

#hgb only dataset
## predicting  hemoglobin
sq_hgb_only$rmspe<-get_rmse(sq_h_h) # sq_h_h is the model output of the ensemble model with 5XGB and 1CB; for example, the file name used should be "hgb_only_predict_hgb_5XGB1CB_XGB.4058.rds" and "hgb_only_predict_hgb_5XGB1CB_CB.536.rds"
sq_h_h_lm<-run_lm(sq_hgb_only[c('age', 'sex', 'time_to_fu_quantiles', 'rmspe')])
sq_subgroup_lm<-sq_h_h_lm
colnames(sq_subgroup_lm) <- c('Variable', 'sq_h_h_coef', 'sq_h_h_conf_lower', 'sq_h_h_conf_upper')


## predicting  ferritin
sq_hgb_only$rmspe<-get_rmse(sq_h_f) # sq_h_f is the model output of a single model CB; file: "hgb_only_predict_ferr_1CB_CB.1171.rds"
sq_h_f_lm<-run_lm(sq_hgb_only[c('age', 'sex', 'time_to_fu_quantiles','rmspe')])
sq_subgroup_lm<-cbind(sq_subgroup_lm, sq_h_f_lm[,2:4])
colnames(sq_subgroup_lm)[5:7] <- c( 'sq_h_f_coef', 'sq_h_f_conf_lower', 'sq_h_f_conf_upper')


#hgb and ferr dataset
## predicting hemoglobin
sq_hgb_ferr$rmspe<- get_rmse(sq_hf_h) # sq_hf_h is the model output of the ensemble model with 4XGB and 2CB; for example, files include "hgb_ferr_predict_hgb_4XGB2CB_XGB.4592.rds" and "hgb_ferr_predict_hgb_4XGB2CB_CB.850.rds" 
sq_hf_h_lm<-run_lm(sq_hgb_ferr[c('age', 'sex', 'time_to_fu_quantiles','rmspe')])
sq_subgroup_lm<-cbind(sq_subgroup_lm, sq_hf_h_lm[,2:4])
colnames(sq_subgroup_lm)[8:10] <- c( 'sq_hf_h_coef', 'sq_hf_h_conf_lower', 'sq_hf_h_conf_upper')


## predicting ferritin
sq_hgb_ferr$rmspe<-get_rmse(sq_hf_f) #  sq_hf_f is the model output of a single model CB; file: "hgb_ferr_predict_ferr_1CB_CB.1222.rds"
sq_hf_f_lm<-run_lm(sq_hgb_ferr[c('age', 'sex','time_to_fu_quantiles', 'rmspe')])
sq_subgroup_lm<-cbind(sq_subgroup_lm, sq_hf_f_lm[,2:4])
colnames(sq_subgroup_lm)[11:13] <- c( 'sq_hf_f_coef', 'sq_hf_f_conf_lower', 'sq_hf_f_conf_upper')

# export parameter value table to a csv

currdir <- getwd()
currfolder <- "dec_update"
path <- file.path(currdir, currfolder)
folder <- "subgroup_performance"

newpath <- file.path(path, folder)
if(!dir.exists(newpath)){
  dir.create(newpath)
}

sq_filename <- "sq_subgroup_lm_data"

sq_filepath <- paste0(sq_filename, ".csv")
sq_filepath <- file.path(newpath, sq_filepath)

write.csv(sq_subgroup_lm, sq_filepath, row.names = FALSE)


