# This script performs addition analysis 0 for NL
# to obtain group size of each subgroup category stratified by cohorts for additional figure and figure 4 improvement

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


variables <- c(
  "time_to_fu_quantilesq4", "time_to_fu_quantilesq3", "time_to_fu_quantilesq2", "time_to_fu_quantilesq1",
  "sexF", "sexM",
  "raceHispanic", "raceBlack", "raceAsian", "raceWhite",
  "agegroup64+", "agegroup40-64", "agegroup25-39", "agegroup15-24"
)
countries <- c("NL")
datasets <- c("Hemoglobin and ferritin", "Hemoglobin only")
outcomes <- c("Ferritin", "Hemoglobin")

# Create the data.table with all combinations
group_size_dat <- CJ(Variable = variables, 
                     Country = countries, 
                     Dataset = datasets, 
                     Outcome = outcomes)
group_size_dat[, Size:=0]

# Helper function: To create subgroup by donor characteristics
create_subgroup <- function(df){
  
  #categorise age
  
  df <- df %>% mutate(agegroup = case_when(age >= 15  & age <= 24 ~ '15-24',
                                           age >= 25  & age <= 39 ~ '25-39',
                                           age >= 40  & age <= 64 ~ '40-64',
                                           age >= 64 ~ '64+'))
  #redefining reference categories
  df$sex <- relevel(factor(df$sex), ref = 'M')
  # df$race <- relevel(factor(df$race), ref = 'White') # commented out because race is not available in Sanquin
  df$agegroup <- relevel(factor(df$agegroup), ref = '15-24') 
  df$time_to_fu_quantiles <- relevel(factor(df$time_to_fu_quantiles), ref = 'q1')
  
  return(df)
}

# Create subgroups
sq_hgb_ferr <- create_subgroup(sq_hgb_ferr)
sq_hgb_only <- create_subgroup(sq_hgb_only)


for (i in 1:nrow(group_size_dat)){
  obs <- group_size_dat[i]
  if (obs[["Dataset"]] == "Hemoglobin and ferritin"){
    ds <- sq_hgb_ferr
  } else if (obs[["Dataset"]] == "Hemoglobin only"){
    ds <- sq_hgb_only
  } 
  
  if (obs[["Variable"]] == "agegroup15-24"){
    gs <- nrow(ds[ds$agegroup == "15-24",])
  } else if (obs[["Variable"]] == "agegroup25-39"){
    gs <- nrow(ds[ds$agegroup == "25-39",])
  } else if (obs[["Variable"]] == "agegroup40-64"){
    gs <- nrow(ds[ds$agegroup == "40-64",])
  } else if (obs[["Variable"]] == "agegroup64+"){
    gs <- nrow(ds[ds$agegroup == "64+",])
  } else if (obs[["Variable"]] == "sexF"){
    gs <- nrow(ds[ds$sex == "F",])
  } else if (obs[["Variable"]] == "sexM"){
    gs <- nrow(ds[ds$sex == "M",])
  } else if (obs[["Variable"]] == "time_to_fu_quantilesq1"){
    gs <- nrow(ds[ds$time_to_fu_quantiles  == "q1",])
  } else if (obs[["Variable"]] == "time_to_fu_quantilesq2"){
    gs <- nrow(ds[ds$time_to_fu_quantiles  == "q2",])
  } else if (obs[["Variable"]] == "time_to_fu_quantilesq3"){
    gs <- nrow(ds[ds$time_to_fu_quantiles  == "q3",])
  } else if (obs[["Variable"]] == "time_to_fu_quantilesq4"){
    gs <- nrow(ds[ds$time_to_fu_quantiles  == "q4",])
  } else {
    gs <- 0 # for race variable
  }
  
  group_size_dat[i, "Size"] <- gs
}

fwrite(group_size_dat, "...path/subgroup_group_size_sq.csv") # replace with the correct file path
