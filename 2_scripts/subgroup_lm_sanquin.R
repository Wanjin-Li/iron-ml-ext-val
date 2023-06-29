library(ggplot2)
library(gridExtra)  # for arranging plots

library(tidyverse)
library(stringr)

# getrmse
get_rmse <- function(df) {
  
  ########################################### to del
  df <- na.omit(df)
  df <- df[df$fu_outcome != 0, ]
  ###########################################
  # print(nrow(df))
  r<-(sqrt(((df$fu_outcome - df$prediction) / (df$fu_outcome + EPSILON))**2)) * 100
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
  #categorise donation history
  df <- df %>% mutate(don_hist_group = case_when(donation_history == 0 ~ '0',
                                                 donation_history >= 1  & donation_history <= 2 ~ '1-2',
                                                 donation_history >= 3 ~ '3+'))
  
  #redefining reference categories
  df$sex <- relevel(factor(df$sex), ref = 'M')
  df$race <- relevel(factor(df$race), ref = 'White') 
  df$agegroup <- relevel(factor(df$agegroup), ref = '15-24') 
  df$don_hist_group <- relevel(factor(df$don_hist_group), ref = '3+') 
  
  #remove race for sanquin
  
  model <- lm(rmspe ~ agegroup + sex + don_hist_group, data = df)
  #sex*dons_hist
  coef <- coef(model)
  conf_intervals <- confint(model)
  coef_df <- data.frame(Term = names(coef),
                        Coef= coef,
                        Conf_Lower = conf_intervals[, 1],
                        Conf_Upper = conf_intervals[, 2])
  
  
  return (coef_df)
}



################ Sanquin: run linear models ###############

# add column donation_history - donations in past 24 months

sq_hgb_only <- sq_hgb_only %>%
  arrange(Visit_Date) %>%
  group_by(DonorID) %>%
  mutate(donation_history = row_number() - 1)
sq_hgb_ferr <- sq_hgb_ferr %>%
  arrange(Visit_Date) %>%
  group_by(DonorID) %>%
  mutate(donation_history = row_number() - 1)

################ Sanquin: run linear models ###############
#sq_h_h here is a dataframe containg 2 columns outcome and predicted outcome
#sq_hgb_only is the hgb only cohort
#hgb only dataset
## predicting  hemoglobin
sq_hgb_only$rmspe<-get_rmse(sq_h_h)
sq_h_h_lm<-run_lm(sq_hgb_only[c('age', 'sex', 'donation_history', 'rmspe')])
sq_subgroup_lm<-sq_h_h_lm
colnames(sq_subgroup_lm) <- c('Variable', 'sq_h_h_coef', 'sq_h_h_conf_lower', 'sq_h_h_conf_upper')

## predicting  ferritin
sq_hgb_only$rmspe<-get_rmse(sq_h_f)
sq_h_f_lm<-run_lm(sq_hgb_only[c('age', 'sex',  'donation_history', 'rmspe')])
sq_subgroup_lm<-cbind(sq_subgroup_lm, sq_h_f_lm)
colnames(sq_subgroup_lm)[5:7] <- c( 'sq_h_f_coef', 'sq_h_f_conf_lower', 'sq_h_f_conf_upper')

#hgb and ferr dataset
## predicting hemoglobin
sq_hgb_ferr$rmspe<-get_rmse(sq_hf_h)
sq_hf_h_lm<-run_lm(sq_hgb_ferr[c('age', 'sex',  'donation_history', 'rmspe')])
sq_subgroup_lm<-cbind(sq_subgroup_lm, sq_hf_h_lm)
colnames(sq_subgroup_lm)[8:10] <- c( 'sq_hf_h_coef', 'sq_hf_h_conf_lower', 'sq_hf_h_conf_upper')


## predicting ferritin
sq_hgb_ferr$rmspe<-get_rmse(sq_hf_f)
sq_hf_f_lm<-run_lm(sq_hgb_ferr[c('age', 'sex',  'donation_history', 'rmspe')])
sq_subgroup_lm<-cbind(sq_subgroup_lm, sq_hf_f_lm)
colnames(sq_subgroup_lm)[11:13] <- c( 'sq_hf_f_coef', 'sq_hf_f_conf_lower', 'sq_hf_f_conf_upper')


print(sq_subgroup_lm)
