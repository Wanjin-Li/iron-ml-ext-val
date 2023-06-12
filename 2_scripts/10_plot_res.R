# Plotting validation results for RISE (internal validation), SANBS, Vitalant, Sanquin

library(ggplot2)
library(gridExtra)  # for arranging plots

library(tidyverse)
library(stringr)
source('https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R')
source("./2_scripts/utility_functions.R")
theme_set(theme_bw()+theme(axis.line = element_line(colour = "black"),
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(),
                           panel.border = element_blank(),
                           panel.background = element_blank()) )

# RMSPE function ----
EPSILON <-  1e-10  # prevent division by zero
get_rmspe <- function(df) {
  
  ########################################### to del
  df <- na.omit(df)
  df <- df[df$fu_outcome != 0, ]
  ###########################################
  # print(nrow(df))
  r<-(sqrt(mean(((df$fu_outcome - df$prediction) / (df$fu_outcome + EPSILON))**2))) * 100
  return(r)
}

# RMSPE function (getting average over folds) ----
get_rmspe_over_repeats_folds <- function(df) {
  num_repeats <- length(unique(df$rpt))
  num_folds <- length(unique(df$fold))
  
  sum_rmspe <- 0
  for (i in 1:num_repeats) {
    for (j in 1:num_folds) {
      sub_df <- df[df$rpt == i & df$fold == j]
      rmspe <- get_rmspe(sub_df)
      sum_rmspe <- sum_rmspe + rmspe
    }
  }
  avg_rmspe <- sum_rmspe / (num_repeats * num_folds)
  return(avg_rmspe)
}


versions <- c("hgb_ferr_predict_hgb", "hgb_only_predict_hgb", "hgb_ferr_predict_ferr", "hgb_only_predict_ferr")

# RISE (internal validation - cross validation outer fold assessment) ----
version_pattern <- "top_model_assess*"
rise_res_paths <- list.files(path="./3_intermediate/ensemble/", pattern=version_pattern, full.names = TRUE) 

for (path in rise_res_paths) {
  model_res_filename <- gsub("^.*/", "", path)  # gets the last element after splitting by "/"
  if (grepl(versions[1], model_res_filename, fixed = TRUE)) {
    r_hf_h <- fread(path)
    r_hf_h_rmspe <- get_rmspe_over_repeats_folds(r_hf_h)
    
  } else if (grepl(versions[2], model_res_filename, fixed = TRUE)) {
    r_h_h <- fread(path)
    r_h_h_rmspe <- get_rmspe_over_repeats_folds(r_h_h)
    
  } else if (grepl(versions[3], model_res_filename, fixed = TRUE)) {
    r_hf_f <- fread(path)
    r_hf_f_rmspe <- get_rmspe_over_repeats_folds(r_hf_f)
    
  } else if (grepl(versions[4], model_res_filename, fixed = TRUE)) {
    r_h_f <- fread(path)
    r_h_f_rmspe <- get_rmspe_over_repeats_folds(r_h_f)
    
  }
  
}


# SANBS ----
for (version in versions) {
  print(version)
  sanbs_model_res_paths <- list.files(path="./3_intermediate/external_validation/sanbs", pattern=version, full.names = TRUE) 
  
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


# Vitalant ----
for (version in versions) {
  print(version)
  vitalant_model_res_paths <- list.files(path="./3_intermediate/external_validation/vitalant", pattern=version, full.names = TRUE) 
  
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

# Sanquin ----  
# Note has NAs in some of the dataframe results so remove these rows for now until Amber reruns the external validation code
for (version in versions) {
  print(version)
  sanquin_model_res_path <- list.files(path="./3_intermediate/external_validation/sanquin", pattern=version, full.names = TRUE) 
  
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

################### todelete
sq_hf_h <- na.omit(sq_hf_h)
sq_hf_h <- sq_hf_h[sq_hf_h$fu_outcome != 0, ]

sq_h_h <- na.omit(sq_h_h)
sq_h_h <- sq_h_h[sq_h_h$fu_outcome != 0, ]

sq_hf_f <- na.omit(sq_hf_f)
sq_hf_f <- sq_hf_f[sq_hf_f$fu_outcome != 0, ]

sq_h_f <- na.omit(sq_h_f)
sq_h_f <- sq_h_f[sq_h_f$fu_outcome != 0, ]
#####################################


# Set up dataframes for plotting ----
cohort <- c("RISE", "Vitalant", "SANBS", "Sanquin")

hgb_ferr_predict_hgb <- data.frame(Cohort  = cohort,
                                   RMSPE = c(r_hf_h_rmspe, v_hf_h_rmspe, s_hf_h_rmspe, sq_hf_h_rmspe),
                                   predict_biomarker = c("Hemoglobin"),
                                   data_version = c("Hemoglobin and Ferritin"))
hgb_only_predict_hgb <- data.frame(Cohort  = cohort,
                                   RMSPE = c(r_h_h_rmspe, v_h_h_rmspe, s_h_h_rmspe, sq_h_h_rmspe),
                                   predict_biomarker = c("Hemoglobin"),
                                   data_version = c("Hemoglobin only"))
hgb_ferr_predict_ferr <- data.frame(Cohort  = cohort,
                                    RMSPE = c(r_hf_f_rmspe, v_hf_f_rmspe, s_hf_f_rmspe, sq_hf_f_rmspe),
                                    predict_biomarker = c("Log10 Ferritin"),
                                    data_version = c("Hemoglobin and Ferritin"))
hgb_only_predict_ferr <- data.frame(Cohort  = cohort,
                                    RMSPE = c(r_h_f_rmspe, v_h_f_rmspe, s_h_f_rmspe, sq_h_f_rmspe),
                                    predict_biomarker = c("Log10 Ferritin"),
                                    data_version = c("Hemoglobin only"))

main_df <- rbind(hgb_ferr_predict_hgb, hgb_only_predict_hgb, hgb_ferr_predict_ferr, hgb_only_predict_ferr)

# Convert values in the "Cohort" column
main_df <- main_df %>%
  mutate(Cohort = case_when(
    Cohort == "RISE" ~ "Training",
    Cohort == "SANBS" ~ "South Africa",
    Cohort == "Sanquin" ~ "Netherlands",
    Cohort == "Vitalant" ~ "US",
    TRUE ~ Cohort
  ))

# Print the modified dataframe
print(main_df)

main_df$Cohort <- factor(main_df$Cohort, levels = c("Training", "US", "South Africa", "Netherlands"))
main_df$predict_biomarker <- factor(main_df$predict_biomarker, levels = c("Hemoglobin","Log10 Ferritin"))
main_df$data_version <- factor(main_df$data_version, levels = c("Hemoglobin and Ferritin", "Hemoglobin only"))

# Plot FIGURE 3 RMSPE ----


p <- ggplot(main_df, aes(x=Cohort, y=RMSPE, fill=Cohort))+
      geom_bar(stat="identity", color="black",show.legend = FALSE)+
      scale_fill_manual(values=c("tomato", "#abdda4", "#56B4E9", "orange"))+
      scale_y_continuous(expand=c(0,0), limits = c(0,37))+  # set y limit
      facet_grid(predict_biomarker ~ data_version)+
      theme(text = element_text(size = 14),  # increase text size
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),  # rotate
            strip.background = element_rect(colour="black", fill="seashell2", size=1.5, linetype="solid"),  # change facet grid label background color
            strip.text.x = element_text(size = 12, color = "black", face = "bold"),  # facet grid x label
            strip.text.y = element_text(size = 12, color = "black", face = "bold"))  # facet grid y label

p

new_p <- grid.arrange(p,top='Baseline biomarkers included', right='Follow-up outcome predicted')

fname_svg <- paste0("./4_output/figs/", "fig3.svg")
fname_png <- paste0("./4_output/figs/", "fig3.png")
ggsave(plot = new_p, fname_svg, width = 6, height = 5.5, unit = "in")
ggsave(plot=new_p, fname_png, width = 6, height = 5.5, unit = "in")







# add Can you add "% increase" on top of the blue and green bars? Like "+6%" "-2%"? ----

compute_percentage_change_in_rmspe <- function(base, new) {
  #print(new)
  return( round( (100 * (new - base) / base ), 1))
  
}

# hgb_ferr - Predict ferritin  
base <- r_hf_f_rmspe

new <- v_hf_f_rmspe; compute_percentage_change_in_rmspe(base, new)
new <- s_hf_f_rmspe; compute_percentage_change_in_rmspe(base, new)
new <- sq_hf_f_rmspe; compute_percentage_change_in_rmspe(base, new)

# hgb_only - Predict ferritin 
base <- r_h_f_rmspe

new <- v_h_f_rmspe; compute_percentage_change_in_rmspe(base, new)
new <- s_h_f_rmspe; compute_percentage_change_in_rmspe(base, new)
new <- sq_h_f_rmspe; compute_percentage_change_in_rmspe(base, new)

# hgb_ferr - Predict hemoglobin  
base <- r_hf_h_rmspe

new <- v_hf_h_rmspe; compute_percentage_change_in_rmspe(base, new)
new <- s_hf_h_rmspe; compute_percentage_change_in_rmspe(base, new)
new <- sq_hf_h_rmspe; compute_percentage_change_in_rmspe(base, new)

# hgb_only - Predict hemoglobin 
base <- r_h_h_rmspe
new <- v_h_h_rmspe; compute_percentage_change_in_rmspe(base, new)
new <- s_h_h_rmspe; compute_percentage_change_in_rmspe(base, new)
new <- sq_h_h_rmspe; compute_percentage_change_in_rmspe(base, new)

stop()







# Subgroup performance: age, sex, race ----
#####################################
# Age: 15-24, 25-39, 40-64, 65+
# Race: Asian, Black, White
# Sex: Male, Female
age_race_inner <- function(df, age_lower, age_upper, race, model_pred_df) {
  idx <- which((df$age >= age_lower) & (df$age <= age_upper) & (df$race == race))  # subset to get rows indices
  rmspe_subgroup <- model_pred_df[idx, ]  # subset model predictions df by these index rows
  res <- get_rmspe(rmspe_subgroup)  # compute rmspe on these rows
  print(round(res, 1))
}
age_race_all_races <- function(df, age_lower, age_upper, races, model_pred_df) {
  #print(table(df[df$race %in% races,]$race))
  idx <- which((df$age >= age_lower) & (df$age <= age_upper) & (df$race %in% races))  # subset to get rows indices
  rmspe_subgroup <- model_pred_df[idx, ]  # subset model predictions df by these index rows
  res <- get_rmspe(rmspe_subgroup)  # compute rmspe on these rows
  print(round(res, 1))
}

age_sex_inner <- function(df, age_lower, age_upper, sex, model_pred_df) {
  idx <- which((df$age >= age_lower) & (df$age <= age_upper) & (df$sex == sex))  # subset to get rows indices
  rmspe_subgroup <- model_pred_df[idx, ]  # subset model predictions df by these index rows
  res <- get_rmspe(rmspe_subgroup)  # compute rmspe on these rows
  print(round(res, 1))
}

age_sex_all_sexes <- function(df, age_lower, age_upper, sexes, model_pred_df) {
  #print(table(df[df$race %in% races,]$race))
  idx <- which((df$age >= age_lower) & (df$age <= age_upper) & (df$sex %in% sexes))  # subset to get rows indices
  rmspe_subgroup <- model_pred_df[idx, ]  # subset model predictions df by these index rows
  res <- get_rmspe(rmspe_subgroup)  # compute rmspe on these rows
  print(round(res, 1))
}





# prep dataframes with race column by adding race back in
hgb_ferr_var <- c("DonorID",
                  "Visit_Date",
                  "rbc_loss_last_12_months",
                  "rbc_loss_last_24_months",
                  "days_since_last_rbc_loss",
                  "days_since_last_drbc_loss",
                  "cum_lifetime_donations",
                  "index_hgb",
                  "blood_type",
                  "age",
                  "sex",
                  "index_ferritin",
                  "index_log_ferritin",
                  "time_to_fu",
                  "fu_hgb",
                  "fu_log_ferritin")
hgb_only_var <- c("DonorID",
                  "Visit_Date",
                  "rbc_loss_last_12_months",
                  "rbc_loss_last_24_months",
                  "days_since_last_rbc_loss",
                  "days_since_last_drbc_loss",
                  "cum_lifetime_donations",
                  "index_hgb",
                  "blood_type",
                  "age",
                  "sex",
                  "time_to_fu",
                  "fu_hgb",
                  "fu_log_ferritin")
hgb_ferr_var_race <-  c(hgb_ferr_var, "race")
hgb_only_var_race <-  c(hgb_only_var, "race")

# sanbs
s_df <- fread("./3_intermediate/private/sanbs_intermediate_to_del4.csv")
s_df <- s_df[,hgb_ferr_var_race]

# s_df$index_hgb <- as.numeric(s_df$index_hgb)
# s_df$fu_hgb <- as.numeric(s_df$fu_hgb)

s_hgb_ferr <- fread("./3_intermediate/private/hgb_ferr_sanbs.csv")
s_hgb_only <- fread("./3_intermediate/private/hgb_only_sanbs.csv")

# add race back in
s_hgb_ferr <- data.frame(merge(x=s_hgb_ferr, y=s_df, by=hgb_ferr_var, all.x=TRUE))
s_hgb_only <- data.frame(merge(x=s_hgb_only, y=s_df, by=hgb_only_var, all.x=TRUE))


dfs <- list(s_hgb_ferr, s_hgb_only, s_hgb_ferr, s_hgb_only)
model_pred_dfs <- list(s_hf_f, s_h_f, s_hf_h, s_h_h)

for (i in 1:4) {
  df <- dfs[[i]]
  model_pred_df <- model_pred_dfs[[i]]
  
  # age_sex
  for (sex_category in c("M", "F")) {
    print(paste0(sex_category, ":"))
    age_sex_inner(df, 65, 200, sex_category, model_pred_df)
    age_sex_inner(df, 40, 64, sex_category, model_pred_df)
    age_sex_inner(df, 25, 39, sex_category, model_pred_df)
    age_sex_inner(df, 15, 24, sex_category, model_pred_df)
    #print("--")
    age_sex_inner(df, 15, 200, sex_category, model_pred_df)
    print("----")
  }
  
  age_sex_all_sexes(df, 65, 200, sexes=c("M", "F"), model_pred_df)
  age_sex_all_sexes(df, 40, 64, sexes=c("M", "F"), model_pred_df)
  age_sex_all_sexes(df, 25, 39, sexes=c("M", "F"), model_pred_df)
  age_sex_all_sexes(df, 15, 24, sexes=c("M", "F"), model_pred_df)
  #print("All:")
  age_sex_all_sexes(df, 15, 200,sexes=c("M", "F"), model_pred_df)
  
  # age_race
  for (race_category in c("Asian", "Black", "White")) {
    print(paste0(race_category, ":"))
    age_race_inner(df, 65, 200, race_category, model_pred_df)
    age_race_inner(df, 40, 64, race_category, model_pred_df)
    age_race_inner(df, 25, 39, race_category, model_pred_df)
    age_race_inner(df, 15, 24, race_category, model_pred_df)
    # print("--")
    age_race_inner(df, 15, 200, race_category, model_pred_df)
    print("----")
  }
  age_race_all_races(df, 65, 200, races=c("Asian", "Black", "White"), model_pred_df)
  age_race_all_races(df, 40, 64, races=c("Asian", "Black", "White"), model_pred_df)
  age_race_all_races(df, 25, 39, races=c("Asian", "Black", "White"), model_pred_df)
  age_race_all_races(df, 15, 24, races=c("Asian", "Black", "White"), model_pred_df)
  # print("All:")
  age_race_all_races(df, 15, 200,races=c("Asian", "Black", "White"), model_pred_df)
  
 
  print("=================================")
}

# copy and paste output in Console to excel and find/replace [1] with empty string then change font to Calibri and color to black



# Vitalant
v_df <- data.frame(fread("./3_intermediate/private/vitalant_intermediate_to_del6.csv"))
v_df <- v_df[,hgb_ferr_var_race]

v_hgb_ferr <- fread("./3_intermediate/private/hgb_ferr_vitalant.csv")  
v_hgb_only <- fread("./3_intermediate/private/hgb_only_vitalant.csv")  

# add race back in
v_hgb_ferr <- data.frame(merge(x=v_hgb_ferr, y=v_df, by=hgb_ferr_var, all.x=TRUE))
v_hgb_only <- data.frame(merge(x=v_hgb_only, y=v_df, by=hgb_only_var, all.x=TRUE))


dfs <- list(v_hgb_ferr, v_hgb_only, v_hgb_ferr, v_hgb_only)
model_pred_dfs <- list(v_hf_f, v_h_f, v_hf_h, v_h_h)

for (i in 1:4) {
  df <- dfs[[i]]
  model_pred_df <- model_pred_dfs[[i]]
  
  # age_sex
  for (sex_category in c("M", "F")) {
    print(paste0(sex_category, ":"))
    age_sex_inner(df, 65, 200, sex_category, model_pred_df)
    age_sex_inner(df, 40, 64, sex_category, model_pred_df)
    age_sex_inner(df, 25, 39, sex_category, model_pred_df)
    age_sex_inner(df, 15, 24, sex_category, model_pred_df)
    #print("--")
    age_sex_inner(df, 15, 200, sex_category, model_pred_df)
    print("----")
  }
  
  age_sex_all_sexes(df, 65, 200, sexes=c("M", "F"), model_pred_df)
  age_sex_all_sexes(df, 40, 64, sexes=c("M", "F"), model_pred_df)
  age_sex_all_sexes(df, 25, 39, sexes=c("M", "F"), model_pred_df)
  age_sex_all_sexes(df, 15, 24, sexes=c("M", "F"), model_pred_df)
  #print("All:")
  age_sex_all_sexes(df, 15, 200,sexes=c("M", "F"), model_pred_df)
  
  # age_race
  for (race_category in c("Asian", "Black", "White")) {
    print(paste0(race_category, ":"))
    age_race_inner(df, 65, 200, race_category, model_pred_df)
    age_race_inner(df, 40, 64, race_category, model_pred_df)
    age_race_inner(df, 25, 39, race_category, model_pred_df)
    age_race_inner(df, 15, 24, race_category, model_pred_df)
    #print("--")
    age_race_inner(df, 15, 200, race_category, model_pred_df)
    print("----")
  }
  age_race_all_races(df, 65, 200, races=c("Asian", "Black", "White"), model_pred_df)
  age_race_all_races(df, 40, 64, races=c("Asian", "Black", "White"), model_pred_df)
  age_race_all_races(df, 25, 39, races=c("Asian", "Black", "White"), model_pred_df)
  age_race_all_races(df, 15, 24, races=c("Asian", "Black", "White"), model_pred_df)
  #print("All:")
  age_race_all_races(df, 15, 200,races=c("Asian", "Black", "White"), model_pred_df)
  
  
  print("=================================")
}




# 
# 
# 
# 
# 
# # Plot 1: Predict Hemoglobin with hgb+ferr
# p1 <- ggplot(hgb_ferr_predict_hgb, aes(x=Cohort, y=RMSPE, fill=Cohort))+
#   geom_bar(stat="identity", color="black")+
#   scale_fill_manual(values=c("tomato","#56B4E9", "#abdda4"))+
#   scale_x_discrete(limits=c("RISE", "Vitalant", "SANBS"))+ 
#   theme(legend.position = "none")   # remove legend
# 
# 
# 
# # Plot 2: Predict Ferritin with hgb+ferr
# p2 <- ggplot(hgb_only_predict_hgb, aes(x=Cohort, y=RMSPE, fill=Cohort))+
#   geom_bar(stat="identity", color="black")+
#   scale_fill_manual(values=c("tomato", "#56B4E9", "#abdda4"))+
#   theme_minimal()+
#   scale_x_discrete(limits=c("RISE", "Vitalant", "SANBS"))+ 
#   theme(legend.position = "none")   
# 
# 
# # Plot 3: Predict Hemoglobin with hgb only
# p3 <- ggplot(hgb_ferr_predict_ferr, aes(x=Cohort, y=RMSPE, fill=Cohort))+
#   geom_bar(stat="identity", color="black")+
#   scale_fill_manual(values=c("tomato", "#56B4E9", "#abdda4"))+
#   theme_minimal()+
#   scale_x_discrete(limits=c("RISE", "Vitalant", "SANBS"))+ 
#   theme(legend.position = "none")   
# 
# 
# # Plot 4: Predict Ferritin with hgb only
# p4 <- ggplot(hgb_only_predict_ferr, aes(x=Cohort, y=RMSPE, fill=Cohort))+
#   geom_bar(stat="identity", color="black")+
#   scale_fill_manual(values=c("tomato", "#56B4E9", "#abdda4"))+
#   theme_minimal()+
#   scale_x_discrete(limits=c("RISE", "Vitalant", "SANBS"))+ 
#   theme(legend.position = "none")   
# 
# 
# 
# # Create a 2 x 2 plotting matrix. The next 4 plots created will be plotted next to each other
# grid.arrange(p1, p2, p3, p4, ncol=2, nrow = 2)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ggplot(data = rate20_all2, aes(x=vaxup, y=diff*328.2*10^6*0.005, ymin=diff_lb*328.2*10^6*0.005,ymax=diff_ub*328.2*10^6*0.005,fill=vaxeff))+
#   geom_bar(position = "dodge",stat = "identity", width = 0.6)+
#   geom_errorbar(position=position_dodge(width=0.6), width=0.2)+
#   ylab("Number of hospitalizations")+
#   xlab("Vaccine Uptake")+
#   labs(title = "Change in the number of hospitalization \n (vs.Historic)", fill="Vaccine Efficacy")+
#   #  coord_cartesian(ylim=c(-0.3,0.3)) +
#   geom_hline(yintercept = 0, linetype = 2) +
#   theme_bw()+
#   theme(
#     panel.spacing.x = unit(0,'mm'),
#     panel.background = element_blank(), axis.line=element_line(colour = "black"),
#     plot.title=element_text(hjust=0.5),
#     legend.title = element_text(size=12),legend.text = element_text(size=12),
#     axis.text.x = element_text(size=11, angle = 30, vjust=0.7),
#     axis.text.y = element_text(size=11),
#     axis.title.x = element_text(size=14),
#     axis.title.y = element_text(size=14))+
#   scale_fill_manual(values = c("seagreen","skyblue","skyblue4","tomato","tomato4"))+
#   scale_y_continuous(expand=c(0,0), limits = c(-5.5*10^5,5.5*10^5))
# # using Kyueun bar plot code that alton sent
















































# 
# # original code
# #Multiclass AUCs (avg pairwise Hand and Till 2001)
# dt_outer_preds_all_repeats <- rbind(
#   cbind(version="withXB",
#         fread("./3_intermediate/top_model_assess_withXB.csv")),
#   cbind(version="noXB",
#         fread("./3_intermediate/top_model_assess_noXB.csv")))
# 
# dt_outer_preds_all_repeats[, is_Z0 := ifelse(fu_outcome=="Z0",1,0)]
# dt_outer_preds_all_repeats[, is_Z1 := ifelse(fu_outcome=="Z1",1,0)]
# dt_outer_preds_all_repeats[, is_Z2 := ifelse(fu_outcome=="Z2",1,0)]
# dt_outer_preds_all_repeats[, is_Z3 := ifelse(fu_outcome=="Z3",1,0)]
# 
# AUCs <- dt_outer_preds_all_repeats[ , list(Overall = multiclass.roc(fu_outcome~Z0+Z1+Z2+Z3)$auc,
#                                            Z0 = roc(is_Z0~Z0)$auc,
#                                            Z1 = roc(is_Z1~Z1)$auc,
#                                            Z2 = roc(is_Z2~Z2)$auc,
#                                            Z3 = roc(is_Z3~Z3)$auc),
#                                     by= c("version", "rpt")]
# 
# 
# AUCs_mean <- AUCs[,lapply(.SD, mean), by=version, .SDcols=c("Overall", paste0("Z",0:3))]
# 
# 
# 
# roc_objects <- list()
# outcomes <- c("None", "HGB_defer", "Low", "Absent")
# 
# for (vsn in c("withXB", "noXB")){
#   for (outcome_num in 0:3){
#     for (rpt_idx in 1:3){
#       response = dt_outer_preds_all_repeats[version== vsn & rpt == rpt_idx,
#                                             fifelse(fu_outcome==paste0("Z",outcome_num),1,0)]
#       predictor = dt_outer_preds_all_repeats[version== vsn & rpt == rpt_idx,
#                                              .SD,
#                                              .SDcols = c(paste0("Z",outcome_num))][[1]]
#       roc_obj <- roc(response = response, predictor = predictor)
#       roc_objects[[vsn]][paste0(outcomes[outcome_num+1],"_",rpt_idx)] <- list(roc_obj)
#     }
#   }
# }
# 
# 
# 
# ROC_1vall_withXB<- ggroc(roc_objects$withXB, aes=c("linetype", "color")) +
#   geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed") +
#   theme(legend.position = "none")+
#   ggtitle("Extra biomarkers")+
#   #guides(col = guide_legend(nrow = 2))+
#   scale_color_manual(
#     values = rep(c("#4affff", "#e8c700", "#d97900", "#b80202"),each=3),
#     labels = rep(c("None", "HGB deferral", "Low iron donation", "Absent iron donation"),each=3))+
#   scale_linetype_manual(
#     values = rep(c("solid", "longdash", "dotdash", "twodash"),each=3),
#     labels = rep(c("None", "HGB deferral", "Low iron donation", "Absent iron donation"),each=3))+
#   xlab("Specificity")+ylab("Sensitivity")+
#   geom_point(aes(x=0.75, y=0.75), fill="black", color="black", size = 2)
# 
# # ggsave("ROC_withXB.png",
# #        width = 4.5,
# #        height = 4.5,
# #        unit = "in")
# 
# ROC_1vall_noXB<- ggroc(roc_objects$noXB, aes=c("linetype", "color")) +
#   geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed") +
#   theme(legend.position = "none")+
#   ggtitle("Standard biomarkers")+
#   #guides(col = guide_legend(nrow = 2))+
#   scale_color_manual(
#     values = rep(c("#4affff", "#e8c700", "#d97900", "#b80202"),each=3),
#     labels = rep(c("None", "HGB deferral", "Low iron donation", "Absent iron donation"),each=3))+
#   scale_linetype_manual(
#     values = rep(c("solid", "longdash", "dotdash", "twodash"),each=3),
#     labels = rep(c("None", "HGB deferral", "Low iron donation", "Absent iron donation"),each=3))+
#   xlab("Specificity")+ylab("Sensitivity")+
#   geom_point(aes(x=0.75, y=0.75), fill="black", color="black", size = 2)
# 
# 
# g_legend<-function(a.gplot){
#   tmp <- ggplot_gtable(ggplot_build(a.gplot))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   legend <- tmp$grobs[[leg]]
#   return(legend)
# }
# 
# mylegend<-g_legend(
#   ggplot(data = data.table(Cat=factor(c("No adverse outcome", "HGB deferral", "Low iron donation", "Absent iron donation"),
#                                       levels = c("No adverse outcome", "HGB deferral", "Low iron donation", "Absent iron donation"))))+
#     geom_line(aes(x=Cat, color=Cat, linetype=Cat, y=Cat))+
#     scale_color_manual(values=c("#4affff", "#e8c700", "#d97900", "#b80202"),
#                        name="")+
#     scale_linetype_manual(values=c("solid", "longdash","dotdash", "twodash"),
#                           name="")+
#     guides(color=guide_legend(nrow=1),
#            linetype = guide_legend(nrow=1))
# )
# 
# 
# 
# ggsave("./4_output/figs/ROC_compare.png",
#        plot=grid.arrange(arrangeGrob(ROC_1vall_noXB,ROC_1vall_withXB,nrow=1),
#                          mylegend,nrow=2, heights=c(7,1)),
#        width = 6.5,
#        height = 4,
#        unit = "in")
# ggsave("./4_output/figs/ROC_compare.pdf",
#        plot=grid.arrange(arrangeGrob(ROC_1vall_noXB,ROC_1vall_withXB,nrow=1),
#                          mylegend,nrow=2, heights=c(7,1)),
#        width = 6.5,
#        height = 4,
#        unit = "in")
# 
# 
# 
# #AUCS by fold
# AUCs_by_fold <- dt_outer_preds_all_repeats[ , list(Overall = multiclass.roc(fu_outcome~Z0+Z1+Z2+Z3)$auc,
#                                                    Z0 = roc(is_Z0~Z0)$auc,
#                                                    Z1 = roc(is_Z1~Z1)$auc,
#                                                    Z2 = roc(is_Z2~Z2)$auc,
#                                                    Z3 = roc(is_Z3~Z3)$auc),
#                                             by= c("version", "rpt", "fold")]
# 
# 
# AUCs_by_fold_long <- melt(AUCs_by_fold, id.vars = 1:3,
#                           variable.name = "outcome", value.name = "AUC")
# 
# 
# AUCs_by_fold_mean_CI <- AUCs_by_fold_long[ , list(
#   mean = mean(AUC),
#   lb = mean(AUC) - sd(AUC)/sqrt(.N),
#   ub = mean(AUC) + sd(AUC)/sqrt(.N)),
#   by = c("version","outcome")]
# 
# fwrite(AUCs_by_fold_mean_CI, "./4_output/AUC_results_meanCI.csv")
# 
# 
# ## SAVE AUC AND HYPERPARAMETERS FOr THE TOP MODEL OF EACH TYPE
# #Top by model type and version
# top_mods <- tune_results_all[,.SD[which.max(AUC_mean)],by=.(model,version), .SDcols = c("modelID", "AUC_mean", "AUC_lb", "AUC_ub")]
# top_mods[, AUC_disp := paste0(percent(AUC_mean, accuracy = .01), " (",percent(AUC_lb, accuracy = .01)," - ",percent(AUC_ub, accuracy = .01),")")]
# #top_mods[,  ID := str_split(modelID, "\\.")[[1]][2], by=AUC_mean ]
# top_mods[,
#          hyperparams :=
#            fifelse(modelID=="XGB.770", list(as.list(xgb_param_sets[770,])),
#                    fifelse(modelID=="XGB.109", list(as.list(xgb_param_sets[109,])),
#                            fifelse(modelID=="elastic_net.1", list(as.list(en_param_sets[1,])),
#                                    fifelse(modelID=="elastic_net.2", list(as.list(en_param_sets[2,])),
#                                            fifelse(modelID=="elastic_net_interactions.16", list(as.list(enint_param_sets[16,])),
#                                                    fifelse(modelID=="elastic_net_interactions.4", list(as.list(enint_param_sets[4,])),
#                                                            fifelse(modelID=="random_forest.76", list(as.list(rf_param_sets[76,])),
#                                                                    fifelse(modelID=="random_forest.39", list(as.list(rf_param_sets[39,])),
#                                                                            fifelse(modelID=="rpart.21", list(as.list(rpart_param_sets[21,])),
#                                                                                    list("ERROR"))))))))))
# ]
# 
# 
# 
# saveRDS(top_mods,"./3_intermediate/tuning_results/tuning_results_top_by_modtype_version.rds")
