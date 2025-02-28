# This script plots eFigure 9: validation results using root mean squared error (RMSE) 
# for RISE (internal validation - nested cross validation), SANBS, Vitalant, Sanquin.


# Plotting validation results for RISE (internal validation), SANBS, Vitalant, Sanquin

library(ggplot2)
library(gridExtra)  # for arranging plots
library(dvmisc)
library(cowplot) 
library(patchwork)
library(tidyverse)
library(stringr)
source('https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R')
source("./2_scripts/utility_functions.R")
theme_set(theme_bw()+theme(axis.line = element_line(colour = "black"),
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(),
                           panel.border = element_blank(),
                           panel.background = element_blank()) )

# Root mean squared error (RMSE) function ----
EPSILON <-  1e-10  # prevent division by zero
get_rmse <- function(df) {
  e <- sqrt(mean((df$fu_outcome - df$prediction)**2))
  return(e)
}


get_multiplier <- function(df){
  m <- 1/sqrt(mean(df$fu_outcome^2))
}

versions <- c("hgb_ferr_predict_hgb", "hgb_only_predict_hgb", "hgb_ferr_predict_ferr", "hgb_only_predict_ferr")

# RISE (nested cross validation - cross validation outer fold assessment) ----
list_r_hf_h_rmse <- list()
list_r_h_h_rmse <- list()
list_r_hf_f_rmse <- list()
list_r_h_f_rmse <- list()

list_r_hf_h <- list()
list_r_h_h <- list()
list_r_hf_f <- list()
list_r_h_f <- list()

list_r_hf_h_multiplier <- list()
list_r_h_h_multiplier <- list()
list_r_hf_f_multiplier <- list()
list_r_h_f_multiplier <- list()


outer_folds <- c("outer_fold_1", "outer_fold_2", "outer_fold_3")


for (fold in outer_folds){
  for (version in versions) {
    pattern_spec <- c(version, fold)
    rise_model_res_paths <- list.files(path="./3_intermediate/external_validation/updates/rise/nested_model", pattern=paste0(pattern_spec, collapse = ".+"), full.names = TRUE) 
    print(rise_model_res_paths)
    if (length(rise_model_res_paths) == 1) {  # single model
      model_res_filename <- gsub("^.*/", "", rise_model_res_paths)  # gets the last element after splitting by "/"
      
      if (grepl(versions[1], model_res_filename, fixed = TRUE)) {
        r_hf_h <- fread(rise_model_res_paths)
        list_r_hf_h[[fold]] <- r_hf_h
        r_hf_h_rmse <- get_rmse(r_hf_h)
        list_r_hf_h_rmse <- append(list_r_hf_h_rmse, r_hf_h_rmse)
        r_hf_h_multipler <- get_multiplier(r_hf_h)
        
      } else if (grepl(versions[2], model_res_filename, fixed = TRUE)) {
        r_h_h <- fread(rise_model_res_paths)
        list_r_h_h[[fold]] <- r_h_h
        r_h_h_rmse <- get_rmse(r_h_h)
        list_r_h_h_rmse <- append(list_r_h_h_rmse, r_h_h_rmse)
        r_h_h_multipler <- get_multiplier(r_h_h)
        
        
      } else if (grepl(versions[3], model_res_filename, fixed = TRUE)) {
        r_hf_f <- fread(rise_model_res_paths)
        list_r_hf_f[[fold]] <- r_hf_f
        r_hf_f_rmse <- get_rmse(r_hf_f)
        list_r_hf_f_rmse <- append(list_r_hf_f_rmse, r_hf_f_rmse)
        r_hf_f_multipler <- get_multiplier(r_hf_f)
        
        
      } else if (grepl(versions[4], model_res_filename, fixed = TRUE)) {
        r_h_f <- fread(rise_model_res_paths)
        list_r_h_f[[fold]] <- r_h_f
        r_h_f_rmse <- get_rmse(r_h_f)
        list_r_h_f_rmse <- append(list_r_h_f_rmse, r_h_f_rmse)
        r_h_f_multipler <- get_multiplier(r_h_f)
        
      }
    } else {  # ensemble model has more than 1 model result to load
      # add prediction of these models together then get average
      sum_df <- fread(rise_model_res_paths[1])
      sum_df <- sum_df - sum_df  # get empty dataframe
      for (single_model_path in rise_model_res_paths) {  # combine model results
        single_model_res <- fread(single_model_path)
        sum_df <- sum_df + single_model_res
      }
      avg_df <- sum_df / (length(rise_model_res_paths))
      
      if (grepl(versions[1], rise_model_res_paths[1], fixed = TRUE)) {
        r_hf_h <- avg_df
        list_r_hf_h[[fold]] <- r_hf_h
        r_hf_h_rmse <- get_rmse(avg_df)
        list_r_hf_h_rmse <- append(list_r_hf_h_rmse, r_hf_h_rmse)
        
        r_hf_h_multipler <- get_multiplier(r_hf_h)
        list_r_hf_h_multiplier <- append(list_r_hf_h_multiplier, r_hf_h_multipler)
        
        
      } else if (grepl(versions[2], rise_model_res_paths[1], fixed = TRUE)) {
        r_h_h <- avg_df
        list_r_h_h[[fold]] <- r_h_h
        r_h_h_rmse <- get_rmse(avg_df)
        list_r_h_h_rmse <- append(list_r_h_h_rmse, r_h_h_rmse)
        
        r_h_h_multipler <- get_multiplier(r_h_h)
        list_r_h_h_multiplier <- append(list_r_h_h_multiplier, r_h_h_multipler)
        
        
      } else if (grepl(versions[3], rise_model_res_paths[1], fixed = TRUE)) {
        r_hf_f <- avg_df
        list_r_hf_f[[fold]] <- r_hf_f
        r_hf_f_rmse <- get_rmse(avg_df)
        list_r_hf_f_rmse <- append(list_r_hf_f_rmse, r_hf_f_rmse)
        
        r_hf_f_multipler <- get_multiplier(r_hf_f)
        list_r_hf_f_multiplier <- append(list_r_hf_f_multiplier, r_hf_f_multipler)
        
        
      } else if (grepl(versions[4], rise_model_res_paths[1], fixed = TRUE)) {
        r_h_f <- avg_df
        list_r_h_f[[fold]] <- r_h_f
        r_h_f_rmse <- get_rmse(avg_df)
        list_r_h_f_rmse <- append(list_r_h_f_rmse, r_h_f_rmse)
        
        r_h_f_multipler <- get_multiplier(r_h_f)
        list_r_h_f_multiplier <- append(list_r_h_f_multiplier, r_h_f_multipler)
        
        
      }
    }
  }
  
}

# Average rmse of three outer folds
r_hf_h_rmse <- mean(unlist(list_r_hf_h_rmse))
r_h_h_rmse <- mean(unlist(list_r_h_h_rmse))
r_hf_f_rmse <- mean(unlist(list_r_hf_f_rmse))
r_h_f_rmse <- mean(unlist(list_r_h_f_rmse))

# Calculate min and max for rmse values 
min_r_hf_h_rmse <- min(unlist(list_r_hf_h_rmse)) 
max_r_hf_h_rmse <- max(unlist(list_r_hf_h_rmse)) 
min_r_h_h_rmse <- min(unlist(list_r_h_h_rmse)) 
max_r_h_h_rmse <- max(unlist(list_r_h_h_rmse)) 
min_r_hf_f_rmse <- min(unlist(list_r_hf_f_rmse)) 
max_r_hf_f_rmse <- max(unlist(list_r_hf_f_rmse)) 
min_r_h_f_rmse <- min(unlist(list_r_h_f_rmse)) 
max_r_h_f_rmse <- max(unlist(list_r_h_f_rmse))

# Average multiplier of three outer folds
r_hf_h_multiplier <- mean(unlist(list_r_hf_h_multiplier))
r_h_h_multiplier <- mean(unlist(list_r_h_h_multiplier))
r_hf_f_multiplier <- mean(unlist(list_r_hf_f_multiplier))
r_h_f_multiplier <- mean(unlist(list_r_h_f_multiplier))


# SANBS ----

for (version in versions) {
  print(version)
  sanbs_model_res_paths <- list.files(path="./3_intermediate/external_validation/updates/sanbs", pattern=version, full.names = TRUE) 
  
  if (length(sanbs_model_res_paths) == 1) {  # single model
    model_res_filename <- gsub("^.*/", "", sanbs_model_res_paths)  # gets the last element after splitting by "/"
    
    if (grepl(versions[1], model_res_filename, fixed = TRUE)) {
      s_hf_h <- fread(sanbs_model_res_paths)
      s_hf_h_rmse <- get_rmse(s_hf_h)
      
      s_hf_h_multiplier <- get_multiplier(s_hf_h)
      
    } else if (grepl(versions[2], model_res_filename, fixed = TRUE)) {
      s_h_h <- fread(sanbs_model_res_paths)
      s_h_h_rmse <- get_rmse(s_h_h)
      
      s_h_h_multiplier <- get_multiplier(s_h_h)
      
    } else if (grepl(versions[3], model_res_filename, fixed = TRUE)) {
      s_hf_f <- fread(sanbs_model_res_paths)
      s_hf_f_rmse <- get_rmse(s_hf_f)
      
      s_hf_f_multiplier <- get_multiplier(s_hf_f)
      
      
    } else if (grepl(versions[4], model_res_filename, fixed = TRUE)) {
      s_h_f <- fread(sanbs_model_res_paths)
      s_h_f_rmse <- get_rmse(s_h_f)
      
      s_h_f_multiplier <- get_multiplier(s_h_f)
      
      
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
      s_hf_h_rmse <- get_rmse(avg_df)
      
      s_hf_h_multiplier <- get_multiplier(avg_df)
      
    } else if (grepl(versions[2], sanbs_model_res_paths[1], fixed = TRUE)) {
      s_h_h <- avg_df
      s_h_h_rmse <- get_rmse(avg_df)
      
      s_h_h_multiplier <- get_multiplier(avg_df)
      
      
    } else if (grepl(versions[3], sanbs_model_res_paths[1], fixed = TRUE)) {
      s_hf_f <- avg_df
      s_hf_f_rmse <- get_rmse(avg_df)
      
      s_hf_f_multiplier <- get_multiplier(avg_df)
      
      
    } else if (grepl(versions[4], sanbs_model_res_paths[1], fixed = TRUE)) {
      s_h_f <- avg_df
      s_h_f_rmse <- get_rmse(avg_df)
      
      s_h_f_multiplier <- get_multiplier(avg_df)
      
    }
  }
}

# Vitalant ----
for (version in versions) {
  print(version)
  vitalant_model_res_paths <- list.files(path="./3_intermediate/external_validation/updates/vitalant", pattern=version, full.names = TRUE) 
  
  if (length(vitalant_model_res_paths) == 1) {  # single model
    model_res_filename <- gsub("^.*/", "", vitalant_model_res_paths)  # gets the last element after splitting by "/"
    
    if (grepl(versions[1], model_res_filename, fixed = TRUE)) {
      v_hf_h <- fread(vitalant_model_res_paths)
      v_hf_h_rmse <- get_rmse(v_hf_h)
      
      v_hf_h_multiplier <- get_multiplier(v_hf_h)
      
    } else if (grepl(versions[2], model_res_filename, fixed = TRUE)) {
      v_h_h <- fread(vitalant_model_res_paths)
      v_h_h_rmse <- get_rmse(v_h_h)
      
      v_h_h_multiplier <- get_multiplier(v_h_h)
      
    } else if (grepl(versions[3], model_res_filename, fixed = TRUE)) {
      v_hf_f <- fread(vitalant_model_res_paths)
      v_hf_f_rmse <- get_rmse(v_hf_f)
      
      v_hf_f_multiplier <- get_multiplier(v_hf_f)
      
    } else if (grepl(versions[4], model_res_filename, fixed = TRUE)) {
      v_h_f <- fread(vitalant_model_res_paths)
      v_h_f_rmse <- get_rmse(v_h_f)
      
      v_h_f_multiplier <- get_multiplier(v_h_f)
      
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
      v_hf_h_rmse <- get_rmse(avg_df)
      
      v_hf_h_multiplier <- get_multiplier(avg_df)
      
      
    } else if (grepl(versions[2], vitalant_model_res_paths[1], fixed = TRUE)) {
      v_h_h <- avg_df
      v_h_h_rmse <- get_rmse(avg_df)
      
      v_h_h_multiplier <- get_multiplier(avg_df)
      
      
    } else if (grepl(versions[3], vitalant_model_res_paths[1], fixed = TRUE)) {
      v_hf_f <- avg_df
      v_hf_f_rmse <- get_rmse(avg_df)
      
      v_hf_f_multiplier <- get_multiplier(avg_df)
      
      
    } else if (grepl(versions[4], vitalant_model_res_paths[1], fixed = TRUE)) {
      v_h_f <- avg_df
      v_h_f_rmse <- get_rmse(avg_df)
      
      v_h_f_multiplier <- get_multiplier(avg_df)
      
      
    }
  }
}

# Sanquin ----  
for (version in versions) {
  print(version)
  sanquin_model_res_path <- list.files(path="./3_intermediate/external_validation/updates/sanquin", pattern=version, full.names = TRUE) 
  
  if (length(sanquin_model_res_path) == 1) {  # single model
    model_res_filename <- gsub("^.*/", "", sanquin_model_res_path)  # gets the last element after splitting by "/"
    
    if (grepl(versions[1], model_res_filename, fixed = TRUE)) {
      sq_hf_h <- fread(sanquin_model_res_path)
      sq_hf_h_rmse <- get_rmse(sq_hf_h)
      
      sq_hf_h_multiplier <- get_multiplier(sq_hf_h)
      
    } else if (grepl(versions[2], model_res_filename, fixed = TRUE)) {
      sq_h_h <- fread(sanquin_model_res_path)
      sq_h_h_rmse <- get_rmse(sq_h_h)
      
      sq_h_h_multiplier <- get_multiplier(sq_h_h)
      
    } else if (grepl(versions[3], model_res_filename, fixed = TRUE)) {
      sq_hf_f <- fread(sanquin_model_res_path)
      sq_hf_f_rmse <- get_rmse(sq_hf_f)
      
      sq_hf_f_multiplier <- get_multiplier(sq_hf_f)
      
    } else if (grepl(versions[4], model_res_filename, fixed = TRUE)) {
      sq_h_f <- fread(sanquin_model_res_path)
      sq_h_f_rmse <- get_rmse(sq_h_f)
      
      sq_h_f_multiplier <- get_multiplier(sq_h_f)
      
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
      sq_hf_h_rmse <- get_rmse(avg_df)
      
      sq_hf_h_multiplier <- get_multiplier(avg_df)
      
    } else if (grepl(versions[2], sanquin_model_res_path[1], fixed = TRUE)) {
      sq_h_h <- avg_df
      sq_h_h_rmse <- get_rmse(avg_df)
      
      sq_h_h_multiplier <- get_multiplier(avg_df)
      
    } else if (grepl(versions[3], sanquin_model_res_path[1], fixed = TRUE)) {
      sq_hf_f <- avg_df
      sq_hf_f_rmse <- get_rmse(avg_df)
      
      sq_hf_f_multiplier <- get_multiplier(avg_df)
      
    } else if (grepl(versions[4], sanquin_model_res_path[1], fixed = TRUE)) {
      sq_h_f <- avg_df
      sq_h_f_rmse <- get_rmse(avg_df)
      
      sq_h_f_multiplier <- get_multiplier(avg_df)
      
    }
  }
}

# Set up dataframes for plotting ----
cohort <- c("RISE", "Vitalant", "SANBS", "Sanquin")


hgb_ferr_predict_hgb <- data.frame(Cohort  = cohort,
                                   RMSE = c(r_hf_h_rmse, v_hf_h_rmse, s_hf_h_rmse, sq_hf_h_rmse),
                                   ymin = c(min_r_hf_h_rmse, NA, NA, NA),
                                   ymax = c(max_r_hf_h_rmse, NA, NA, NA),
                                   multiplier = c(r_hf_h_multiplier, v_hf_h_multiplier, s_hf_h_multiplier, sq_hf_h_multiplier),
                                   predict_biomarker = c("Hemoglobin"),
                                   data_version = c("Hemoglobin and Ferritin"))
hgb_only_predict_hgb <- data.frame(Cohort  = cohort,
                                   RMSE = c(r_h_h_rmse, v_h_h_rmse, s_h_h_rmse, sq_h_h_rmse),
                                   ymin = c(min_r_h_h_rmse, NA, NA, NA),
                                   ymax = c(max_r_h_h_rmse, NA, NA, NA),
                                   multiplier = c(r_h_h_multiplier, v_h_h_multiplier, s_h_h_multiplier, sq_h_h_multiplier),
                                   predict_biomarker = c("Hemoglobin"),
                                   data_version = c("Hemoglobin only"))
hgb_ferr_predict_ferr <- data.frame(Cohort  = cohort,
                                    RMSE = c(r_hf_f_rmse, v_hf_f_rmse, s_hf_f_rmse, sq_hf_f_rmse),
                                    ymin = c(min_r_hf_f_rmse, NA, NA, NA),
                                    ymax = c(max_r_hf_f_rmse, NA, NA, NA),
                                    multiplier = c(r_hf_f_multiplier, v_hf_f_multiplier, s_hf_f_multiplier, sq_hf_f_multiplier),
                                    
                                    predict_biomarker = c("Log10 Ferritin"),
                                    data_version = c("Hemoglobin and Ferritin"))
hgb_only_predict_ferr <- data.frame(Cohort  = cohort,
                                    RMSE = c(r_h_f_rmse, v_h_f_rmse, s_h_f_rmse, sq_h_f_rmse),
                                    ymin = c(min_r_h_f_rmse, NA, NA, NA),
                                    ymax = c(max_r_h_f_rmse, NA, NA, NA),
                                    multiplier = c(r_h_f_multiplier, v_h_f_multiplier, s_h_f_multiplier, sq_h_f_multiplier),
                                    
                                    predict_biomarker = c("Log10 Ferritin"),
                                    data_version = c("Hemoglobin only"))

main_df <- rbind(hgb_ferr_predict_hgb, hgb_only_predict_hgb, hgb_ferr_predict_ferr, hgb_only_predict_ferr)
print(main_df)

# Convert values in the "Cohort" column
main_df <- main_df %>%
  mutate(Cohort = case_when(
    Cohort == "RISE" ~ "Nested CV",
    Cohort == "SANBS" ~ "SA",
    Cohort == "Sanquin" ~ "NL",
    Cohort == "Vitalant" ~ "US",
    TRUE ~ Cohort
  ))

# Print the modified dataframe
print(main_df)

main_df$Cohort <- factor(main_df$Cohort, levels = c("Nested CV","US", "SA", "NL"))
main_df$predict_biomarker <- factor(main_df$predict_biomarker, levels = c("Hemoglobin","Log10 Ferritin"))
main_df$data_version <- factor(main_df$data_version, levels = c("Hemoglobin and Ferritin", "Hemoglobin only"))

# Add "% increase" on top of the bars

compute_percentage_change_in_rmse <- function(base, new) {
  #print(new)
  
  res <- round( (100 * (new - base) / base ), 1)
  return(res)
  
}

percent_increase <- c(NA)


# hgb_ferr - Predict hemoglobin  

base <- r_hf_h_rmse

new <- v_hf_h_rmse; res <- compute_percentage_change_in_rmse(base, new); percent_increase <- append(percent_increase, res)
new <- s_hf_h_rmse; res <- compute_percentage_change_in_rmse(base, new); percent_increase <- append(percent_increase, res)
new <- sq_hf_h_rmse; res <- compute_percentage_change_in_rmse(base, new); percent_increase <- append(percent_increase, res)

percent_increase <- append(percent_increase, NA)

# hgb_only - Predict hemoglobin 

base <- r_h_h_rmse

new <- v_h_h_rmse; res <- compute_percentage_change_in_rmse(base, new); percent_increase <- append(percent_increase, res)
new <- s_h_h_rmse; res <- compute_percentage_change_in_rmse(base, new); percent_increase <- append(percent_increase, res)
new <- sq_h_h_rmse; res <- compute_percentage_change_in_rmse(base, new); percent_increase <- append(percent_increase, res)

percent_increase <- append(percent_increase, NA)

# hgb_ferr - Predict ferritin 

base <- r_hf_f_rmse

new <- v_hf_f_rmse; res <- compute_percentage_change_in_rmse(base, new); percent_increase <- append(percent_increase, res)
new <- s_hf_f_rmse; res <- compute_percentage_change_in_rmse(base, new); percent_increase <- append(percent_increase, res)
new <- sq_hf_f_rmse; res <- compute_percentage_change_in_rmse(base, new); percent_increase <- append(percent_increase, res)

percent_increase <- append(percent_increase, NA)

# hgb_only - Predict ferritin 

base <- r_h_f_rmse

new <- v_h_f_rmse; res <- compute_percentage_change_in_rmse(base, new); percent_increase <- append(percent_increase, res)
new <- s_h_f_rmse; res <- compute_percentage_change_in_rmse(base, new); percent_increase <- append(percent_increase, res)
new <- sq_h_f_rmse; res <- compute_percentage_change_in_rmse(base, new); percent_increase <- append(percent_increase, res)


percent_increase
str_percent_increase <- c()

for (i in percent_increase) {
  if (is.na(i)) {
    str_percent_increase <- append(str_percent_increase, i)
    
  } else if (i >= 0) {
    res <- paste("+", i, "%", sep="")
    str_percent_increase <- append(str_percent_increase, res)
  } else if (i <= 0) {
    res <- paste(i, "%", sep="")
    str_percent_increase <- append(str_percent_increase, res)
  }
}

main_df$str_percent_increase <- str_percent_increase
stop()


# Plot eFIGURE 9 RMSE ----

# Create a plot function to set y limits separately for each facet 
main_df_hemoglobin <- main_df %>% filter(predict_biomarker=="Hemoglobin")
main_df_ferritin <- main_df %>% filter(predict_biomarker=="Log10 Ferritin")


y_limits_predict_hgb <- c(0, 1.5)
y_limits_predict_ferr <- c(0, 0.5)


plot_facet <- function(data, y_limits, show_facet_lables=TRUE, hide_x_axis_label=FALSE, hide_y_axis_lable=FALSE) { 
  p <- ggplot(data, aes(x = Cohort, y = RMSE, fill = Cohort)) + 
    geom_bar(stat = "identity", color = "black", show.legend = FALSE) + 
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, position = position_dodge(width = 0.9), na.rm = TRUE) + 
    scale_fill_manual(values = c("tomato", "#abdda4", "#56B4E9", "orange")) + 
    scale_y_continuous(expand = c(0, 0), limits = y_limits) + 
    facet_grid(predict_biomarker ~ data_version) + 
    geom_text(aes(label = str_percent_increase), color = "black", position = position_dodge(width = 0.9), vjust = -0.25, show.legend = FALSE) + 
    geom_text(aes(label = round(RMSE, 2)), color = "white", position = position_dodge(width = 0.9), vjust = 3, show.legend = FALSE) + 
    theme(text = element_text(size = 14), # increase text size 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), # rotate 
          strip.background = element_rect(colour = "black", fill = "seashell2", size = 1.5, linetype = "solid"), # change facet grid label background color 
          strip.text.x = element_text(size = 11, color = "black", face = "bold"), # facet grid x label 
          strip.text.y = element_text(size = 12, color = "black", face = "bold"), # facet grid y label 
          axis.title.y = element_blank()) 
  if (!show_facet_lables){
    p <- p + theme(strip.text.x = element_blank(),
                   strip.background.x  = element_blank())
  }
  if (hide_x_axis_label){
    p <- p + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  
  return(p)
} 


# Create separate plots for Hemoglobin and Log10 Ferritin with different y limits 
hemoglobin_plot <- plot_facet(main_df_hemoglobin, y_limits_predict_hgb, hide_x_axis_label = TRUE) 
ferritin_plot <- plot_facet(main_df_ferritin, y_limits_predict_ferr, show_facet_lables = FALSE) 

# Combine the two plots using cowplot::plot_grid 

# Use cowplot to add a common y-axis label
y_axis_label <- ggdraw() + draw_label("RMSE", angle = 90, x = 0.5, y = 0.5, vjust = 0.5, size = 12)

# Combine the plots using patchwork and ensure equal widths
combined_plot <- (hemoglobin_plot / ferritin_plot) + 
  theme(plot.tag.position = 'left', plot.tag = element_text(size = 14))

# Add the y-axis label and ensure proper alignment using cowplot
final_plot <- plot_grid(y_axis_label, combined_plot, ncol = 2, rel_widths = c(0.05, 1))

final_plot_2 <- grid.arrange(final_plot, top="Baseline biomarkers included", right="Follow-up outcome predicted")
final_plot_2


fname_svg <- paste0("./4_output/", "fig3_RMSE.svg") 
fname_png <- paste0("./4_output/", "fig3_RMSE.png")

ggsave(plot = final_plot_2, fname_svg, width = 6, height = 5.5, unit = "in")
ggsave(plot = final_plot_2, fname_png, width = 6, height = 5.5, unit = "in")


# Bootstrap to calculate 95% CI for RMSE percent change ----

bootstrap_rmse_ci_dt <- function(base_rmse, cohort_data, n_boot=1000, seed=12345){
  set.seed(seed)
  
  # Number of rows in external cohort data
  n <- nrow(cohort_data)
  
  # Perform bootstrapping and compute RMSE 
  bootstrap_results <- replicate(n_boot, {
    
    # Resample external cohort with replacement
    resample_indices <- sample(1:n, size=n, replace = TRUE)
    ext_cohort_sample <- cohort_data[resample_indices]
    
    # Calculate RMSE for resampled data
    ext_cohort_rmse <- get_rmse(ext_cohort_sample)
    
    # Calculate percent change
    compute_percentage_change_in_rmse(base_rmse, ext_cohort_rmse)
    
  })
  
  # Compute mean and confidence intervals
  mean_change <- mean(bootstrap_results)
  ci <- quantile(bootstrap_results, probs = c(0.025, 0.975))
  
  # Return results
  list(mean = mean_change, ci_lower=ci[1], ci_upper=ci[2])
  
}


## Predicting hemoglobin with hemoglobin and ferritin 

# Perform bootstrapping for each cohort
s_hf_h_bootstrap_rmse <- bootstrap_rmse_ci_dt(r_hf_h_rmse, s_hf_h)
v_hf_h_bootstrap_rmse <- bootstrap_rmse_ci_dt(r_hf_h_rmse, v_hf_h)
sq_hf_h_bootstrap_rmse <- bootstrap_rmse_ci_dt(r_hf_h_rmse, sq_hf_h)

## Predicting hemoglobin with hemoglobin only 

# Perform bootstrapping for each cohort
s_h_h_bootstrap_rmse <- bootstrap_rmse_ci_dt(r_h_h_rmse, s_h_h)
v_h_h_bootstrap_rmse <- bootstrap_rmse_ci_dt(r_h_h_rmse, v_h_h)
sq_h_h_bootstrap_rmse <- bootstrap_rmse_ci_dt(r_h_h_rmse, sq_h_h)

## Predicting ferritin with hemoglobin and ferritin 

# Perform bootstrapping for each cohort
s_hf_f_bootstrap_rmse <- bootstrap_rmse_ci_dt(r_hf_f_rmse, s_hf_f)
v_hf_f_bootstrap_rmse <- bootstrap_rmse_ci_dt(r_hf_f_rmse, v_hf_f)
sq_hf_f_bootstrap_rmse <- bootstrap_rmse_ci_dt(r_hf_f_rmse, sq_hf_f)

## Predicting ferritin with hemoglobin only

# Perform bootstrapping for each cohort
s_h_f_bootstrap_rmse <- bootstrap_rmse_ci_dt(r_h_f_rmse, s_h_f)
v_h_f_bootstrap_rmse <- bootstrap_rmse_ci_dt(r_h_f_rmse, v_h_f)
sq_h_f_bootstrap_rmse <- bootstrap_rmse_ci_dt(r_h_f_rmse, sq_h_f)



## Integrate 95% CI with main result table
main_df2 <- main_df %>%
  mutate(
    ci_lower = case_when(
      Cohort == "US" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin and Ferritin" ~ v_hf_h_bootstrap_rmse$ci_lower,
      Cohort == "SA" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin and Ferritin" ~ s_hf_h_bootstrap_rmse$ci_lower,
      Cohort == "NL" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin and Ferritin" ~ sq_hf_h_bootstrap_rmse$ci_lower,
      
      Cohort == "US" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin only" ~ v_h_h_bootstrap_rmse$ci_lower,
      Cohort == "SA" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin only" ~ s_h_h_bootstrap_rmse$ci_lower,
      Cohort == "NL" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin only" ~ sq_h_h_bootstrap_rmse$ci_lower,
      
      Cohort == "US" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin and Ferritin" ~ v_hf_f_bootstrap_rmse$ci_lower,
      Cohort == "SA" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin and Ferritin" ~ s_hf_f_bootstrap_rmse$ci_lower,
      Cohort == "NL" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin and Ferritin" ~ sq_hf_f_bootstrap_rmse$ci_lower,
      
      Cohort == "US" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin only" ~ v_h_f_bootstrap_rmse$ci_lower,
      Cohort == "SA" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin only" ~ s_h_f_bootstrap_rmse$ci_lower,
      Cohort == "NL" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin only" ~ sq_h_f_bootstrap_rmse$ci_lower,
      
      TRUE ~ NA_real_
    ),
    
    ci_upper = case_when(
      Cohort == "US" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin and Ferritin" ~ v_hf_h_bootstrap_rmse$ci_upper,
      Cohort == "SA" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin and Ferritin" ~ s_hf_h_bootstrap_rmse$ci_upper,
      Cohort == "NL" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin and Ferritin" ~ sq_hf_h_bootstrap_rmse$ci_upper,
      
      Cohort == "US" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin only" ~ v_h_h_bootstrap_rmse$ci_upper,
      Cohort == "SA" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin only" ~ s_h_h_bootstrap_rmse$ci_upper,
      Cohort == "NL" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin only" ~ sq_h_h_bootstrap_rmse$ci_upper,
      
      Cohort == "US" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin and Ferritin" ~ v_hf_f_bootstrap_rmse$ci_upper,
      Cohort == "SA" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin and Ferritin" ~ s_hf_f_bootstrap_rmse$ci_upper,
      Cohort == "NL" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin and Ferritin" ~ sq_hf_f_bootstrap_rmse$ci_upper,
      
      Cohort == "US" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin only" ~ v_h_f_bootstrap_rmse$ci_upper,
      Cohort == "SA" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin only" ~ s_h_f_bootstrap_rmse$ci_upper,
      Cohort == "NL" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin only" ~ sq_h_f_bootstrap_rmse$ci_upper,
      
      TRUE ~ NA_real_
    )
    
  )

main_df2 <- main_df2 %>%
  mutate(
    ci_text = ifelse(
      !is.na(ci_lower) & !is.na(ci_upper),
      paste0(
        "(",
        ifelse(ci_lower > 0, paste0("+", round(ci_lower, 1)), round(ci_lower, 1)), "%,\n",
        ifelse(ci_upper > 0, paste0("+", round(ci_upper, 1)), round(ci_upper, 1)), "%)"
      ),
      NA_character_
    )
  )

print(main_df2)

# Add 95% CI for RMSE percent change to Figure 3
main_df2_hemoglobin <- main_df2 %>% filter(predict_biomarker=="Hemoglobin")
main_df2_ferritin <- main_df2 %>% filter(predict_biomarker=="Log10 Ferritin")


y_limits_predict_hgb <- c(0, 1.5)
y_limits_predict_ferr <- c(0, 0.5)


# Create a plot function to set y limits separately for each facet
plot_facet <- function(data, y_limits, show_facet_labels = TRUE, hide_x_axis_label = FALSE, hide_y_axis_label = FALSE) { 
  p <- ggplot(data, aes(x = Cohort, y = RMSE, fill = Cohort)) + 
    geom_bar(stat = "identity", color = "black", show.legend = FALSE) + 
    geom_errorbar(aes(ymin = ymin, ymax = ymax), 
                  width = 0.2, position = position_dodge(width = 0.9), na.rm = TRUE) + 
    scale_fill_manual(values = c("tomato", "#abdda4", "#56B4E9", "orange")) + 
    scale_y_continuous(expand = c(0, 0), limits = y_limits) + 
    facet_grid(predict_biomarker ~ data_version) + 
    geom_text(aes(label = str_percent_increase), 
              color = "black", position = position_dodge(width = 0.9), vjust = -0.25, show.legend = FALSE) + 
    geom_text(aes(label = ci_text), 
              color = "black", position = position_dodge(width = 0.9), vjust = -1, size = 2.5, show.legend = FALSE) + 
    geom_text(aes(label = round(RMSE, 2)), 
              color = "white", position = position_dodge(width = 0.9), vjust = 3, show.legend = FALSE) + 
    theme(text = element_text(size = 14), # Increase text size 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), # Rotate x-axis labels 
          strip.background = element_rect(colour = "black", fill = "seashell2", size = 1.5, linetype = "solid"), # Change facet label background color 
          strip.text.x = element_text(size = 11, color = "black", face = "bold"), # Facet x-labels 
          strip.text.y = element_text(size = 12, color = "black", face = "bold"), # Facet y-labels 
          axis.title.y = element_blank()) 
  
  # Optional: Hide facet labels or axis labels if specified
  if (!show_facet_labels) {
    p <- p + theme(strip.text.x = element_blank(),
                   strip.background.x = element_blank())
  }
  if (hide_x_axis_label) {
    p <- p + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  
  return(p)
} 

# Create separate plots for Hemoglobin and Log10 Ferritin with different y limits 
hemoglobin_plot <- plot_facet(main_df2_hemoglobin, y_limits_predict_hgb, hide_x_axis_label = TRUE) 
ferritin_plot <- plot_facet(main_df2_ferritin, y_limits_predict_ferr, show_facet_labels = FALSE) 

# Combine the two plots using cowplot::plot_grid 

# Use cowplot to add a common y-axis label
y_axis_label <- ggdraw() + 
  draw_label("RMSE", angle = 90, x = 0.5, y = 0.5, vjust = 0.5, size = 12)

# Combine the plots using patchwork and ensure equal widths
combined_plot <- (hemoglobin_plot / ferritin_plot) + 
  theme(plot.tag.position = 'left', plot.tag = element_text(size = 14))

# Add the y-axis label and ensure proper alignment using cowplot
final_plot_ci <- plot_grid(y_axis_label, combined_plot, ncol = 2, rel_widths = c(0.05, 1))

# Add the title and alignment
final_plot_ci_2 <- grid.arrange(final_plot_ci, 
                                top = "Baseline biomarkers included", 
                                right = "Follow-up outcome predicted")

final_plot_ci_2

fname_svg <- paste0("./4_output/", "fig3_RMSE_ci.svg") 
fname_png <- paste0("./4_output/", "fig3_RMSE_ci.png")
fname_pdf <- paste0("./4_output/", "fig3_RMSE_ci.pdf")

ggsave(plot = final_plot_ci_2, fname_svg, width = 6, height = 5.5, unit = "in")
ggsave(plot = final_plot_ci_2, fname_png, width = 6, height = 5.5, unit = "in")
ggsave(plot = final_plot_ci_2, fname_pdf, width = 6, height = 5.5, unit = "in")


# _________________________ Bootstrap for 95% CI of RMSE percent change finished _________________________________________________________________________


