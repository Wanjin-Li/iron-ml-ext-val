# This script plots Figure 3: validation results using root mean squared percentage error (RMSPE) 
# for RISE (internal validation - nested cross validation), SANBS, Vitalant, Sanquin.
# This script also performs additional post-doc analysis as follows:
## 1) Additional analysis 1: To determine absent iron stores and iron deficiency at return visits based on return ferritin
## 2) Additional analysis 2: To calculate RMSPE after averaging RMSPE for each individual
## 3) Additional analysis 3: To compare the tuning results from regular RMSPE from the basic linear regression model and the top models
## 4) Additional analysis 4: To check the normal distribution of hemoglobin and ferritin in RISE training data


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

# RMSPE function ----
EPSILON <-  1e-10  # prevent division by zero
get_rmspe <- function(df) {
  r<-(sqrt(mean(((df$fu_outcome - df$prediction) / (df$fu_outcome  + EPSILON))**2))) * 100
  return(r)
}


versions <- c("hgb_ferr_predict_hgb", "hgb_only_predict_hgb", "hgb_ferr_predict_ferr", "hgb_only_predict_ferr")

# RISE (nested cross validation - cross validation outer fold assessment) ----
list_r_hf_h_rmspe <- list()
list_r_h_h_rmspe <- list()
list_r_hf_f_rmspe <- list()
list_r_h_f_rmspe <- list()

list_r_hf_h <- list()
list_r_h_h <- list()
list_r_hf_f <- list()
list_r_h_f <- list()

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
        r_hf_h_rmspe <- get_rmspe(r_hf_h)
        list_r_hf_h_rmspe <- append(list_r_hf_h_rmspe, r_hf_h_rmspe)
        
      } else if (grepl(versions[2], model_res_filename, fixed = TRUE)) {
        r_h_h <- fread(rise_model_res_paths)
        list_r_h_h[[fold]] <- r_h_h
        r_h_h_rmspe <- get_rmspe(r_h_h)
        list_r_h_h_rmspe <- append(list_r_h_h_rmspe, r_h_h_rmspe)
        
      } else if (grepl(versions[3], model_res_filename, fixed = TRUE)) {
        r_hf_f <- fread(rise_model_res_paths)
        list_r_hf_f[[fold]] <- r_hf_f
        r_hf_f_rmspe <- get_rmspe(r_hf_f)
        list_r_hf_f_rmspe <- append(list_r_hf_f_rmspe, r_hf_f_rmspe)
        
      } else if (grepl(versions[4], model_res_filename, fixed = TRUE)) {
        r_h_f <- fread(rise_model_res_paths)
        list_r_h_f[[fold]] <- r_h_f
        r_h_f_rmspe <- get_rmspe(r_h_f)
        list_r_h_f_rmspe <- append(list_r_h_f_rmspe, r_h_f_rmspe)
        
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
        r_hf_h_rmspe <- get_rmspe(avg_df)
        list_r_hf_h_rmspe <- append(list_r_hf_h_rmspe, r_hf_h_rmspe)
        
      } else if (grepl(versions[2], rise_model_res_paths[1], fixed = TRUE)) {
        r_h_h <- avg_df
        list_r_h_h[[fold]] <- r_h_h
        r_h_h_rmspe <- get_rmspe(avg_df)
        list_r_h_h_rmspe <- append(list_r_h_h_rmspe, r_h_h_rmspe)
        
      } else if (grepl(versions[3], rise_model_res_paths[1], fixed = TRUE)) {
        r_hf_f <- avg_df
        list_r_hf_f[[fold]] <- r_hf_f
        r_hf_f_rmspe <- get_rmspe(avg_df)
        list_r_hf_f_rmspe <- append(list_r_hf_f_rmspe, r_hf_f_rmspe)
        
      } else if (grepl(versions[4], rise_model_res_paths[1], fixed = TRUE)) {
        r_h_f <- avg_df
        list_r_h_f[[fold]] <- r_h_f
        r_h_f_rmspe <- get_rmspe(avg_df)
        list_r_h_f_rmspe <- append(list_r_h_f_rmspe, r_h_f_rmspe)
        
      }
    }
  }
  
}

# Average RMSPE of three outer folds
r_hf_h_rmspe <- mean(unlist(list_r_hf_h_rmspe))
r_h_h_rmspe <- mean(unlist(list_r_h_h_rmspe))
r_hf_f_rmspe <- mean(unlist(list_r_hf_f_rmspe))
r_h_f_rmspe <- mean(unlist(list_r_h_f_rmspe))

# Calculate min and max for RMSPE values 
min_r_hf_h_rmspe <- min(unlist(list_r_hf_h_rmspe)) 
max_r_hf_h_rmspe <- max(unlist(list_r_hf_h_rmspe)) 
min_r_h_h_rmspe <- min(unlist(list_r_h_h_rmspe)) 
max_r_h_h_rmspe <- max(unlist(list_r_h_h_rmspe)) 
min_r_hf_f_rmspe <- min(unlist(list_r_hf_f_rmspe)) 
max_r_hf_f_rmspe <- max(unlist(list_r_hf_f_rmspe)) 
min_r_h_f_rmspe <- min(unlist(list_r_h_f_rmspe)) 
max_r_h_f_rmspe <- max(unlist(list_r_h_f_rmspe))



# SANBS ----
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


# Vitalant ----
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

# Sanquin ----  
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




# Set up dataframes for plotting ----
cohort <- c("RISE", "Vitalant", "SANBS", "Sanquin")


hgb_ferr_predict_hgb <- data.frame(Cohort  = cohort,
                                   RMSPE = c(r_hf_h_rmspe, v_hf_h_rmspe, s_hf_h_rmspe, sq_hf_h_rmspe),
                                   ymin = c(min_r_hf_h_rmspe, NA, NA, NA),
                                   ymax = c(max_r_hf_h_rmspe, NA, NA, NA),
                                   predict_biomarker = c("Hemoglobin"),
                                   data_version = c("Hemoglobin and Ferritin"))
hgb_only_predict_hgb <- data.frame(Cohort  = cohort,
                                   RMSPE = c(r_h_h_rmspe, v_h_h_rmspe, s_h_h_rmspe, sq_h_h_rmspe),
                                   ymin = c(min_r_h_h_rmspe, NA, NA, NA),
                                   ymax = c(max_r_h_h_rmspe, NA, NA, NA),
                                   predict_biomarker = c("Hemoglobin"),
                                   data_version = c("Hemoglobin only"))
hgb_ferr_predict_ferr <- data.frame(Cohort  = cohort,
                                    RMSPE = c(r_hf_f_rmspe, v_hf_f_rmspe, s_hf_f_rmspe, sq_hf_f_rmspe),
                                    ymin = c(min_r_hf_f_rmspe, NA, NA, NA),
                                    ymax = c(max_r_hf_f_rmspe, NA, NA, NA),
                                    predict_biomarker = c("Log10 Ferritin"),
                                    data_version = c("Hemoglobin and Ferritin"))
hgb_only_predict_ferr <- data.frame(Cohort  = cohort,
                                    RMSPE = c(r_h_f_rmspe, v_h_f_rmspe, s_h_f_rmspe, sq_h_f_rmspe),
                                    ymin = c(min_r_h_f_rmspe, NA, NA, NA),
                                    ymax = c(max_r_h_f_rmspe, NA, NA, NA),
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

# add Can you add "% increase" on top of the blue and green bars? Like "+6%" "-2%"? ----

compute_percentage_change_in_rmspe <- function(base, new) {
  #print(new)
  
  res <- round( (100 * (new - base) / base ), 1)
  return(res)
  
}

percent_increase <- c(NA)


# hgb_ferr - Predict hemoglobin  

base <- r_hf_h_rmspe

new <- v_hf_h_rmspe; res <- compute_percentage_change_in_rmspe(base, new); percent_increase <- append(percent_increase, res)
new <- s_hf_h_rmspe; res <- compute_percentage_change_in_rmspe(base, new); percent_increase <- append(percent_increase, res)
new <- sq_hf_h_rmspe; res <- compute_percentage_change_in_rmspe(base, new); percent_increase <- append(percent_increase, res)

percent_increase <- append(percent_increase, NA)

# hgb_only - Predict hemoglobin 

base <- r_h_h_rmspe

new <- v_h_h_rmspe; res <- compute_percentage_change_in_rmspe(base, new); percent_increase <- append(percent_increase, res)
new <- s_h_h_rmspe; res <- compute_percentage_change_in_rmspe(base, new); percent_increase <- append(percent_increase, res)
new <- sq_h_h_rmspe; res <- compute_percentage_change_in_rmspe(base, new); percent_increase <- append(percent_increase, res)

percent_increase <- append(percent_increase, NA)

# hgb_ferr - Predict ferritin 

base <- r_hf_f_rmspe

new <- v_hf_f_rmspe; res <- compute_percentage_change_in_rmspe(base, new); percent_increase <- append(percent_increase, res)
new <- s_hf_f_rmspe; res <- compute_percentage_change_in_rmspe(base, new); percent_increase <- append(percent_increase, res)
new <- sq_hf_f_rmspe; res <- compute_percentage_change_in_rmspe(base, new); percent_increase <- append(percent_increase, res)

percent_increase <- append(percent_increase, NA)

# hgb_only - Predict ferritin 

base <- r_h_f_rmspe

new <- v_h_f_rmspe; res <- compute_percentage_change_in_rmspe(base, new); percent_increase <- append(percent_increase, res)
new <- s_h_f_rmspe; res <- compute_percentage_change_in_rmspe(base, new); percent_increase <- append(percent_increase, res)
new <- sq_h_f_rmspe; res <- compute_percentage_change_in_rmspe(base, new); percent_increase <- append(percent_increase, res)


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


# Plot FIGURE 3 RMSPE ----

# Create a plot function to set y limits separately for each facet 
main_df_hemoglobin <- main_df %>% filter(predict_biomarker=="Hemoglobin")
main_df_ferritin <- main_df %>% filter(predict_biomarker=="Log10 Ferritin")


y_limits_predict_hgb <- c(0, 10)
y_limits_predict_ferr <- c(0, 35)


plot_facet <- function(data, y_limits, show_facet_lables=TRUE, hide_x_axis_label=FALSE, hide_y_axis_lable=FALSE) { 
  p <- ggplot(data, aes(x = Cohort, y = RMSPE, fill = Cohort)) + 
    geom_bar(stat = "identity", color = "black", show.legend = FALSE) + 
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, position = position_dodge(width = 0.9), na.rm = TRUE) + 
    scale_fill_manual(values = c("tomato", "#abdda4", "#56B4E9", "orange")) + 
    scale_y_continuous(expand = c(0, 0), limits = y_limits) + 
    facet_grid(predict_biomarker ~ data_version) + 
    # geom_point(data=data2,aes(x=Cohort,y=RMSPE),alpha=0, show.legend = FALSE) + 
    geom_text(aes(label = str_percent_increase), color = "black", position = position_dodge(width = 0.9), vjust = -0.25, show.legend = FALSE) + 
    geom_text(aes(label = round(RMSPE, 2)), color = "white", position = position_dodge(width = 0.9), vjust = 3, show.legend = FALSE) + 
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
y_axis_label <- ggdraw() + draw_label("RMSPE", angle = 90, x = 0.5, y = 0.5, vjust = 0.5, size = 12)

# Combine the plots using patchwork and ensure equal widths
combined_plot <- (hemoglobin_plot / ferritin_plot) + 
  theme(plot.tag.position = 'left', plot.tag = element_text(size = 14))

# Add the y-axis label and ensure proper alignment using cowplot
final_plot <- plot_grid(y_axis_label, combined_plot, ncol = 2, rel_widths = c(0.05, 1))

final_plot_2 <- grid.arrange(final_plot, top="Baseline biomarkers included", right="Follow-up outcome predicted")
final_plot_2


fname_svg <- paste0("./4_output/", "fig3.svg") 
fname_png <- paste0("./4_output/", "fig3.png")
fname_pdf <- paste0("./4_output/", "fig3.pdf")

ggsave(plot = final_plot_2, fname_svg, width = 6, height = 5.5, unit = "in")
ggsave(plot = final_plot_2, fname_png, width = 6, height = 5.5, unit = "in")
ggsave(plot = final_plot_2, fname_pdf, width = 6, height = 5.5, unit = "in")

# Bootstrap to calculate 95% CI for RMSPE percent change ----

bootstrap_rmspe_ci_dt <- function(base_rmspe, cohort_data, n_boot=1000, seed=12345){
  set.seed(seed)
  
  # Number of rows in external cohort data
  n <- nrow(cohort_data)
  
  # Perform bootstrapping and compute RMSPE 
  bootstrap_results <- replicate(n_boot, {
    
    # Resample external cohort with replacement
    resample_indices <- sample(1:n, size=n, replace = TRUE)
    ext_cohort_sample <- cohort_data[resample_indices]
    
    # Calculate RMSPE for resampled data
    ext_cohort_rmspe <- get_rmspe(ext_cohort_sample)
    
    # Calculate percent change
    compute_percentage_change_in_rmspe(base_rmspe, ext_cohort_rmspe)
    
  })
  
  # Compute mean and confidence intervals
  mean_change <- mean(bootstrap_results)
  ci <- quantile(bootstrap_results, probs = c(0.025, 0.975))
  
  # Return results
  list(mean = mean_change, ci_lower=ci[1], ci_upper=ci[2])

}


## Predicting hemoglobin with hemoglobin and ferritin 

# Perform bootstrapping for each cohort
s_hf_h_bootstrap_rmspe <- bootstrap_rmspe_ci_dt(r_hf_h_rmspe, s_hf_h)
v_hf_h_bootstrap_rmspe <- bootstrap_rmspe_ci_dt(r_hf_h_rmspe, v_hf_h)
sq_hf_h_bootstrap_rmspe <- bootstrap_rmspe_ci_dt(r_hf_h_rmspe, sq_hf_h)

## Predicting hemoglobin with hemoglobin only 

# Perform bootstrapping for each cohort
s_h_h_bootstrap_rmspe <- bootstrap_rmspe_ci_dt(r_h_h_rmspe, s_h_h)
v_h_h_bootstrap_rmspe <- bootstrap_rmspe_ci_dt(r_h_h_rmspe, v_h_h)
sq_h_h_bootstrap_rmspe <- bootstrap_rmspe_ci_dt(r_h_h_rmspe, sq_h_h)

## Predicting ferritin with hemoglobin and ferritin 

# Perform bootstrapping for each cohort
s_hf_f_bootstrap_rmspe <- bootstrap_rmspe_ci_dt(r_hf_f_rmspe, s_hf_f)
v_hf_f_bootstrap_rmspe <- bootstrap_rmspe_ci_dt(r_hf_f_rmspe, v_hf_f)
sq_hf_f_bootstrap_rmspe <- bootstrap_rmspe_ci_dt(r_hf_f_rmspe, sq_hf_f)

## Predicting ferritin with hemoglobin only

# Perform bootstrapping for each cohort
s_h_f_bootstrap_rmspe <- bootstrap_rmspe_ci_dt(r_h_f_rmspe, s_h_f)
v_h_f_bootstrap_rmspe <- bootstrap_rmspe_ci_dt(r_h_f_rmspe, v_h_f)
sq_h_f_bootstrap_rmspe <- bootstrap_rmspe_ci_dt(r_h_f_rmspe, sq_h_f)



## Integrate 95% CI with main result table
main_df2 <- main_df %>%
  mutate(
    ci_lower = case_when(
      Cohort == "US" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin and Ferritin" ~ v_hf_h_bootstrap_rmspe$ci_lower,
      Cohort == "SA" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin and Ferritin" ~ s_hf_h_bootstrap_rmspe$ci_lower,
      Cohort == "NL" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin and Ferritin" ~ sq_hf_h_bootstrap_rmspe$ci_lower,
      
      Cohort == "US" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin only" ~ v_h_h_bootstrap_rmspe$ci_lower,
      Cohort == "SA" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin only" ~ s_h_h_bootstrap_rmspe$ci_lower,
      Cohort == "NL" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin only" ~ sq_h_h_bootstrap_rmspe$ci_lower,
      
      Cohort == "US" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin and Ferritin" ~ v_hf_f_bootstrap_rmspe$ci_lower,
      Cohort == "SA" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin and Ferritin" ~ s_hf_f_bootstrap_rmspe$ci_lower,
      Cohort == "NL" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin and Ferritin" ~ sq_hf_f_bootstrap_rmspe$ci_lower,
      
      Cohort == "US" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin only" ~ v_h_f_bootstrap_rmspe$ci_lower,
      Cohort == "SA" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin only" ~ s_h_f_bootstrap_rmspe$ci_lower,
      Cohort == "NL" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin only" ~ sq_h_f_bootstrap_rmspe$ci_lower,
      
      TRUE ~ NA_real_
    ),
    
    ci_upper = case_when(
      Cohort == "US" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin and Ferritin" ~ v_hf_h_bootstrap_rmspe$ci_upper,
      Cohort == "SA" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin and Ferritin" ~ s_hf_h_bootstrap_rmspe$ci_upper,
      Cohort == "NL" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin and Ferritin" ~ sq_hf_h_bootstrap_rmspe$ci_upper,
      
      Cohort == "US" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin only" ~ v_h_h_bootstrap_rmspe$ci_upper,
      Cohort == "SA" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin only" ~ s_h_h_bootstrap_rmspe$ci_upper,
      Cohort == "NL" & predict_biomarker == "Hemoglobin" & data_version == "Hemoglobin only" ~ sq_h_h_bootstrap_rmspe$ci_upper,
      
      Cohort == "US" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin and Ferritin" ~ v_hf_f_bootstrap_rmspe$ci_upper,
      Cohort == "SA" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin and Ferritin" ~ s_hf_f_bootstrap_rmspe$ci_upper,
      Cohort == "NL" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin and Ferritin" ~ sq_hf_f_bootstrap_rmspe$ci_upper,
      
      Cohort == "US" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin only" ~ v_h_f_bootstrap_rmspe$ci_upper,
      Cohort == "SA" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin only" ~ s_h_f_bootstrap_rmspe$ci_upper,
      Cohort == "NL" & predict_biomarker == "Log10 Ferritin" & data_version == "Hemoglobin only" ~ sq_h_f_bootstrap_rmspe$ci_upper,
      
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

# Add 95% CI for RMSPE percent change to Figure 3
main_df2_hemoglobin <- main_df2 %>% filter(predict_biomarker=="Hemoglobin")
main_df2_ferritin <- main_df2 %>% filter(predict_biomarker=="Log10 Ferritin")


y_limits_predict_hgb <- c(0, 10)
y_limits_predict_ferr <- c(0, 40)


# Create a plot function to set y limits separately for each facet
plot_facet <- function(data, y_limits, show_facet_labels = TRUE, hide_x_axis_label = FALSE, hide_y_axis_label = FALSE) { 
  p <- ggplot(data, aes(x = Cohort, y = RMSPE, fill = Cohort)) + 
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
    geom_text(aes(label = round(RMSPE, 2)), 
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
  draw_label("RMSPE", angle = 90, x = 0.5, y = 0.5, vjust = 0.5, size = 12)

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

fname_svg <- paste0("./4_output/", "fig3_ci.svg") 
fname_png <- paste0("./4_output/", "fig3_ci.png")
fname_pdf <- paste0("./4_output/", "fig3_ci.pdf")

ggsave(plot = final_plot_ci_2, fname_svg, width = 6, height = 5.5, unit = "in")
ggsave(plot = final_plot_ci_2, fname_png, width = 6, height = 5.5, unit = "in")
ggsave(plot = final_plot_ci_2, fname_pdf, width = 6, height = 5.5, unit = "in")


# _________________________ Bootstrap for 95% CI of RMSPE percent change finished _________________________________________________________________________



# Subgroup performance: age, sex, race ----

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


## Vitalant ----
v_df <- data.frame(fread("./3_intermediate/private/vitalant_updates/vitalant_intermediate_to_del6.csv"))
v_df <- v_df[,hgb_ferr_var_race]

v_hgb_ferr <- fread("./3_intermediate/private/vitalant_updates/hgb_ferr_vitalant.csv")  
v_hgb_only <- fread("./3_intermediate/private/vitalant_updates/hgb_only_vitalant.csv")  

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

######### Performance across subgroups #############

# get rmse
get_rmse <- function(df) {
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
  #redefining reference categories
  df$sex <- relevel(factor(df$sex), ref = 'M')
  df$race <- relevel(factor(df$race), ref = 'White') 
  df$agegroup <- relevel(factor(df$agegroup), ref = '15-24') 
  df$time_to_fu_quantiles <- relevel(factor(df$time_to_fu_quantiles), ref = 'q1')
  
  model <- lm(rmspe ~ agegroup + sex + race + time_to_fu_quantiles, data = df) 
  
  # model <- lm(rmspe ~ agegroup + sex + race + time_to_fu + 
  #               I(time_to_fu^2) + I(time_to_fu^3), data = df) # use a polynomial expansion of time with two or three degrees (i.e. FU + FU^2 + FU^3) to address reviewer's comment
  #sex*dons_hist
  coef <- coef(model)
  conf_intervals <- confint(model)
  coef_df <- data.frame(Term = names(coef),
                        Coef= coef,
                        Conf_Lower = conf_intervals[, 1],
                        Conf_Upper = conf_intervals[, 2])
  
  
  return (coef_df)
}




################ SANBS: run linear models ###############

# UNCOMMENT THE FOLLOWING LINES WHEN USING TIME_TO_FU AS QUANTILES
# add column to divide time_to_fu into quartiles
s_hgb_only$time_to_fu_quantiles <- quant_groups(s_hgb_only$time_to_fu, 4)
levels(s_hgb_only$time_to_fu_quantiles)
levels(s_hgb_only$time_to_fu_quantiles)<-c('q1', 'q2', 'q3', 'q4')

s_hgb_ferr$time_to_fu_quantiles <- quant_groups(s_hgb_ferr$time_to_fu, 4)
levels(s_hgb_ferr$time_to_fu_quantiles)
levels(s_hgb_ferr$time_to_fu_quantiles)<-c('q1', 'q2', 'q3', 'q4')


#hgb only dataset
## predicting  hemoglobin

s_hgb_only$rmspe<-get_rmse(s_h_h)
s_h_h_lm<-run_lm(s_hgb_only[c('age', 'sex', 'race', 'time_to_fu_quantiles', 'rmspe')])
s_h_h_lm<-s_h_h_lm[-c(8, 9),] # remove unknown/mixed race
s_subgroup_lm<-s_h_h_lm
colnames(s_subgroup_lm) <- c('Variable', 's_h_h_coef', 's_h_h_conf_lower', 's_h_h_conf_upper')
  
## predicting  ferritin
s_hgb_only$rmspe<-get_rmse(s_h_f)
s_h_f_lm<-run_lm(s_hgb_only[c('age', 'sex', 'race', 'time_to_fu_quantiles','rmspe')])
s_h_f_lm<-s_h_f_lm[-c(8, 9),]
s_subgroup_lm<-cbind(s_subgroup_lm, s_h_f_lm[,2:4])
colnames(s_subgroup_lm)[5:7] <- c( 's_h_f_coef', 's_h_f_conf_lower', 's_h_f_conf_upper')

#hgb and ferr dataset
## predicting hemoglobin
s_hgb_ferr$rmspe<- get_rmse(s_hf_h)
s_hf_h_lm<-run_lm(s_hgb_ferr[c('age', 'sex', 'race', 'time_to_fu_quantiles','rmspe')])
s_hf_h_lm<-s_hf_h_lm[-c(8, 9),]
s_subgroup_lm<-cbind(s_subgroup_lm, s_hf_h_lm[,2:4])
colnames(s_subgroup_lm)[8:10] <- c( 's_hf_h_coef', 's_hf_h_conf_lower', 's_hf_h_conf_upper')


## predicting ferritin
s_hgb_ferr$rmspe<-get_rmse(s_hf_f)
s_hf_f_lm<-run_lm(s_hgb_ferr[c('age', 'sex', 'race','time_to_fu_quantiles', 'rmspe')])
s_hf_f_lm<-s_hf_f_lm[-c(8, 9),]
s_subgroup_lm<-cbind(s_subgroup_lm, s_hf_f_lm[,2:4])
colnames(s_subgroup_lm)[11:13] <- c( 's_hf_f_coef', 's_hf_f_conf_lower', 's_hf_f_conf_upper')

# _________________NEW__________
# NEW subgroup performance table modeling time_to_fu as quantile groups
## export parameter value table to a csv
# path <- "./3_intermediate"
# folder <- "subgroup_performance"
# 
# newpath <- file.path(path, folder)
# if(!dir.exists(newpath)){
#   dir.create(newpath)
# }
# 
# s_filename <- "s_subgroup_lm_data_polyfu"
# 
# s_filepath <- paste0(s_filename, ".csv")
# s_filepath <- file.path(newpath, s_filepath)
# 
# write.csv(s_subgroup_lm, s_filepath, row.names = FALSE)

# ________________ORIGINAL_____________
# ORIGINAL subgroup performance table modeling time_to_fu as quantile groups
## export parameter value table to a csv
path <- "./3_intermediate"
folder <- "subgroup_performance"

newpath <- file.path(path, folder)
if(!dir.exists(newpath)){
  dir.create(newpath)
}

s_filename <- "s_subgroup_lm_data"

s_filepath <- paste0(s_filename, ".csv")
s_filepath <- file.path(newpath, s_filepath)

write.csv(s_subgroup_lm, s_filepath, row.names = FALSE)

################ Vitalant: run linear models ###############

# add column to divide time_to_fu into quartiles
v_hgb_only$time_to_fu_quantiles <- quant_groups(v_hgb_only$time_to_fu, 4)
levels(v_hgb_only$time_to_fu_quantiles)
levels(v_hgb_only$time_to_fu_quantiles)<-c('q1', 'q2', 'q3', 'q4')

v_hgb_ferr$time_to_fu_quantiles <- quant_groups(v_hgb_ferr$time_to_fu, 4)
levels(v_hgb_ferr$time_to_fu_quantiles)
levels(v_hgb_ferr$time_to_fu_quantiles)<-c('q1', 'q2', 'q3', 'q4')


#hgb only dataset
## predicting  hemoglobin
v_hgb_only$rmspe<-get_rmse(v_h_h)
v_h_h_lm<-run_lm(v_hgb_only[c('age', 'sex', 'race', 'time_to_fu_quantiles','rmspe')])
v_h_h_lm<-v_h_h_lm[-c(9, 10, 11, 12),] # remove unknown/missing/mixed/other race
v_subgroup_lm<-v_h_h_lm
colnames(v_subgroup_lm) <- c('Variable', 'v_h_h_coef', 'v_h_h_conf_lower', 'v_h_h_conf_upper')

## predicting  ferritin
v_hgb_only$rmspe<-get_rmse(v_h_f)
v_h_f_lm<-run_lm(v_hgb_only[c('age', 'sex', 'race', 'time_to_fu_quantiles','rmspe')])
v_h_f_lm<-v_h_f_lm[-c(9, 10, 11, 12),]
v_subgroup_lm<-cbind(v_subgroup_lm, v_h_f_lm[,2:4])
colnames(v_subgroup_lm)[5:7] <- c( 'v_h_f_coef', 'v_h_f_conf_lower', 'v_h_f_conf_upper')


#hgb and ferr dataset
## predicting hemoglobin
v_hgb_ferr$rmspe<-get_rmse(v_hf_h)
v_hf_h_lm<-run_lm(v_hgb_ferr[c('age', 'sex', 'race','time_to_fu_quantiles', 'rmspe')])
v_hf_h_lm<-v_hf_h_lm[-c(9, 10, 11, 12),]
v_subgroup_lm<-cbind(v_subgroup_lm, v_hf_h_lm[,2:4])
colnames(v_subgroup_lm)[8:10] <- c( 'v_hf_h_coef', 'v_hf_h_conf_lower', 'v_hf_h_conf_upper')

## predicting ferritin
v_hgb_ferr$rmspe<-get_rmse(v_hf_f)
v_hf_f_lm<-run_lm(v_hgb_ferr[c('age', 'sex', 'race', 'time_to_fu_quantiles','rmspe')])
v_hf_f_lm<-v_hf_f_lm[-c(9, 10, 11, 12),]
v_subgroup_lm<-cbind(v_subgroup_lm, v_hf_f_lm[,2:4])
colnames(v_subgroup_lm)[11:13] <- c( 'v_hf_f_coef', 'v_hf_f_conf_lower', 'v_hf_f_conf_upper')

# _________________NEW__________
# NEW subgroup performance table modeling time_to_fu as quantile groups
## export parameter value table to a csv
# 
# v_filename <- "v_subgroup_lm_data_polyfu"
# 
# v_filepath <- paste0(v_filename, ".csv")
# v_filepath <- file.path(newpath, v_filepath)
# 
# write.csv(v_subgroup_lm, v_filepath, row.names = FALSE)


# ________________ORIGINAL_____________
# ORIGINAL subgroup performance table modeling time_to_fu as quantile groups
## export parameter value table to a csv

v_filename <- "v_subgroup_lm_data"

v_filepath <- paste0(v_filename, ".csv")
v_filepath <- file.path(newpath, v_filepath)

write.csv(v_subgroup_lm, v_filepath, row.names = FALSE)



################ Sanquin: run linear models ###############

# add column donation_history - donations in past 24 months

# add column to divide time_to_fu into quartiles
sq_hgb_only$time_to_fu_quantiles <- quant_groups(sq_hgb_only$time_to_fu, 4)
levels(sq_hgb_only$time_to_fu_quantiles)
levels(sq_hgb_only$time_to_fu_quantiles)<-c('q1', 'q2', 'q3', 'q4')

sq_hgb_ferr$time_to_fu_quantiles <- quant_groups(sq_hgb_ferr$time_to_fu, 4)
levels(sq_hgb_ferr$time_to_fu_quantiles)
levels(sq_hgb_ferr$time_to_fu_quantiles)<-c('q1', 'q2', 'q3', 'q4')

#hgb only dataset
## predicting  hemoglobin
sq_hgb_only$rmspe<-get_rmse(sq_h_h)
sq_h_h_lm<-run_lm(sq_hgb_only[c('age', 'sex', 'race', 'time_to_fu_quantiles', 'rmspe')])
sq_subgroup_lm<-sq_h_h_lm
colnames(sq_subgroup_lm) <- c('Variable', 'sq_h_h_coef', 'sq_h_h_conf_lower', 'sq_h_h_conf_upper')


## predicting  ferritin
sq_hgb_only$rmspe<-get_rmse(sq_h_f)
sq_h_f_lm<-run_lm(sq_hgb_only[c('age', 'sex', 'race', 'time_to_fu_quantiles','rmspe')])
sq_subgroup_lm<-cbind(sq_subgroup_lm, sq_h_f_lm[,2:4])
colnames(sq_subgroup_lm)[5:7] <- c( 'sq_h_f_coef', 'sq_h_f_conf_lower', 'sq_h_f_conf_upper')


#hgb and ferr dataset
## predicting hemoglobin
sq_hgb_ferr$rmspe<- get_rmse(sq_hf_h)
sq_hf_h_lm<-run_lm(sq_hgb_ferr[c('age', 'sex', 'race', 'time_to_fu_quantiles','rmspe')])
sq_subgroup_lm<-cbind(sq_subgroup_lm, sq_hf_h_lm[,2:4])
colnames(sq_subgroup_lm)[8:10] <- c( 'sq_hf_h_coef', 'sq_hf_h_conf_lower', 'sq_hf_h_conf_upper')


## predicting ferritin
sq_hgb_ferr$rmspe<-get_rmse(sq_hf_f)
sq_hf_f_lm<-run_lm(sq_hgb_ferr[c('age', 'sex', 'race','time_to_fu_quantiles', 'rmspe')])
sq_subgroup_lm<-cbind(sq_subgroup_lm, sq_hf_f_lm[,2:4])
colnames(sq_subgroup_lm)[11:13] <- c( 'sq_hf_f_coef', 'sq_hf_f_conf_lower', 'sq_hf_f_conf_upper')

# export parameter value table to a csv

# currdir <- getwd()
# currfolder <- "3_intermediate"
# path <- file.path(currdir, currfolder)
# folder <- "subgroup_performance"
# 
# newpath <- file.path(path, folder)
# if(!dir.exists(newpath)){
#   dir.create(newpath)
# }

sq_filename <- "sq_subgroup_lm_data"

sq_filepath <- paste0(sq_filename, ".csv")
sq_filepath <- file.path(newpath, sq_filepath)

write.csv(sq_subgroup_lm, sq_filepath, row.names = FALSE)



## Obtain group size of each subgroup category stratified by cohorts -----
variables <- c(
  "time_to_fu_quantilesq4", "time_to_fu_quantilesq3", "time_to_fu_quantilesq2", "time_to_fu_quantilesq1",
  "sexF", "sexM",
  "raceHispanic", "raceBlack", "raceAsian", "raceWhite",
  "agegroup64+", "agegroup40-64", "agegroup25-39", "agegroup15-24"
)
countries <- c("US", "SA", "NL")
datasets <- c("Hemoglobin and ferritin", "Hemoglobin only")
# outcomes <- c("Ferritin", "Hemoglobin")

# Create the data.table with all combinations
group_size_dat <- CJ(Variable = variables, 
                     Country = countries, 
                     Dataset = datasets)
group_size_dat[, Size:=0]

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

s_hgb_ferr <- create_subgroup(s_hgb_ferr)
s_hgb_only <- create_subgroup(s_hgb_only)
v_hgb_ferr <- create_subgroup(v_hgb_ferr)
v_hgb_only <- create_subgroup(v_hgb_only)

# Excluding NL from group size table now and add it back later on
subset_group_size_dat <- group_size_dat[group_size_dat[["Country"]] != "NL"]


for (i in 1:nrow(subset_group_size_dat)){
  obs <- subset_group_size_dat[i]
  if (obs[["Country"]] == "SA" & obs[["Dataset"]] == "Hemoglobin and ferritin"){
    ds <- s_hgb_ferr
  } else if (obs[["Country"]] == "SA" & obs[["Dataset"]] == "Hemoglobin only"){
    ds <- s_hgb_only
  } else if (obs[["Country"]] == "US" & obs[["Dataset"]] == "Hemoglobin and ferritin"){
    ds <- v_hgb_ferr
  } else if (obs[["Country"]] == "US" & obs[["Dataset"]] == "Hemoglobin only"){
    ds <- v_hgb_only
  }
  
  if (obs[["Variable"]] == "agegroup15-24"){
    gs <- nrow(ds[ds$agegroup == "15-24",])
  } else if (obs[["Variable"]] == "agegroup25-39"){
    gs <- nrow(ds[ds$agegroup == "25-39",])
  } else if (obs[["Variable"]] == "agegroup40-64"){
    gs <- nrow(ds[ds$agegroup == "40-64",])
  } else if (obs[["Variable"]] == "agegroup64+"){
    gs <- nrow(ds[ds$agegroup == "64+",])
  } else if (obs[["Variable"]] == "raceAsian"){
    gs <- nrow(ds[ds$race == "Asian",])
  } else if (obs[["Variable"]] == "raceBlack"){
    gs <- nrow(ds[ds$race == "Black",])
  } else if (obs[["Variable"]] == "raceHispanic"){
    gs <- nrow(ds[ds$race == "Hispanic",])
  } else if (obs[["Variable"]] == "raceWhite"){
    gs <- nrow(ds[ds$race == "White",])
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
  } 
 
  subset_group_size_dat[i, "Size"] <- gs
}

fwrite(subset_group_size_dat, "./3_intermediate/subgroup_performance/subgroup_group_size.csv")



# Addition analysis 1: To determine low and/or absent iron stores at return visits based on follow-up ferritin and hemoglobin ######

# Extract ferritin prediction results
r_hf_f_outer1 <- list_r_hf_f[["outer_fold_1"]]
r_hf_f_outer2 <- list_r_hf_f[["outer_fold_2"]]
r_hf_f_outer3 <- list_r_hf_f[["outer_fold_3"]]
r_h_f_outer1 <- list_r_h_f[["outer_fold_1"]]
r_h_f_outer2 <- list_r_h_f[["outer_fold_2"]]
r_h_f_outer3 <- list_r_h_f[["outer_fold_3"]]

# Extract hemoglobin prediction results
r_hf_h_outer1 <- list_r_hf_h[["outer_fold_1"]]
r_hf_h_outer2 <- list_r_hf_h[["outer_fold_2"]]
r_hf_h_outer3 <- list_r_hf_h[["outer_fold_3"]]
r_h_h_outer1 <- list_r_h_h[["outer_fold_1"]]
r_h_h_outer2 <- list_r_h_h[["outer_fold_2"]]
r_h_h_outer3 <- list_r_h_h[["outer_fold_3"]]

# Extract original test outer fold data to obtain sex information

# Ferritin prediction
test_r_hf_f_outer1 <- fread("./3_intermediate/private/outer_fold_test_data/pred_ferr/test_factors_hgb_ferr_outer_fold_1.csv")
test_r_hf_f_outer2 <- fread("./3_intermediate/private/outer_fold_test_data/pred_ferr/test_factors_hgb_ferr_outer_fold_2.csv")
test_r_hf_f_outer3 <- fread("./3_intermediate/private/outer_fold_test_data/pred_ferr/test_factors_hgb_ferr_outer_fold_3.csv")
test_r_h_f_outer1 <- fread("./3_intermediate/private/outer_fold_test_data/pred_ferr/test_factors_hgb_only_outer_fold_1.csv")
test_r_h_f_outer2 <- fread("./3_intermediate/private/outer_fold_test_data/pred_ferr/test_factors_hgb_only_outer_fold_2.csv")
test_r_h_f_outer3 <- fread("./3_intermediate/private/outer_fold_test_data/pred_ferr/test_factors_hgb_only_outer_fold_3.csv")

test_s_hf_f <- fread("./3_intermediate/private/hgb_ferr_sanbs.csv")
test_s_h_f <- fread("./3_intermediate/private/hgb_only_sanbs.csv")

test_v_hf_f <- fread("./3_intermediate/private/vitalant_updates/hgb_ferr_vitalant.csv")
test_v_h_f <- fread("./3_intermediate/private/vitalant_updates/hgb_only_vitalant.csv")

# Hemoglobin prediction
test_r_hf_h_outer1 <- fread("./3_intermediate/private/outer_fold_test_data/pred_hgb/test_factors_hgb_ferr_outer_fold_1.csv")
test_r_hf_h_outer2 <- fread("./3_intermediate/private/outer_fold_test_data/pred_hgb/test_factors_hgb_ferr_outer_fold_2.csv")
test_r_hf_h_outer3 <- fread("./3_intermediate/private/outer_fold_test_data/pred_hgb/test_factors_hgb_ferr_outer_fold_3.csv")
test_r_h_h_outer1 <- fread("./3_intermediate/private/outer_fold_test_data/pred_hgb/test_factors_hgb_only_outer_fold_1.csv")
test_r_h_h_outer2 <- fread("./3_intermediate/private/outer_fold_test_data/pred_hgb/test_factors_hgb_only_outer_fold_2.csv")
test_r_h_h_outer3 <- fread("./3_intermediate/private/outer_fold_test_data/pred_hgb/test_factors_hgb_only_outer_fold_3.csv")

test_s_hf_h <- fread("./3_intermediate/private/hgb_ferr_sanbs.csv")
test_s_h_h <- fread("./3_intermediate/private/hgb_only_sanbs.csv")

test_v_hf_h <- fread("./3_intermediate/private/vitalant_updates/hgb_ferr_vitalant.csv")
test_v_h_h <- fread("./3_intermediate/private/vitalant_updates/hgb_only_vitalant.csv")

# Combine original test outer fold data with prediction

# Ferritin prediction
combined_r_hf_f_outer1 <- cbind(test_r_hf_f_outer1, r_hf_f_outer1)
combined_r_hf_f_outer2 <- cbind(test_r_hf_f_outer2, r_hf_f_outer2)
combined_r_hf_f_outer3 <- cbind(test_r_hf_f_outer3, r_hf_f_outer3)
combined_r_h_f_outer1 <- cbind(test_r_h_f_outer1, r_h_f_outer1)
combined_r_h_f_outer2 <- cbind(test_r_h_f_outer2, r_h_f_outer2)
combined_r_h_f_outer3 <- cbind(test_r_h_f_outer3, r_h_f_outer3)

combined_s_hf_f <- cbind(test_s_hf_f, s_hf_f)
combined_s_h_f <- cbind(test_s_h_f, s_h_f)

combined_v_hf_f <- cbind(test_v_hf_f, v_hf_f)
combined_v_h_f <- cbind(test_v_h_f, v_h_f)

# Hemoglobin prediction
combined_r_hf_h_outer1 <- cbind(test_r_hf_h_outer1, r_hf_h_outer1)
combined_r_hf_h_outer2 <- cbind(test_r_hf_h_outer2, r_hf_h_outer2)
combined_r_hf_h_outer3 <- cbind(test_r_hf_h_outer3, r_hf_h_outer3)
combined_r_h_h_outer1 <- cbind(test_r_h_h_outer1, r_h_h_outer1)
combined_r_h_h_outer2 <- cbind(test_r_h_h_outer2, r_h_h_outer2)
combined_r_h_h_outer3 <- cbind(test_r_h_h_outer3, r_h_h_outer3)

combined_s_hf_h <- cbind(test_s_hf_h, s_hf_h)
combined_s_h_h <- cbind(test_s_h_h, s_h_h)

combined_v_hf_h <- cbind(test_v_hf_h, v_hf_h)
combined_v_h_h <- cbind(test_v_h_h, v_h_h)


# Remove redundant columns

# Ferritin predicton
combined_r_hf_f_outer1 <- combined_r_hf_f_outer1[, .(RandID, sex, prediction, fu_outcome)]
combined_r_hf_f_outer2 <- combined_r_hf_f_outer2[, .(RandID, sex, prediction, fu_outcome)]
combined_r_hf_f_outer3 <- combined_r_hf_f_outer3[, .(RandID, sex, prediction, fu_outcome)]
combined_r_h_f_outer1 <- combined_r_h_f_outer1[, .(RandID, sex, prediction, fu_outcome)]
combined_r_h_f_outer2 <- combined_r_h_f_outer2[, .(RandID, sex, prediction, fu_outcome)]
combined_r_h_f_outer3 <- combined_r_h_f_outer3[, .(RandID, sex, prediction, fu_outcome)]

combined_s_hf_f <- combined_s_hf_f[, .(DonorID, sex, prediction, fu_outcome)]
combined_s_h_f <- combined_s_h_f[, .(DonorID, sex, prediction, fu_outcome)]

combined_v_hf_f <- combined_v_hf_f[, .(DonorID, sex, prediction, fu_outcome)]
combined_v_h_f <- combined_v_h_f[, .(DonorID, sex, prediction, fu_outcome)]

# Hemoglobin prediction
combined_r_hf_h_outer1 <- combined_r_hf_h_outer1[, .(RandID, sex, prediction, fu_outcome)]
combined_r_hf_h_outer2 <- combined_r_hf_h_outer2[, .(RandID, sex, prediction, fu_outcome)]
combined_r_hf_h_outer3 <- combined_r_hf_h_outer3[, .(RandID, sex, prediction, fu_outcome)]
combined_r_h_h_outer1 <- combined_r_h_h_outer1[, .(RandID, sex, prediction, fu_outcome)]
combined_r_h_h_outer2 <- combined_r_h_h_outer2[, .(RandID, sex, prediction, fu_outcome)]
combined_r_h_h_outer3 <- combined_r_h_h_outer3[, .(RandID, sex, prediction, fu_outcome)]

combined_s_hf_h <- combined_s_hf_h[, .(DonorID, sex, prediction, fu_outcome)]
combined_s_h_h <- combined_s_h_h[, .(DonorID, sex, prediction, fu_outcome)]

combined_v_hf_h <- combined_v_hf_h[, .(DonorID, sex, prediction, fu_outcome)]
combined_v_h_h <- combined_v_h_h[, .(DonorID, sex, prediction, fu_outcome)]


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
    if (outcome == "observed" & evaluation == "< 12"){ # change to 12
      count = nrow(dt[fu_outcome < log10(12)])
    } else if (outcome == "observed" & evaluation == ">= 12"){
      count = nrow(dt[!(fu_outcome < log10(12))])
    } else if (outcome == "predicted" & evaluation == "< 12"){
      count = nrow(dt[prediction  < log10(12)])
    } else if (outcome == "predicted" & evaluation == ">= 12"){
      count = nrow(dt[!(prediction  < log10(12))])
    }
  }
  
  return(count)
}

# Calculate the number of observed OR predicted return ferritin below or above the threshold

# Results table

######################## Females ############################################################
low_abs_iron_ferritin_f_dt <- data.table(
  Country = c("RISE", "RISE", "RISE", "RISE", "RISE", "RISE",
              "SA", "SA", "US", "US", "NL", "NL"),
  Outer_fold = c("Outer fold 1", "Outer fold 2", "Outer fold 3", 
                 "Outer fold 1", "Outer fold 2", "Outer fold 3",
                 rep("Full dataset", 6)),
  Dataset = c("Hemoglobin and ferritin", "Hemoglobin and ferritin", "Hemoglobin and ferritin",
              "Hemoglobin only", "Hemoglobin only", "Hemoglobin only",
              rep(c("Hemoglobin and ferritin", "Hemoglobin only"), 3)),
  `Observed ferritin < 12 ng/mL` = 0, # initialize as 0
  `Observed ferritin >= 12 ng/mL` = 0,
  `Predicted ferritin < 12 ng/mL` = 0,
  `Predicted ferritin >= 12 ng/mL` = 0)

# Dataset: Hemoglobin and ferritin; Sex: Female; Predict: Ferritin

# RISE 
low_abs_iron_ferritin_f_dt[1, `Observed ferritin < 12 ng/mL` := count_ferritin(combined_r_hf_f_outer1, "observed", "< 12", "female")]
low_abs_iron_ferritin_f_dt[2, `Observed ferritin < 12 ng/mL` := count_ferritin(combined_r_hf_f_outer2, "observed", "< 12", "female")]
low_abs_iron_ferritin_f_dt[3, `Observed ferritin < 12 ng/mL` := count_ferritin(combined_r_hf_f_outer3, "observed", "< 12", "female")]
low_abs_iron_ferritin_f_dt[1, `Observed ferritin >= 12 ng/mL` := count_ferritin(combined_r_hf_f_outer1, "observed", ">= 12", "female")]
low_abs_iron_ferritin_f_dt[2, `Observed ferritin >= 12 ng/mL` := count_ferritin(combined_r_hf_f_outer2, "observed", ">= 12", "female")]
low_abs_iron_ferritin_f_dt[3, `Observed ferritin >= 12 ng/mL` := count_ferritin(combined_r_hf_f_outer3, "observed", ">= 12", "female")]

low_abs_iron_ferritin_f_dt[1, `Predicted ferritin < 12 ng/mL` := count_ferritin(combined_r_hf_f_outer1, "predicted", "< 12", "female")]
low_abs_iron_ferritin_f_dt[2, `Predicted ferritin < 12 ng/mL` := count_ferritin(combined_r_hf_f_outer2, "predicted", "< 12", "female")]
low_abs_iron_ferritin_f_dt[3, `Predicted ferritin < 12 ng/mL` := count_ferritin(combined_r_hf_f_outer3, "predicted", "< 12", "female")]
low_abs_iron_ferritin_f_dt[1, `Predicted ferritin >= 12 ng/mL` := count_ferritin(combined_r_hf_f_outer1, "predicted", ">= 12", "female")]
low_abs_iron_ferritin_f_dt[2, `Predicted ferritin >= 12 ng/mL` := count_ferritin(combined_r_hf_f_outer2, "predicted", ">= 12", "female")]
low_abs_iron_ferritin_f_dt[3, `Predicted ferritin >= 12 ng/mL` := count_ferritin(combined_r_hf_f_outer3, "predicted", ">= 12", "female")]

# SA
low_abs_iron_ferritin_f_dt[7, `Observed ferritin < 12 ng/mL` := count_ferritin(combined_s_hf_f, "observed", "< 12", "female")]
low_abs_iron_ferritin_f_dt[7, `Observed ferritin >= 12 ng/mL` := count_ferritin(combined_s_hf_f, "observed", ">= 12", "female")]
low_abs_iron_ferritin_f_dt[7, `Predicted ferritin < 12 ng/mL` := count_ferritin(combined_s_hf_f, "predicted", "< 12", "female")]
low_abs_iron_ferritin_f_dt[7, `Predicted ferritin >= 12 ng/mL` := count_ferritin(combined_s_hf_f, "predicted", ">= 12", "female")]

# US
low_abs_iron_ferritin_f_dt[9, `Observed ferritin < 12 ng/mL` := count_ferritin(combined_v_hf_f, "observed", "< 12", "female")]
low_abs_iron_ferritin_f_dt[9, `Observed ferritin >= 12 ng/mL` := count_ferritin(combined_v_hf_f, "observed", ">= 12", "female")]
low_abs_iron_ferritin_f_dt[9, `Predicted ferritin < 12 ng/mL` := count_ferritin(combined_v_hf_f, "predicted", "< 12", "female")]
low_abs_iron_ferritin_f_dt[9, `Predicted ferritin >= 12 ng/mL` := count_ferritin(combined_v_hf_f, "predicted", ">= 12", "female")]

# Dataset: Hemoglobin only; Sex: Female; Predict: Ferritin

# RISE
low_abs_iron_ferritin_f_dt[4, `Observed ferritin < 12 ng/mL` := count_ferritin(combined_r_h_f_outer1, "observed", "< 12", "female")]
low_abs_iron_ferritin_f_dt[5, `Observed ferritin < 12 ng/mL` := count_ferritin(combined_r_h_f_outer2, "observed", "< 12", "female")]
low_abs_iron_ferritin_f_dt[6, `Observed ferritin < 12 ng/mL` := count_ferritin(combined_r_h_f_outer3, "observed", "< 12", "female")]
low_abs_iron_ferritin_f_dt[4, `Observed ferritin >= 12 ng/mL` := count_ferritin(combined_r_h_f_outer1, "observed", ">= 12", "female")]
low_abs_iron_ferritin_f_dt[5, `Observed ferritin >= 12 ng/mL` := count_ferritin(combined_r_h_f_outer2, "observed", ">= 12", "female")]
low_abs_iron_ferritin_f_dt[6, `Observed ferritin >= 12 ng/mL` := count_ferritin(combined_r_h_f_outer3, "observed", ">= 12", "female")]

low_abs_iron_ferritin_f_dt[4, `Predicted ferritin < 12 ng/mL` := count_ferritin(combined_r_h_f_outer1, "predicted", "< 12", "female")]
low_abs_iron_ferritin_f_dt[5, `Predicted ferritin < 12 ng/mL` := count_ferritin(combined_r_h_f_outer2, "predicted", "< 12", "female")]
low_abs_iron_ferritin_f_dt[6, `Predicted ferritin < 12 ng/mL` := count_ferritin(combined_r_h_f_outer3, "predicted", "< 12", "female")]
low_abs_iron_ferritin_f_dt[4, `Predicted ferritin >= 12 ng/mL` := count_ferritin(combined_r_h_f_outer1, "predicted", ">= 12", "female")]
low_abs_iron_ferritin_f_dt[5, `Predicted ferritin >= 12 ng/mL` := count_ferritin(combined_r_h_f_outer2, "predicted", ">= 12", "female")]
low_abs_iron_ferritin_f_dt[6, `Predicted ferritin >= 12 ng/mL` := count_ferritin(combined_r_h_f_outer3, "predicted", ">= 12", "female")]

# SA
low_abs_iron_ferritin_f_dt[8, `Observed ferritin < 12 ng/mL` := count_ferritin(combined_s_h_f, "observed", "< 12", "female")]
low_abs_iron_ferritin_f_dt[8, `Observed ferritin >= 12 ng/mL` := count_ferritin(combined_s_h_f, "observed", ">= 12", "female")]
low_abs_iron_ferritin_f_dt[8, `Predicted ferritin < 12 ng/mL` := count_ferritin(combined_s_h_f, "predicted", "< 12", "female")]
low_abs_iron_ferritin_f_dt[8, `Predicted ferritin >= 12 ng/mL` := count_ferritin(combined_s_h_f, "predicted", ">= 12", "female")]

# US
low_abs_iron_ferritin_f_dt[10, `Observed ferritin < 12 ng/mL` := count_ferritin(combined_v_h_f, "observed", "< 12", "female")]
low_abs_iron_ferritin_f_dt[10, `Observed ferritin >= 12 ng/mL` := count_ferritin(combined_v_h_f, "observed", ">= 12", "female")]
low_abs_iron_ferritin_f_dt[10, `Predicted ferritin < 12 ng/mL` := count_ferritin(combined_v_h_f, "predicted", "< 12", "female")]
low_abs_iron_ferritin_f_dt[10, `Predicted ferritin >= 12 ng/mL` := count_ferritin(combined_v_h_f, "predicted", ">= 12", "female")]


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

print(low_abs_iron_ferritin_f_dt)

# Calculate the number of BOTH observed AND predicted return ferritin below or above the threshold

# Low ferritin cut-off at 12 ng/mL
cross_low_ferritin_12_f_dt <- data.table(
  `Predicted ferritin` = c("< 12 ng/mL", ">= 12 ng/mL"),
  `Observed ferritin < 12 ng/mL` = 0,
  `Observed ferritin >= 12 ng/mL` = 0,
  Country = c(rep("RISE", 12), 
              rep("SA", 4),
              rep("US", 4),
              rep("NL", 4)),
  Outer_fold = c(rep(c("Outer fold 1", "Outer fold 1", "Outer fold 2", 
                       "Outer fold 2", "Outer fold 3", "Outer fold 3"),2),
                 rep("Full dataset", 12)),
  Dataset = c(rep("Hemoglobin and ferritin",6),
              rep("Hemoglobin only",6),
              rep("Hemoglobin and ferritin", 2),
              rep("Hemoglobin only", 2),
              rep("Hemoglobin and ferritin", 2),
              rep("Hemoglobin only", 2),
              rep("Hemoglobin and ferritin", 2),
              rep("Hemoglobin only", 2))
)

# Low ferritin cut-off at 25 ng/mL
cross_low_ferritin_25_f_dt <- data.table(
  `Predicted ferritin` = c("< 25 ng/mL", ">= 25 ng/mL"),
  `Observed ferritin < 25 ng/mL` = 0,
  `Observed ferritin >= 25 ng/mL` = 0,
  Country = c(rep("RISE", 12), 
              rep("SA", 4),
              rep("US", 4),
              rep("NL", 4)),
  Outer_fold = c(rep(c("Outer fold 1", "Outer fold 1", "Outer fold 2", 
                       "Outer fold 2", "Outer fold 3", "Outer fold 3"),2),
                 rep("Full dataset", 12)),
  Dataset = c(rep("Hemoglobin and ferritin",6),
              rep("Hemoglobin only",6),
              rep("Hemoglobin and ferritin", 2),
              rep("Hemoglobin only", 2),
              rep("Hemoglobin and ferritin", 2),
              rep("Hemoglobin only", 2),
              rep("Hemoglobin and ferritin", 2),
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
cross_ferritin_cal <- function(cross_dt, predicted_ferritin, country, outer_fold, train_dt, observed_column, dt2, 
                               evaluation_obs, evaluation_pred, sex) {
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
    
    if (country=="RISE"){
      if (outer_fold=="Outer fold 1"){
        if (train_dataset=="Hemoglobin and ferritin"){
          dataset <- combined_r_hf_f_outer1
        } else if (train_dataset=="Hemoglobin only"){
          dataset <- combined_r_h_f_outer1
        }
      } else if (outer_fold=="Outer fold 2"){
        if (train_dataset=="Hemoglobin and ferritin"){
          dataset <- combined_r_hf_f_outer2
        } else if (train_dataset=="Hemoglobin only"){
          dataset <- combined_r_h_f_outer2
        }
      } else if (outer_fold=="Outer fold 3"){
        if (train_dataset=="Hemoglobin and ferritin"){
          dataset <- combined_r_hf_f_outer3
        } else if (train_dataset=="Hemoglobin only"){
          dataset <- combined_r_h_f_outer3
        }
      }
    } else if (country=="SA"){
      if (train_dataset=="Hemoglobin and ferritin"){
        dataset <- combined_s_hf_f
      } else if (train_dataset=="Hemoglobin only"){
        dataset <- combined_s_h_f
      } 
    } else if (country=="US"){
      if (train_dataset=="Hemoglobin and ferritin"){
        dataset <- combined_v_hf_f
      } else if (train_dataset=="Hemoglobin only"){
        dataset <- combined_v_h_f
      } 
    } else{
      break # add NL results back
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


######################## Males ##################################################

low_abs_iron_ferritin_m_dt <- data.table(
  Country = c("RISE", "RISE", "RISE", "RISE", "RISE", "RISE",
              "SA", "SA", "US", "US", "NL", "NL"),
  Outer_fold = c("Outer fold 1", "Outer fold 2", "Outer fold 3", 
                 "Outer fold 1", "Outer fold 2", "Outer fold 3",
                 rep("Full dataset", 6)),
  Dataset = c("Hemoglobin and ferritin", "Hemoglobin and ferritin", "Hemoglobin and ferritin",
              "Hemoglobin only", "Hemoglobin only", "Hemoglobin only",
              rep(c("Hemoglobin and ferritin", "Hemoglobin only"), 3)),
  `Observed ferritin < 12 ng/mL` = 0, # initialize as 0; change ferritin cutoff to 12 ng/mL
  `Observed ferritin >= 12 ng/mL` = 0,
  `Predicted ferritin < 12 ng/mL` = 0,
  `Predicted ferritin >= 12 ng/mL` = 0)

# Dataset: Hemoglobin and ferritin; Sex: Male; Predict: Ferritin

# RISE 
low_abs_iron_ferritin_m_dt[1, `Observed ferritin < 12 ng/mL` := count_ferritin(combined_r_hf_f_outer1, "observed", "< 12", "male")]
low_abs_iron_ferritin_m_dt[2, `Observed ferritin < 12 ng/mL` := count_ferritin(combined_r_hf_f_outer2, "observed", "< 12", "male")]
low_abs_iron_ferritin_m_dt[3, `Observed ferritin < 12 ng/mL` := count_ferritin(combined_r_hf_f_outer3, "observed", "< 12", "male")]
low_abs_iron_ferritin_m_dt[1, `Observed ferritin >= 12 ng/mL` := count_ferritin(combined_r_hf_f_outer1, "observed", ">= 12", "male")]
low_abs_iron_ferritin_m_dt[2, `Observed ferritin >= 12 ng/mL` := count_ferritin(combined_r_hf_f_outer2, "observed", ">= 12", "male")]
low_abs_iron_ferritin_m_dt[3, `Observed ferritin >= 12 ng/mL` := count_ferritin(combined_r_hf_f_outer3, "observed", ">= 12", "male")]

low_abs_iron_ferritin_m_dt[1, `Predicted ferritin < 12 ng/mL` := count_ferritin(combined_r_hf_f_outer1, "predicted", "< 12", "male")]
low_abs_iron_ferritin_m_dt[2, `Predicted ferritin < 12 ng/mL` := count_ferritin(combined_r_hf_f_outer2, "predicted", "< 12", "male")]
low_abs_iron_ferritin_m_dt[3, `Predicted ferritin < 12 ng/mL` := count_ferritin(combined_r_hf_f_outer3, "predicted", "< 12", "male")]
low_abs_iron_ferritin_m_dt[1, `Predicted ferritin >= 12 ng/mL` := count_ferritin(combined_r_hf_f_outer1, "predicted", ">= 12", "male")]
low_abs_iron_ferritin_m_dt[2, `Predicted ferritin >= 12 ng/mL` := count_ferritin(combined_r_hf_f_outer2, "predicted", ">= 12", "male")]
low_abs_iron_ferritin_m_dt[3, `Predicted ferritin >= 12 ng/mL` := count_ferritin(combined_r_hf_f_outer3, "predicted", ">= 12", "male")]

# SA
low_abs_iron_ferritin_m_dt[7, `Observed ferritin < 12 ng/mL` := count_ferritin(combined_s_hf_f, "observed", "< 12", "male")]
low_abs_iron_ferritin_m_dt[7, `Observed ferritin >= 12 ng/mL` := count_ferritin(combined_s_hf_f, "observed", ">= 12", "male")]
low_abs_iron_ferritin_m_dt[7, `Predicted ferritin < 12 ng/mL` := count_ferritin(combined_s_hf_f, "predicted", "< 12", "male")]
low_abs_iron_ferritin_m_dt[7, `Predicted ferritin >= 12 ng/mL` := count_ferritin(combined_s_hf_f, "predicted", ">= 12", "male")]

# US
low_abs_iron_ferritin_m_dt[9, `Observed ferritin < 12 ng/mL` := count_ferritin(combined_v_hf_f, "observed", "< 12", "male")]
low_abs_iron_ferritin_m_dt[9, `Observed ferritin >= 12 ng/mL` := count_ferritin(combined_v_hf_f, "observed", ">= 12", "male")]
low_abs_iron_ferritin_m_dt[9, `Predicted ferritin < 12 ng/mL` := count_ferritin(combined_v_hf_f, "predicted", "< 12", "male")]
low_abs_iron_ferritin_m_dt[9, `Predicted ferritin >= 12 ng/mL` := count_ferritin(combined_v_hf_f, "predicted", ">= 12", "male")]

# Dataset: Hemoglobin only; Sex: Male; Predict: Ferritin

# RISE
low_abs_iron_ferritin_m_dt[4, `Observed ferritin < 12 ng/mL` := count_ferritin(combined_r_h_f_outer1, "observed", "< 12", "male")]
low_abs_iron_ferritin_m_dt[5, `Observed ferritin < 12 ng/mL` := count_ferritin(combined_r_h_f_outer2, "observed", "< 12", "male")]
low_abs_iron_ferritin_m_dt[6, `Observed ferritin < 12 ng/mL` := count_ferritin(combined_r_h_f_outer3, "observed", "< 12", "male")]
low_abs_iron_ferritin_m_dt[4, `Observed ferritin >= 12 ng/mL` := count_ferritin(combined_r_h_f_outer1, "observed", ">= 12", "male")]
low_abs_iron_ferritin_m_dt[5, `Observed ferritin >= 12 ng/mL` := count_ferritin(combined_r_h_f_outer2, "observed", ">= 12", "male")]
low_abs_iron_ferritin_m_dt[6, `Observed ferritin >= 12 ng/mL` := count_ferritin(combined_r_h_f_outer3, "observed", ">= 12", "male")]

low_abs_iron_ferritin_m_dt[4, `Predicted ferritin < 12 ng/mL` := count_ferritin(combined_r_h_f_outer1, "predicted", "< 12", "male")]
low_abs_iron_ferritin_m_dt[5, `Predicted ferritin < 12 ng/mL` := count_ferritin(combined_r_h_f_outer2, "predicted", "< 12", "male")]
low_abs_iron_ferritin_m_dt[6, `Predicted ferritin < 12 ng/mL` := count_ferritin(combined_r_h_f_outer3, "predicted", "< 12", "male")]
low_abs_iron_ferritin_m_dt[4, `Predicted ferritin >= 12 ng/mL` := count_ferritin(combined_r_h_f_outer1, "predicted", ">= 12", "male")]
low_abs_iron_ferritin_m_dt[5, `Predicted ferritin >= 12 ng/mL` := count_ferritin(combined_r_h_f_outer2, "predicted", ">= 12", "male")]
low_abs_iron_ferritin_m_dt[6, `Predicted ferritin >= 12 ng/mL` := count_ferritin(combined_r_h_f_outer3, "predicted", ">= 12", "male")]

# SA
low_abs_iron_ferritin_m_dt[8, `Observed ferritin < 12 ng/mL` := count_ferritin(combined_s_h_f, "observed", "< 12", "male")]
low_abs_iron_ferritin_m_dt[8, `Observed ferritin >= 12 ng/mL` := count_ferritin(combined_s_h_f, "observed", ">= 12", "male")]
low_abs_iron_ferritin_m_dt[8, `Predicted ferritin < 12 ng/mL` := count_ferritin(combined_s_h_f, "predicted", "< 12", "male")]
low_abs_iron_ferritin_m_dt[8, `Predicted ferritin >= 12 ng/mL` := count_ferritin(combined_s_h_f, "predicted", ">= 12", "male")]

# US
low_abs_iron_ferritin_m_dt[10, `Observed ferritin < 12 ng/mL` := count_ferritin(combined_v_h_f, "observed", "< 12", "male")]
low_abs_iron_ferritin_m_dt[10, `Observed ferritin >= 12 ng/mL` := count_ferritin(combined_v_h_f, "observed", ">= 12", "male")]
low_abs_iron_ferritin_m_dt[10, `Predicted ferritin < 12 ng/mL` := count_ferritin(combined_v_h_f, "predicted", "< 12", "male")]
low_abs_iron_ferritin_m_dt[10, `Predicted ferritin >= 12 ng/mL` := count_ferritin(combined_v_h_f, "predicted", ">= 12", "male")]


# Further calculation
low_abs_iron_ferritin_m_dt[, `Total` := 
                             `Observed ferritin < 12 ng/mL` + `Observed ferritin >= 12 ng/mL`]
low_abs_iron_ferritin_m_dt[, `% Observed ferritin < 12` := 
                             paste0(round(`Observed ferritin < 12 ng/mL` / `Total` * 100,1), "%")]
low_abs_iron_ferritin_m_dt[, `% Observed ferritin >= 12` := 
                             paste0(round(`Observed ferritin >= 12 ng/mL` / `Total` * 100,1), "%")]
low_abs_iron_ferritin_m_dt[, `% Predicted ferritin < 12` := 
                             paste0(round(`Predicted ferritin < 12 ng/mL` / `Total` * 100,1), "%")]
low_abs_iron_ferritin_m_dt[, `% Predicted ferritin >= 12` := 
                             paste0(round(`Predicted ferritin >= 12 ng/mL` / `Total` * 100,1), "%")]


# Calculate the number of BOTH observed AND predicted return ferritin below or above the threshold

cross_low_ferritin_12_m_dt <- data.table(
  `Predicted ferritin` = c("< 12 ng/mL", ">= 12 ng/mL"),
  `Observed ferritin < 12 ng/mL` = 0,
  `Observed ferritin >= 12 ng/mL` = 0,
  Country = c(rep("RISE", 12), 
              rep("SA", 4),
              rep("US", 4),
              rep("NL", 4)),
  Outer_fold = c(rep(c("Outer fold 1", "Outer fold 1", "Outer fold 2", 
                       "Outer fold 2", "Outer fold 3", "Outer fold 3"),2),
                 rep("Full dataset", 12)),
  Dataset = c(rep("Hemoglobin and ferritin",6),
              rep("Hemoglobin only",6),
              rep("Hemoglobin and ferritin", 2),
              rep("Hemoglobin only", 2),
              rep("Hemoglobin and ferritin", 2),
              rep("Hemoglobin only", 2),
              rep("Hemoglobin and ferritin", 2),
              rep("Hemoglobin only", 2))
)

cross_low_ferritin_25_m_dt <- data.table(
  `Predicted ferritin` = c("< 25 ng/mL", ">= 25 ng/mL"),
  `Observed ferritin < 25 ng/mL` = 0,
  `Observed ferritin >= 25 ng/mL` = 0,
  Country = c(rep("RISE", 12), 
              rep("SA", 4),
              rep("US", 4),
              rep("NL", 4)),
  Outer_fold = c(rep(c("Outer fold 1", "Outer fold 1", "Outer fold 2", 
                       "Outer fold 2", "Outer fold 3", "Outer fold 3"),2),
                 rep("Full dataset", 12)),
  Dataset = c(rep("Hemoglobin and ferritin",6),
              rep("Hemoglobin only",6),
              rep("Hemoglobin and ferritin", 2),
              rep("Hemoglobin only", 2),
              rep("Hemoglobin and ferritin", 2),
              rep("Hemoglobin only", 2),
              rep("Hemoglobin and ferritin", 2),
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
    
    if (country=="RISE"){
      if (outer_fold=="Outer fold 1"){
        if (train_dataset=="Hemoglobin and ferritin"){
          dataset <- combined_r_hf_f_outer1
        } else if (train_dataset=="Hemoglobin only"){
          dataset <- combined_r_h_f_outer1
        }
      } else if (outer_fold=="Outer fold 2"){
        if (train_dataset=="Hemoglobin and ferritin"){
          dataset <- combined_r_hf_f_outer2
        } else if (train_dataset=="Hemoglobin only"){
          dataset <- combined_r_h_f_outer2
        }
      } else if (outer_fold=="Outer fold 3"){
        if (train_dataset=="Hemoglobin and ferritin"){
          dataset <- combined_r_hf_f_outer3
        } else if (train_dataset=="Hemoglobin only"){
          dataset <- combined_r_h_f_outer3
        }
      }
    } else if (country=="SA"){
      if (train_dataset=="Hemoglobin and ferritin"){
        dataset <- combined_s_hf_f
      } else if (train_dataset=="Hemoglobin only"){
        dataset <- combined_s_h_f
      } 
    } else if (country=="US"){
      if (train_dataset=="Hemoglobin and ferritin"){
        dataset <- combined_v_hf_f
      } else if (train_dataset=="Hemoglobin only"){
        dataset <- combined_v_h_f
      } 
    } else{
      break # add NL results back
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



######################## All ##################################################


# Calculate the number of BOTH observed AND predicted return ferritin below or above the threshold

cross_low_ferritin_12_dt <- data.table(
  `Predicted ferritin` = c("< 12 ng/mL", ">= 12 ng/mL"),
  `Observed ferritin < 12 ng/mL` = 0,
  `Observed ferritin >= 12 ng/mL` = 0,
  Country = c(rep("RISE", 12), 
              rep("SA", 4),
              rep("US", 4),
              rep("NL", 4)),
  Outer_fold = c(rep(c("Outer fold 1", "Outer fold 1", "Outer fold 2", 
                       "Outer fold 2", "Outer fold 3", "Outer fold 3"),2),
                 rep("Full dataset", 12)),
  Dataset = c(rep("Hemoglobin and ferritin",6),
              rep("Hemoglobin only",6),
              rep("Hemoglobin and ferritin", 2),
              rep("Hemoglobin only", 2),
              rep("Hemoglobin and ferritin", 2),
              rep("Hemoglobin only", 2),
              rep("Hemoglobin and ferritin", 2),
              rep("Hemoglobin only", 2))
)

cross_low_ferritin_25_dt <- data.table(
  `Predicted ferritin` = c("< 25 ng/mL", ">= 25 ng/mL"),
  `Observed ferritin < 25 ng/mL` = 0,
  `Observed ferritin >= 25 ng/mL` = 0,
  Country = c(rep("RISE", 12), 
              rep("SA", 4),
              rep("US", 4),
              rep("NL", 4)),
  Outer_fold = c(rep(c("Outer fold 1", "Outer fold 1", "Outer fold 2", 
                       "Outer fold 2", "Outer fold 3", "Outer fold 3"),2),
                 rep("Full dataset", 12)),
  Dataset = c(rep("Hemoglobin and ferritin",6),
              rep("Hemoglobin only",6),
              rep("Hemoglobin and ferritin", 2),
              rep("Hemoglobin only", 2),
              rep("Hemoglobin and ferritin", 2),
              rep("Hemoglobin only", 2),
              rep("Hemoglobin and ferritin", 2),
              rep("Hemoglobin only", 2))
)



# For loop to iterate through rows and update the table
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
    
    if (country=="RISE"){
      if (outer_fold=="Outer fold 1"){
        if (train_dataset=="Hemoglobin and ferritin"){
          dataset <- combined_r_hf_f_outer1
        } else if (train_dataset=="Hemoglobin only"){
          dataset <- combined_r_h_f_outer1
        }
      } else if (outer_fold=="Outer fold 2"){
        if (train_dataset=="Hemoglobin and ferritin"){
          dataset <- combined_r_hf_f_outer2
        } else if (train_dataset=="Hemoglobin only"){
          dataset <- combined_r_h_f_outer2
        }
      } else if (outer_fold=="Outer fold 3"){
        if (train_dataset=="Hemoglobin and ferritin"){
          dataset <- combined_r_hf_f_outer3
        } else if (train_dataset=="Hemoglobin only"){
          dataset <- combined_r_h_f_outer3
        }
      }
    } else if (country=="SA"){
      if (train_dataset=="Hemoglobin and ferritin"){
        dataset <- combined_s_hf_f
      } else if (train_dataset=="Hemoglobin only"){
        dataset <- combined_s_h_f
      } 
    } else if (country=="US"){
      if (train_dataset=="Hemoglobin and ferritin"){
        dataset <- combined_v_hf_f
      } else if (train_dataset=="Hemoglobin only"){
        dataset <- combined_v_h_f
      } 
    } else{
      break # add NL results back
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




# _________________________Additional analysis 1 finished _________________________________________________________________________


# Additional analysis 2: To calculate RMSPE after averaging RMSPE for each individual #############################################

# Change column names of RISE data
setnames(combined_r_hf_f_outer1, "RandID", "DonorID")
setnames(combined_r_hf_f_outer2, "RandID", "DonorID")
setnames(combined_r_hf_f_outer3, "RandID", "DonorID")
setnames(combined_r_h_f_outer1, "RandID", "DonorID")
setnames(combined_r_h_f_outer2, "RandID", "DonorID")
setnames(combined_r_h_f_outer3, "RandID", "DonorID")

setnames(combined_r_hf_h_outer1, "RandID", "DonorID")
setnames(combined_r_hf_h_outer2, "RandID", "DonorID")
setnames(combined_r_hf_h_outer3, "RandID", "DonorID")
setnames(combined_r_h_h_outer1, "RandID", "DonorID")
setnames(combined_r_h_h_outer2, "RandID", "DonorID")
setnames(combined_r_h_h_outer3, "RandID", "DonorID")

# Calculate RMSPE per donor

# Ferritin prediction
RMSPE_by_donor_pred_f_dt <- data.table(
  Country = c("RISE", "RISE", "RISE", "RISE", "RISE", "RISE",
              "SA", "SA", "US", "US", "NL", "NL"),
  Outer_fold = c("Outer fold 1", "Outer fold 2", "Outer fold 3", 
                 "Outer fold 1", "Outer fold 2", "Outer fold 3",
                 rep("Full dataset", 6)),
  Dataset = c("Hemoglobin and ferritin", "Hemoglobin and ferritin", "Hemoglobin and ferritin",
              "Hemoglobin only", "Hemoglobin only", "Hemoglobin only",
              rep(c("Hemoglobin and ferritin", "Hemoglobin only"), 3)),
  Outcome = c(rep("Ferritin", 12)),
  Unadj_RMSPE = c(13.90, 16.02, 14.62, # Directly extracted from figure 3 external validation plot
                  31.41, 26.48, 24.43,
                  18.94, 27.55,
                  14.88, 22.92,
                  15.06, 19.09),
  PP_RMSPE = 0)


# Helper function: To calculate average RMSPE per donor first and then take the average RMSPE across all donors
calc_rmspe_per_donor <- function(data, epsilon = EPSILON){
  
  # Calculate Percent Error
  data[, PercentError := ((fu_outcome - prediction) / (fu_outcome + epsilon)) * 100]
  
  # Square the Percent Error
  data[, SquaredPercentError := PercentError^2]
  
  # Compute Mean Squared Percent Error (MSPE) per Donor
  donor_rmspe <- data[, .(MeanSquaredPercentError = mean(SquaredPercentError)), by = DonorID]
  
  # Calculate RMSPE for each Donor
  donor_rmspe[, RMSPE := sqrt(MeanSquaredPercentError)]
  
  # Compute the Average RMSPE across all Donors
  overall_average_rmspe <- round(mean(donor_rmspe$RMSPE),2)
  
  return(overall_average_rmspe)
}

pred_f_datasets <- list(
  list(Country = "RISE", Outer_fold = "Outer fold 1", Dataset = "Hemoglobin and ferritin", data = combined_r_hf_f_outer1),
  list(Country = "RISE", Outer_fold = "Outer fold 2", Dataset = "Hemoglobin and ferritin", data = combined_r_hf_f_outer2),
  list(Country = "RISE", Outer_fold = "Outer fold 3", Dataset = "Hemoglobin and ferritin", data = combined_r_hf_f_outer3),
  list(Country = "RISE", Outer_fold = "Outer fold 1", Dataset = "Hemoglobin only", data = combined_r_h_f_outer1),
  list(Country = "RISE", Outer_fold = "Outer fold 2", Dataset = "Hemoglobin only", data = combined_r_h_f_outer2),
  list(Country = "RISE", Outer_fold = "Outer fold 3", Dataset = "Hemoglobin only", data = combined_r_h_f_outer3),
  list(Country = "SA", Outer_fold = "Full dataset", Dataset = "Hemoglobin and ferritin", data = combined_s_hf_f),
  list(Country = "SA", Outer_fold = "Full dataset", Dataset = "Hemoglobin only", data = combined_s_h_f),
  list(Country = "US", Outer_fold = "Full dataset", Dataset = "Hemoglobin and ferritin", data = combined_v_hf_f),
  list(Country = "US", Outer_fold = "Full dataset", Dataset = "Hemoglobin only", data = combined_v_h_f)
  # list(Country = "NL", Outer_fold = "Full dataset", Dataset = "Hemoglobin and ferritin", data = combined_sq_hf_f),
  # list(Country = "NL", Outer_fold = "Full dataset", Dataset = "Hemoglobin only", data = combined_sq_h_f)
)

# Loop through the datasets to update the table
for (ds in pred_f_datasets) {
  RMSPE_by_donor_pred_f_dt[
    Country == ds$Country & Outer_fold == ds$Outer_fold & Dataset == ds$Dataset,
    PP_RMSPE := calc_rmspe_per_donor(ds$data)
  ]
}


# Hemoglobin prediction

RMSPE_by_donor_pred_h_dt <- data.table(
  Country = c("RISE", "RISE", "RISE", "RISE", "RISE", "RISE",
              "SA", "SA", "US", "US", "NL", "NL"),
  Outer_fold = c("Outer fold 1", "Outer fold 2", "Outer fold 3", 
                 "Outer fold 1", "Outer fold 2", "Outer fold 3",
                 rep("Full dataset", 6)),
  Dataset = c("Hemoglobin and ferritin", "Hemoglobin and ferritin", "Hemoglobin and ferritin",
              "Hemoglobin only", "Hemoglobin only", "Hemoglobin only",
              rep(c("Hemoglobin and ferritin", "Hemoglobin only"), 3)),
  Outcome = c(rep("Hemoglobin", 12)),
  Unadj_RMSPE = c(7.35, 6.50, 6.49,
                  6.58, 6.83, 6.97,
                  6.65, 7.3,
                  4.59, 7.19,
                  6.27, 5.87),
  PP_RMSPE = 0)

print(RMSPE_by_donor_pred_h_dt)

pred_h_datasets <- list(
  list(Country = "RISE", Outer_fold = "Outer fold 1", Dataset = "Hemoglobin and ferritin", data = combined_r_hf_h_outer1),
  list(Country = "RISE", Outer_fold = "Outer fold 2", Dataset = "Hemoglobin and ferritin", data = combined_r_hf_h_outer2),
  list(Country = "RISE", Outer_fold = "Outer fold 3", Dataset = "Hemoglobin and ferritin", data = combined_r_hf_h_outer3),
  list(Country = "RISE", Outer_fold = "Outer fold 1", Dataset = "Hemoglobin only", data = combined_r_h_h_outer1),
  list(Country = "RISE", Outer_fold = "Outer fold 2", Dataset = "Hemoglobin only", data = combined_r_h_h_outer2),
  list(Country = "RISE", Outer_fold = "Outer fold 3", Dataset = "Hemoglobin only", data = combined_r_h_h_outer3),
  list(Country = "SA", Outer_fold = "Full dataset", Dataset = "Hemoglobin and ferritin", data = combined_s_hf_h),
  list(Country = "SA", Outer_fold = "Full dataset", Dataset = "Hemoglobin only", data = combined_s_h_h),
  list(Country = "US", Outer_fold = "Full dataset", Dataset = "Hemoglobin and ferritin", data = combined_v_hf_h),
  list(Country = "US", Outer_fold = "Full dataset", Dataset = "Hemoglobin only", data = combined_v_h_h)
  # list(Country = "NL", Outer_fold = "Full dataset", Dataset = "Hemoglobin and ferritin", data = combined_sq_hf_f),
  # list(Country = "NL", Outer_fold = "Full dataset", Dataset = "Hemoglobin only", data = combined_sq_h_f)
)

# Loop through the datasets to update the table
for (ds in pred_h_datasets) {
  RMSPE_by_donor_pred_h_dt[
    Country == ds$Country & Outer_fold == ds$Outer_fold & Dataset == ds$Dataset,
    PP_RMSPE := calc_rmspe_per_donor(ds$data)
  ]
}


# _________________________Additional analysis 2 finished _________________________________________________________________________

# Additional analysis 3: To compare the tuning results from regular RMSPE from the following models: #########################
# 1) the basic linear regression model (elastic net w/ both penalty set to 0),
# 2) the top elastic model
# 3) the top model

en_base_mod_tune_rmspe_h_h <- fread("./3_intermediate/tune_results/base_mod_tune_predict_hgb_data_hgb_only_EN_params1_to_1051.csv")
en_base_mod_tune_rmspe_hf_h <- fread("./3_intermediate/tune_results/base_mod_tune_predict_hgb_data_hgb_ferr_EN_params1_to_1051.csv")
en_base_mod_tune_rmspe_h_f <- fread("./3_intermediate/tune_results/base_mod_tune_predict_ferr_data_hgb_only_EN_params1_to_1051.csv")
en_base_mod_tune_rmspe_hf_f <- fread("./3_intermediate/tune_results/base_mod_tune_predict_ferr_data_hgb_ferr_EN_params1_to_1051.csv")

# The basic linear regression model
en_h_h_rmspe_lr <- en_base_mod_tune_rmspe_h_h[hyperparam_idx == 1, "rmspe_mean"]
en_hf_h_rmspe_lr <- en_base_mod_tune_rmspe_hf_h[hyperparam_idx == 1, "rmspe_mean"]
en_h_f_rmspe_lr <- en_base_mod_tune_rmspe_h_f[hyperparam_idx == 1, "rmspe_mean"]
en_hf_f_rmspe_lr <- en_base_mod_tune_rmspe_hf_f[hyperparam_idx == 1, "rmspe_mean"]

# The top elastic net model
en_h_h_top <- en_base_mod_tune_rmspe_h_h[rmspe_mean == min(en_base_mod_tune_rmspe_h_h[["rmspe_mean"]])]
en_h_h_rmspe_top <- en_h_h_top[["rmspe_mean"]]

en_hf_h_top <- en_base_mod_tune_rmspe_hf_h[rmspe_mean == min(en_base_mod_tune_rmspe_hf_h[["rmspe_mean"]])]
en_hf_h_rmspe_top <- en_hf_h_top[["rmspe_mean"]]

en_h_f_top <- en_base_mod_tune_rmspe_h_f[rmspe_mean == min(en_base_mod_tune_rmspe_h_f[["rmspe_mean"]])]
en_h_f_rmspe_top <- en_h_f_top[["rmspe_mean"]]

en_hf_f_top <- en_base_mod_tune_rmspe_hf_f[rmspe_mean == min(en_base_mod_tune_rmspe_hf_f[["rmspe_mean"]])]
en_hf_f_rmspe_top <- en_hf_f_top[["rmspe_mean"]]

# Report results:

print(paste0("Predicting hemoglobin with index Hb only: "))
print(paste0("The basic linear regression model: "))
print(en_h_h_rmspe_lr)

print(paste0("The top elastic net model: "))
print(en_h_h_rmspe_top)

print("###################################################")
print(paste0("Predicting hemoglobin with index Hb and ferritin: "))
print(paste0("The basic linear regression model: "))
print(en_hf_h_rmspe_lr)

print(paste0("The top elastic net model: "))
print(en_hf_h_rmspe_top)

print("###################################################")
print(paste0("Predicting ferritin with index Hb only: "))
print(paste0("The basic linear regression model: "))
print(en_h_f_rmspe_lr)

print(paste0("The top elastic net model: "))
print(en_h_f_rmspe_top)


print("###################################################")
print(paste0("Predicting ferritin with index Hb and ferritin: "))
print(paste0("The basic linear regression model: "))
print(en_hf_f_rmspe_lr)

print(paste0("The top elastic net model: "))
print(en_hf_f_rmspe_top)

# _________________________Additional analysis 3 finished _________________________________________________________________________

# Additional analysis 4: To check the normal distribution of hemoglobin and ferritin in RISE training data ###############

# load RISE data
dat_paths <- c("./3_intermediate/private/hgb_ferr_rise.csv",
               "./3_intermediate/private/hgb_only_rise.csv")

r_hf <- fread(dat_paths[1])  
r_h <- fread(dat_paths[2])  

# Select needed columns
r_h <- r_h[, .(RandID, fu_hgb, fu_ferritin, fu_log_ferritin)]
r_hf <- r_hf[, .(RandID, fu_hgb, fu_ferritin, fu_log_ferritin)]

# Plot histogram for fu_hgb, fu_ferritin, fu_log_ferritin

# Hb only dataset
r_h_h_hist <- ggplot(r_h, aes(x=fu_hgb))+
  geom_histogram(binwidth=0.3, color="black", fill="#619CFF")+
  labs(title="Hemoglobin", x="Follow-up hemoglobin", y="Frequency")

r_h_f_hist <- ggplot(r_h, aes(x=fu_ferritin))+
  geom_histogram(binwidth = 20, color="black", fill="#F8766D")+
  labs(title="Ferritin", x="Follow-up ferritin", y="Frequency")

r_h_logf_hist <- ggplot(r_h, aes(x=fu_log_ferritin))+
  geom_histogram(binwidth=0.05, color="black", fill="#00BA38")+
  labs(title="Log ferritin", x="Follow-up log ferritin", y="Frequency")


# Combine the plots into a single panel with a title
r_h_hist <- (r_h_h_hist | r_h_logf_hist | r_h_f_hist) + 
  plot_annotation(title = "Dataset: Hemoglobin only")

# Hb and ferritin dataset
r_hf_h_hist <- ggplot(r_hf, aes(x=fu_hgb))+
  geom_histogram(binwidth=0.3, color="black", fill="#619CFF")+
  labs(title="Hemoglobin", x="Follow-up hemoglobin", y="Frequency")

r_hf_f_hist <- ggplot(r_hf, aes(x=fu_ferritin))+
  geom_histogram(binwidth = 20, color="black", fill="#F8766D")+
  labs(title="Ferritin", x="Follow-up ferritin", y="Frequency")

r_hf_logf_hist <- ggplot(r_hf, aes(x=fu_log_ferritin))+
  geom_histogram(binwidth=0.05, color="black", fill="#00BA38")+
  labs(title="Log ferritin", x="Follow-up log ferritin", y="Frequency")


# Combine the plots into a single panel with a title
r_hf_hist <- (r_hf_h_hist | r_hf_logf_hist | r_hf_f_hist) + 
  plot_annotation(title = "Dataset: Hemoglobin and ferritin")

# Combine the two plots

r_hist <- (r_hf_hist / r_h_hist) +
  plot_annotation(
    title = "Distribution of predicted biomarkers",
    subtitle = "Top: Hemoglobin and Ferritin Dataset\nBottom: Hemoglobin Only Dataset"
  )  
r_hist

png(filename = "./4_output/rise_biomarkers_histogram.png", width = 10, height = 10, units = "in", res = 300)
grid.draw(r_hist)
dev.off()

svg(filename = "./4_output/rise_biomarkers_histogram.svg", width = 10,, height = 10)
grid.draw(r_hist)
dev.off()

# _________________________Additional analysis 4 finished _________________________________________________________________________
