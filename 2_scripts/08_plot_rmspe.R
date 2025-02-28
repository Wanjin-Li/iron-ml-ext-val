# This script plots Figure S6: validation results for RISE (internal validation - regular cross validation), SANBS, Vitalant, Sanquin.

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

# RISE (internal validation - regular cross validation; likely overfit) ----

rise_top_mods_hf_h <- fread("./3_intermediate/ensemble/updates/top_mods_assess_ensemble1_hgb_ferr_predict_hgb_full.csv")
rise_top_mods_h_h <- fread("./3_intermediate/ensemble/updates/top_mods_assess_ensemble1_hgb_only_predict_hgb_full.csv")
rise_top_mods_hf_f <- fread("./3_intermediate/ensemble/updates/top_mods_assess_CB.1222_hgb_ferr_predict_ferr_full.csv")
rise_top_mods_h_f <- fread("./3_intermediate/ensemble/updates/top_mods_assess_CB.1171_hgb_only_predict_ferr_full.csv")

r_hf_h_rmspe <- rise_top_mods_hf_h[modelID=="Ensemble1"]$rmspe_mean
r_h_h_rmspe <- rise_top_mods_h_h[modelID=="Ensemble1"]$rmspe_mean
r_hf_f_rmspe <- rise_top_mods_hf_f[modelID=="CB.1222"]$rmspe_mean
r_h_f_rmspe <- rise_top_mods_h_f[modelID=="CB.1171"]$rmspe_mean



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
    Cohort == "RISE" ~ "Internal (CV)",
    Cohort == "SANBS" ~ "SA",
    Cohort == "Sanquin" ~ "NL",
    Cohort == "Vitalant" ~ "US",
    TRUE ~ Cohort
  ))

# Print the modified dataframe
print(main_df)

main_df$Cohort <- factor(main_df$Cohort, levels = c("Internal (CV)","US", "SA", "NL"))
main_df$predict_biomarker <- factor(main_df$predict_biomarker, levels = c("Hemoglobin","Log10 Ferritin"))
main_df$data_version <- factor(main_df$data_version, levels = c("Hemoglobin and Ferritin", "Hemoglobin only"))

# add "% increase" on top of the bars

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


# Plot FIGURE S6 ----

# Create a plot function to set y limits separately for each facet 
main_df_hemoglobin <- main_df %>% filter(predict_biomarker=="Hemoglobin")
main_df_ferritin <- main_df %>% filter(predict_biomarker=="Log10 Ferritin")


y_limits_predict_hgb <- c(0, 10)
y_limits_predict_ferr <- c(0, 35)


plot_facet <- function(data, y_limits, show_facet_lables=TRUE, hide_x_axis_label=FALSE, hide_y_axis_lable=FALSE) { 
  p <- ggplot(data, aes(x = Cohort, y = RMSPE, fill = Cohort)) + 
    geom_bar(stat = "identity", color = "black", show.legend = FALSE) + 
    scale_fill_manual(values = c("tomato", "#abdda4", "#56B4E9", "orange")) + 
    scale_y_continuous(expand = c(0, 0), limits = y_limits) + 
    facet_grid(predict_biomarker ~ data_version) + 
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

# Use cowplot to add a common y-axis label
y_axis_label <- ggdraw() + draw_label("RMSPE", angle = 90, x = 0.5, y = 0.5, vjust = 0.5, size = 12)

# Combine the plots using patchwork and ensure equal widths
combined_plot <- (hemoglobin_plot / ferritin_plot) + 
  theme(plot.tag.position = 'left', plot.tag = element_text(size = 14))

# Add the y-axis label and ensure proper alignment using cowplot
final_plot <- plot_grid(y_axis_label, combined_plot, ncol = 2, rel_widths = c(0.05, 1))

final_plot_2 <- grid.arrange(final_plot, top="Baseline biomarkers included", right="Follow-up outcome predicted")
final_plot_2

fname_svg <- paste0("./4_output/updates/figs/", "figS6.svg") 
fname_png <- paste0("./4_output/updates/figs/", "figS6.png")
fname_pdf <- paste0("./4_output/updates/figs/", "figS6.pdf")

ggsave(plot = final_plot_2, fname_svg, width = 6, height = 5.5, unit = "in")
ggsave(plot = final_plot_2, fname_png, width = 6, height = 5.5, unit = "in")
ggsave(plot = final_plot_2, fname_pdf, width = 6, height = 5.5, unit = "in")
