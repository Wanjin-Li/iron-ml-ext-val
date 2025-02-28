# This script plots Figure 4: forest plot of the linear regression coefficient from subgroup analysis.

library(readxl)
library(ggplot2)
library(data.table)
library(grid)
library(gridExtra)


# _______ORIGINAL___________ (use time_to_fu as quantiles)
s_subgroup_lm_dat <- fread("./3_intermediate/subgroup_performance/s_subgroup_lm_data.csv")
v_subgroup_lm_dat <- fread("./3_intermediate/subgroup_performance/v_subgroup_lm_data.csv")
sq_subgroup_lm_dat <- fread("./3_intermediate/subgroup_performance/sq_subgroup_lm_data.csv")
# _____________________


# _______NEW___________ (model time_to_fu as polynomial terms)
# s_subgroup_lm_dat <- fread("./3_intermediate/subgroup_performance/s_subgroup_lm_data_polyfu.csv")
# v_subgroup_lm_dat <- fread("./3_intermediate/subgroup_performance/v_subgroup_lm_data_polyfu.csv")
# sq_subgroup_lm_dat <- fread("./3_intermediate/subgroup_performance/sq_subgroup_lm_data_polyfu.csv")



# add Hispanic row back to SA
dt_raceHispanic <- data.table(Variable = "raceHispanic",
                              s_h_h_coef = NA, s_h_h_conf_lower = NA, s_h_h_conf_upper = NA,
                              s_h_f_coef = NA, s_h_f_conf_lower = NA, s_h_f_conf_upper = NA,
                              s_hf_h_coef = NA, s_hf_h_conf_lower = NA, s_hf_h_conf_upper = NA,
                              s_hf_f_coef = NA, s_hf_f_conf_lower = NA, s_hf_f_conf_upper = NA
                              )

s_subgroup_lm_dat <- rbind(s_subgroup_lm_dat, dt_raceHispanic)

# add race/ethnicity rows back to NL

dt_raceHispanic <- data.table(Variable = "raceHispanic",
                              sq_h_h_coef = NA, sq_h_h_conf_lower = NA, sq_h_h_conf_upper = NA,
                              sq_h_f_coef = NA, sq_h_f_conf_lower = NA, sq_h_f_conf_upper = NA,
                              sq_hf_h_coef = NA, sq_hf_h_conf_lower = NA, sq_hf_h_conf_upper = NA,
                              sq_hf_f_coef = NA, sq_hf_f_conf_lower = NA, sq_hf_f_conf_upper = NA
)


dt_raceAsian <- data.table(Variable = "raceAsian",
                              sq_h_h_coef = NA, sq_h_h_conf_lower = NA, sq_h_h_conf_upper = NA,
                              sq_h_f_coef = NA, sq_h_f_conf_lower = NA, sq_h_f_conf_upper = NA,
                              sq_hf_h_coef = NA, sq_hf_h_conf_lower = NA, sq_hf_h_conf_upper = NA,
                              sq_hf_f_coef = NA, sq_hf_f_conf_lower = NA, sq_hf_f_conf_upper = NA
)

dt_raceBlack <- data.table(Variable = "raceBlack",
                           sq_h_h_coef = NA, sq_h_h_conf_lower = NA, sq_h_h_conf_upper = NA,
                           sq_h_f_coef = NA, sq_h_f_conf_lower = NA, sq_h_f_conf_upper = NA,
                           sq_hf_h_coef = NA, sq_hf_h_conf_lower = NA, sq_hf_h_conf_upper = NA,
                           sq_hf_f_coef = NA, sq_hf_f_conf_lower = NA, sq_hf_f_conf_upper = NA
)

sq_subgroup_lm_dat <- rbind(sq_subgroup_lm_dat, dt_raceHispanic, dt_raceAsian, dt_raceBlack)




# merge subgroup lm performance
t_dat <- merge(s_subgroup_lm_dat, v_subgroup_lm_dat, by = "Variable", all = TRUE)
t_dat <- merge(t_dat, sq_subgroup_lm_dat, by = "Variable", all = TRUE)

t_dat[, Variable := factor(Variable, levels = t_dat$Variable)]
#wide to long
t_dat <- melt(t_dat, id.vars = c("Variable"), variable.name = "concat")


t_dat[, c("Country", "Dataset", "Outcome", "Measure", "Measure2") := tstrsplit(as.character(concat), '_')]
t_dat[, Measure := fifelse(is.na(Measure2), Measure, Measure2)]
t_dat[, Measure2 := NULL]

t_dat[, Country := fifelse(Country=="s", "SA", ifelse(Country=="v", "US", "NL"))]

# t_dat[, Country := fifelse(Country=="s", "SA", "US")] # comment out this line once NL result is back
t_dat[, Dataset := fifelse(Dataset== "h", "Hemoglobin only", "Hemoglobin and ferritin")]
t_dat[, Outcome := fifelse(Outcome== "h", "Hemoglobin", "Ferritin")]

t_dat <- dcast(t_dat, Variable + Country + Dataset + Outcome ~ Measure, value.var = "value")
t_dat[, significant := fifelse(lower > 0 | upper < 0, T, F) ]

# t_dat$Variable <- factor(
#   t_dat$Variable,
#   levels = c("time_to_fu", "I(time_to_fu^2)", "I(time_to_fu^3)", 
#              setdiff(unique(t_dat$Variable), c("time_to_fu", "I(time_to_fu^2)", "I(time_to_fu^3)")))
#   )
setorder(t_dat, Dataset, Outcome)

# Create a new column to combine country and significant status
t_dat[, shape_group := paste(Country, significant, sep = "_")]

t_dat[, Outcome := fifelse(Outcome=="Ferritin", "Log10 Ferritin", Outcome)]

t_dat[, Variable := as.character(Variable)]
t_dat[, Variable := fifelse(Variable=="time_to_fu_quantilesq1", "Time to return Q1 (shortest)", 
                                       ifelse(Variable=="time_to_fu_quantilesq2", "Time to return Q2", 
                                              ifelse(Variable=="time_to_fu_quantilesq3", "Time to return Q3",
                                                     ifelse(Variable=="time_to_fu_quantilesq4", "Time to return Q4 (longest)", Variable))))]
time_levels <- c("Time to return Q1 (shortest)", "Time to return Q2", "Time to return Q3", "Time to return Q4 (longest)")
t_dat[, Variable := factor(Variable, levels = unique(c(time_levels, as.character(Variable))))]
t_dat[, Variable := factor(Variable, levels=unique(Variable))]

# Store the regression results using time_to_fu as polynomial terms
t_dat_time_to_fu <- t_dat[Variable == "time_to_fu" | Variable == "I(time_to_fu^2)" | Variable == "I(time_to_fu^3)"]
setorder(t_dat_time_to_fu, Variable)
fwrite(t_dat_time_to_fu, "./4_output/forest_polyfu_coef.csv")

# Plotting

shape_mapping <- c(
  "US_TRUE" = 16,    # Filled circle for US significant
  "US_FALSE" = 1,    # Hollow circle for US non-significant
  "SA_TRUE" = 17,    # Filled triangle for SA significant
  "SA_FALSE" = 2,    # Hollow triangle for SA non-significant
  "NL_TRUE" = 15,    # Filled square for NL significant
  "NL_FALSE" = 0     # Hollow square for NL non-significant
)

forest_plt <- ggplot(data = t_dat[Variable !="(Intercept)"], 
       aes(x = coef, xmin = lower, xmax = upper, 
           y = Variable, color = Country, shape = shape_group))+
  scale_color_manual(values=c("US"="#abdda4", "SA"="#56B4E9", "NL"="orange"))+
  scale_shape_manual(values=shape_mapping)+ # uncomment this line if end up with different shapes for country
  # scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.2),
  #                    labels = c("TRUE" = "Significant", "FALSE" = "Not Significant"))+
  facet_grid(cols = vars(Dataset), rows = vars(Outcome))+
  geom_errorbar(aes(y=Variable, xmin = lower, xmax = upper), 
                linewidth = 0.5, width = 0.2, position = position_dodge(width = 0.7)) +
  geom_point(aes(y=Variable), size=2, position = position_dodge(width = 0.7))+
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30")+
  theme_bw()+
  labs(y="", x="Linear regression coefficient", shape = "Significance by Country")+
  theme(
    legend.position = "bottom",
    legend.box = "vertical", # Stacks the legends vertically
    legend.spacing.y = unit(0.5, "lines"), # Adds space between the two legends
    strip.text = element_text(size = 10),
    axis.text = element_text(size = 9)
  ) +
  guides(
    color = guide_legend(order = 1), # Country legend appears first
    shape = guide_legend(order = 2)  # Significance legend appears below Country
  ) +
  coord_cartesian(xlim = c(-6, 6))+ # change xlim to c(-6,6) when using time_to_fu quantiles
  annotate("text", label="Higher\nerror",x = 4.7, y = 5.3, color = "grey27", size=3)+ # change to x=4.7, y=5.3 when using time_to_fu quantiles
  annotate("text", label="Lower\nerror",x = -4.7, y = 5.3, color = "grey27", size=3) # change to x=-4.7, y=5.3 when using time_to_fu quantiles
  
forest_plt

top_label <- textGrob("Baseline biomarkers included", hjust=0.2)
new_forest_plt <- grid.arrange(forest_plt,top=top_label, right='Follow-up outcome predicted')


# Open PNG device with desired size and resolution
png(filename = "./4_output/forest_v2.png", width = 7.8, height = 6, units = "in", res = 300)
grid.draw(new_forest_plt)
dev.off()

svg(filename = "./4_output/forest_v2.svg", width = 7.8, height = 6)
grid.draw(new_forest_plt)
dev.off()

# Adding a panel for group sizes
group_size_dat <- fread("./3_intermediate/subgroup_performance/subgroup_group_size.csv")
sq_group_size_dat <- fread("./3_intermediate/subgroup_performance/subgroup_group_size_sq.csv") # add NL results

# Combine SA, US, NL results
all_group_size_dat <- rbind(group_size_dat, sq_group_size_dat)

all_group_size_dat[, Variable2 := ifelse(grepl("age", Variable), "agegroup",
                                     ifelse(grepl("race", Variable), "race",
                                            ifelse(grepl("sex", Variable), "sex",
                                                         ifelse(grepl("time", Variable), "time_to_fu_quantiles", NA)
                                                   )))]

all_group_size_dat[, Total := sum(Size), by = .(Dataset, Country, Variable2)]
all_group_size_dat[, Percentage := (Size/Total)*100]
all_group_size_dat[, Variable := fifelse(Variable=="time_to_fu_quantilesq1", "Time to return Q1 (shortest)", 
                            ifelse(Variable=="time_to_fu_quantilesq2", "Time to return Q2", 
                                   ifelse(Variable=="time_to_fu_quantilesq3", "Time to return Q3",
                                          ifelse(Variable=="time_to_fu_quantilesq4", "Time to return Q4 (longest)", Variable))))]
all_group_size_dat[, Percentage := fifelse(Country == "NL" & Variable2 == "race", 0, Percentage)]

# Group size bar plot
group_size_plot <- ggplot(all_group_size_dat, aes(x = Variable, y = Percentage, fill = Country)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = scales::comma(Size, accuracy=1)),
            position = position_dodge(width = 0.9),
             hjust = -0.2, size = 3) +
  facet_grid(cols = vars(Dataset), rows = vars(Country)) +
  scale_fill_manual(values = c("US" = "#abdda4", "SA" = "#56B4E9", "NL" = "orange")) +
  scale_y_continuous(limits = c(0,100))+
  labs(y = "Relative group size (%)", x = NULL, fill = "Country") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    strip.text.y = element_text(angle = 0, hjust = 0.5)
  ) +
  coord_flip()

group_size_plot <- grid.arrange(group_size_plot, right="Country")


combined_plot <- grid.arrange(
  new_forest_plt, 
  group_size_plot, 
  nrow = 2, 
  heights = c(1.2, 1)  # Adjust height ratio between panels
)

print(combined_plot)


png(filename = "./4_output/forest_panel.png", width = 9, height = 14, units = "in", res = 300)
grid.draw(combined_plot)
dev.off()

svg(filename = "./4_output/forest_panel.svg", width = 9, height = 14)
grid.draw(combined_plot)
dev.off()

pdf(file = "./4_output/forest_panel.pdf", width = 9, height = 14)
grid.draw(combined_plot)
dev.off()


