
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(data.table)

theme_set(theme_bw()+theme(axis.line = element_line(colour = "black"),
                             panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(),
                             panel.border = element_blank(),
                             panel.background = element_blank()) )

EPSILON <- 1e-10

# Percent change = 100% x (follow up - index) / index 


# load RISE data
dat_paths <- c("./3_intermediate/private/hgb_ferr_rise.csv",
               "./3_intermediate/private/hgb_only_rise.csv")
               
               # sanbs
               #"./1_data/sanbs_external_validation/sanbs_hgb_ferr_08_Jan_2023.csv",
               #"./1_data/sanbs_external_validation/sanbs_hgb_only_08_Jan_2023.csv") 

rise_hf <- fread(dat_paths[1])  #"./1_data/private/ml_training_data_hgb_ferr.csv")
rise_h <- fread(dat_paths[2])  #"./1_data/private/ml_training_data_hgb_ferr.csv")


# load SANBS data


#sanbs_hf <- fread(dat_paths[3])  #"./1_data/private/ml_training_data_hgb_ferr.csv")
#sanbs_h <- fread(dat_paths[4])  #"./1_data/private/ml_training_data_hgb_ferr.csv")


r_hf <- rise_hf[,c("VisitDate", "index_hgb", "fu_hgb", "time_to_fu")]


# Percent follow up hemoglobin lower than index hemoglobin
sum (r_hf$fu_hgb - r_hf$index_hgb < 0) / nrow(r_hf)


r_hf$id <- 1:nrow(r_hf)
r_hf$percent <- ( (r_hf$fu_hgb - r_hf$index_hgb)/ (r_hf$index_hgb + EPSILON) ) * 100

df <- r_hf %>% pivot_longer(cols = c("index_hgb", "fu_hgb"),
               names_to = "time")
df[df$time == 'index_hgb', ]$time_to_fu <- 0  # set time_to_followup of index to 0
df[df$time == 'index_hgb', ]$percent <- 0  # set time_to_followup of index to 0

df$biomarker <- "hemoglobin"
# Plot Type 1 ----
# # x = index, fu
# # y = hgb
# ggplot(data = df,
#        aes(x = time,
#            y = value,
#            group = id)) +
#   scale_x_discrete(limits = c("index_hgb", "fu_hgb"))+  # set order of x axis items
#   ylab("Hemoglobin")+
#   geom_line(size = 0.5,
#             alpha = 0.1)

# Plot Type 2 ----
# # x = index, fu
# # y = percent hgb change
# ggplot(data = df, aes(x = time, y = percent, group = id))+
#   scale_x_discrete(limits = c("index_hgb", "fu_hgb"))+  # set order of x axis items
#   ylab("Percent Hgb change = 100% x (follow up - index) / index ")+
#   geom_point(size=0.1)+
#   geom_line(size = 0.5,
#             alpha = 0.1)

# Plot Type 3 ----
# # x = time to follow up (days)
# # y = hgb
# ggplot(data = df, aes(x = time_to_fu, y = value, group = id))+
#   ylab("Hemoglobin")+
#   geom_point(size=0.1)+
#   geom_line(size = 0.5,
#             alpha = 0.1)

# Plot Type 4 ----
# x = time to follow up (days)
# y = percent hgb change
ggplot(data = df, aes(x = time_to_fu, y = percent, group = id))+
  ylab("Percent Hgb change = 100% x (follow up - index) / index ")+
  geom_point(size=0.1, color="#43a2ca")+
  geom_line(size = 0.3, alpha = 0.1,color="#43a2ca")


####### Ferritin

r_hf <- rise_hf[,c("VisitDate", "index_ferritin", "fu_ferritin", "time_to_fu")]

# Percent follow up ferritin lower than index ferritin
sum (r_hf$fu_ferritin - r_hf$index_ferritin < 0) / nrow(r_hf)


r_hf$id <- 1:nrow(r_hf)
r_hf$percent <- ( (r_hf$fu_ferritin - r_hf$index_ferritin)/ (r_hf$fu_ferritin + EPSILON)) * 100

df1 <- r_hf %>% pivot_longer(cols = c("index_ferritin", "fu_ferritin"),
               names_to = "time")
df1[df1$time == 'index_ferritin', ]$time_to_fu <- 0  # set time_to_followup of index to 0
df1[df1$time == 'index_ferritin', ]$percent <- 0  # set time_to_followup of index to 0

df1$biomarker <- "ferritin"


# x = time to follow up (days)
# y = percent ferritin change
ggplot(data = df1, aes(x = time_to_fu, y = percent, group = id))+
  ylab("Percent ferritin change = 100% x (follow up - index) / index ")+
  xlab("Time to follow up (days)")+
  geom_point(size=0.1, color="#fc9272")+
  geom_line(size = 0.3, alpha = 0.1, color="#fc9272")


### Log 10
r_hf$index_log10_ferritin <- log10(r_hf$index_ferritin)
r_hf$fu_log10_ferritin <- log10(r_hf$fu_ferritin)
r_hf$percent_log10 <- ( (r_hf$fu_log10_ferritin - r_hf$index_log10_ferritin)/ (r_hf$index_log10_ferritin + EPSILON)) * 100
df2 <- r_hf %>% pivot_longer(cols = c("index_log10_ferritin", "fu_log10_ferritin"),
                             names_to = "time")
df2[df2$time == 'index_log10_ferritin', ]$time_to_fu <- 0  # set time_to_followup of index to 0
df2[df2$time == 'index_log10_ferritin', ]$percent_log10 <- 0  # set time_to_followup of index to 0

df2$biomarker <- "log10 ferritin"

df2 <- df2[df2$percent_log10 < 1e4,]  ### REMOVE PERCENTAGE CHANGE INFINITY

ggplot(data = df2, aes(x = time_to_fu, y = percent_log10, group = id))+
  ylab("Percent log10 ferritin change")+
  xlab("Time to follow up (days)")+
  geom_point(size=0.1, color="#fc9272")+
  geom_line(size = 0.3, alpha = 0.1, color="#fc9272")



#################### UNUSED ##################
## facet hgb, ferr
res <- rbind(df, df1)

bp <- ggplot(data = res, aes(x = time_to_fu, y = percent, group = id, color=biomarker))+
  ylab("Percent change between index and follow up donations")+
  xlab("Time to follow up (days)")+
  geom_point(size=0.1)+
  geom_line(size = 0.3, alpha = 0.1)


bp + facet_grid(. ~ biomarker)
#################### UNUSED ##################


#### facet hgb, log 10 ferr
df_to_bind <- df2[, c("VisitDate", "time_to_fu", "id", "percent_log10", "time", "value", "biomarker")]
df_to_bind <- df_to_bind %>% rename("percent" = "percent_log10")
res1 <- rbind(df, df_to_bind)
res1$biomarker = factor(res1$biomarker, levels=c("log10 ferritin", "hemoglobin"))

bp1 <- ggplot(data = res1, aes(x = time_to_fu, y = percent, group = id, color=biomarker))+
  ylab("Percent change")+
  xlab("Time to follow-up (days)")+
  geom_point(size=0.1)+
  geom_line(size = 0.3, alpha = 0.1)+
  scale_color_manual(values = c("#00BA38", "#619CFF"))


bp1 + facet_grid(. ~ biomarker)

ggsave("./4_output/updates/figs/figS3_slope_graph.png")
ggsave("./4_output/updates/figs/figS3_slope_graph.svg")

#################### UNUSED ##################
# res_all_3 <- rbind(df, df1) # uncomment to include ferritin
res_all_3 <- df
res_all_3 <- rbind(res_all_3, df_to_bind)
res_all_3$biomarker = factor(res_all_3$biomarker, levels=c("ferritin", "log10 ferritin", "hemoglobin"))

bp2 <- ggplot(data = res_all_3, aes(x = time_to_fu, y = percent, group = id, color=biomarker))+
  ylab("Percent change")+
  xlab("Time to follow up (days)")+
  geom_point(size=0.1)+
  geom_line(size = 0.3, alpha = 0.1)+
  theme(legend.position = "none")  # no legend



bp2 + facet_grid(. ~ biomarker)

#ggsave("./4_output/figs/suppfig2_slope_graph.svg")

#################### UNUSED ##################



# Plotting histograms variability ----
# (follow up - index) / index * 100%

hgb <- df[df$time == "fu_hgb",] 
hgb <- hgb[, c("percent", "time")]
hgb$time <- "Follow up hemoglobin"

ferr <- df1[df1$time == "fu_ferritin",]
ferr <- ferr[, c("percent", "time")]
ferr$time <- "Follow up ferritin"

logferr <- df2[df2$time == "fu_log10_ferritin",]
logferr <- logferr[, c("percent_log10", "time")]
logferr <- logferr %>% 
  rename(
    percent = percent_log10,
  )
logferr$time <- "Follow up log10 ferritin"

res <- rbind(hgb, ferr)
res <- rbind(res, logferr)
res$time = factor(res$time, levels=c("Follow up ferritin", "Follow up log10 ferritin", "Follow up hemoglobin"))

################## UNUSED ##########################
# plot1 <- ggplot(res, aes(x=percent, color=time)) +
#   geom_histogram(fill="white", alpha=0.5, position="identity", bins = 500)
# plot1 + facet_grid(. ~ time)

# p1 <- ggplot(ferr, aes(x=percent, color=time)) +
#   geom_histogram(fill="white", alpha=0.5, position="identity", bins = 500, color="#F8766D")+
#   xlab("Percent change")+
#   labs(title="Ferritin")

# p2 <- ggplot(logferr, aes(x=percent, color=time)) +
#   geom_histogram(fill="white", alpha=0.5, position="identity", bins = 500, color="#00BA38")+
#   xlab("Percent change")+
#   labs(title="Log10 Ferritin")

# p3 <- ggplot(hgb, aes(x=percent, color=time)) +
#   geom_histogram(fill="white", alpha=0.5, position="identity", bins = 500, color="#619CFF")+
#   xlab("Percent change")+
#   labs(title="Hemoglobin")

################## UNUSED ##########################


################### change to density plot instead #############################

p1 <- ggplot(ferr, aes(x=percent, color=time)) +
  geom_density(color="#F8766D", fill="#F8766D")+
  xlab("Percent change")+
  labs(title="Ferritin")+  
  theme(legend.position = "none")  # no legend


p2 <- ggplot(logferr, aes(x=percent, color=time)) +
  geom_density(color="#00BA38" , fill="#00BA38")+
  xlab("Percent change")+
  labs(title="Log10 Ferritin")+
  theme(legend.position = "none")


p3 <- ggplot(hgb, aes(x=percent, color=time)) +
  geom_density(color="#619CFF", fill="#619CFF")+
  xlab("Percent change")+
  labs(title="Hemoglobin") +
  theme(legend.position = "none")

ggpubr::ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
ggsave("./4_output/updates/figs/figs2_densityplot.svg", width = 7)
ggsave("./4_output/updates/figs/figs2_densityplot.png",  width = 7)

