# Preprocess RISE dataset and create labeled datasets for model training

library(data.table)
library(dplyr)
# library(sas7bdat)  # too slow
library(summarytools)  # for creating data summaries

### LOAD DATA -----------------------------

#  <- read.sas7bdat("./1_data/private/vitalant/donation_donor.sas7bdat")
# b <- read.sas7bdat("./1_data/private/vitalant/medical_quest.sas7bdat")
# ferritin <- read.sas7bdat("./1_data/private/vitalant/ferritin.sas7bdat")
# 
# fwrite(a, "./3_intermediate/private/vitalant_donation_donor.csv")  
# fwrite(b, "./3_intermediate/private/vitalant_medical_quest.csv")  
# fwrite(c, "./3_intermediate/private/vitalant_ferritin.csv")  

### LOAD DATA -----------------------------
install.packages("ggraph")
library(ggraph)  # for read_sas()
 
install.packages("tidygraph")
library(tidygraph)