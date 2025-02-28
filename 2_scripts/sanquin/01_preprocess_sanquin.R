# For sanquin

library(data.table)
library(dplyr)
library(summarytools)  # for creating data summaries

# Load in dataframe ----
df <- fread(NA)

# Rename Sanquin variables to standard variables ----
df <- df %>% rename(DonorID = KeyID,
                    Visit_Date = Donatiedatum,
                    rbc_loss_last_12_months = count12, 
                    rbc_loss_last_24_months = count24,
                    days_since_last_rbc_loss = dayssincelastdonation,
                    days_since_last_drbc_loss = days_since_last_drbc_loss,
                    cum_lifetime_donations = DonatieCount,
                    index_hgb = Hb,
                    blood_type = Bloedgroep,
                    age = Leeftijd,
                    sex = Geslacht,
                    index_ferritin = Ferritine,
                    index_log_ferritin = logFerritine,
                    time_to_fu = daysnextdonation,
                    fu_hgb = fup_hb,
                    fu_log_ferritin = fup_ferritin)

# Subset to whole blood donations only. i.e. exclude deferrals or double rbc donations ----
NA


# Subset to relevant columns for analysis ----
df <- df[, c("DonorID",
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
             "fu_log_ferritin")]

str(df)



# Create 2 datasets ----
# Dataset 1: index donation (hgb, ferritin present) ---> predict follow up (hgb, ferritin separately)
# Dataset 2 (more samples): index donation (hgb only, ferritin can be missing) --> predict follow up (hgb, ferritin separately)

## Dataset 1 ----

# Remove index donations with NA in hgb or with 0 in index_log_ferritin
# Only keep rows with no NAs in hgb and fu_hgb value; same for ferritin
d.hgb_ferr <- df[!is.na(df$index_log_ferritin) &
                 !is.na(df$index_hgb) &
                 !is.na(df$fu_hgb) &
                 !is.na(df$fu_log_ferritin),]

summary(d.hgb_ferr$fu_log_ferritin)
str(d.hgb_ferr)



# Dataset 2 (should be larger than Dataset 1) ----

# index donation can have missing ferritin
d.hgb_only <- df[!is.na(df$index_hgb) &
                   !is.na(df$fu_hgb) &
                   !is.na(df$fu_log_ferritin),]  # follow up ferritin must be present since we will predict this

# remove index donation ferritin column (also need to remove log_ferritin col)
d.hgb_only <- subset(d.hgb_only, select = -c(index_ferritin, index_log_ferritin))
str(d.hgb_ferr)


# CREATE DATA SUMMARY ----
print(dfSummary(d.hgb_ferr), file="./sanquin_data_summary_labeled_hgb_ferr.html")
print(dfSummary(d.hgb_only), file="./sanquin_data_summary_labeled_hgb_only.html")


intermediate_directory <- './private'  # create private directory to store results
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}

fwrite(d.hgb_ferr, "./private/hgb_ferr_sanquin.csv")  
fwrite(d.hgb_only, "./private/hgb_only_sanquin.csv") 


#TODO: send both summary files back to chen-yang.su@mail.mcgill.ca
# 1. sanquin_data_summary_labeled_hgb_ferr.html
# 2. sanquin_data_summary_labeled_hgb_only.html