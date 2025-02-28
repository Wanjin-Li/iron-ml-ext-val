# Create labeled datasets for model training 

library(data.table)
library(lubridate)
library(dplyr)

t_donations_old <- fread("./1_data/private/SANBSdata/Modified_SANBS_Donations_06_Jan_2023.csv")  # 5115924 samples
t_donations <- fread("./1_data/private/SANBSdata/Modified_SANBS_Donations_07_Jan_2023.csv")  # 5115924 samples

length(unique(t_donations_old$DonorID))
length(unique(t_donations$DonorID))


# had an error in race so will need to rerun sanbs_02_preprocess.R
# right <- d.labeled[, c("DonorID", "Visit_Date", "rbc_loss_last_12_months", "rbc_loss_last_24_months", "race")]
# left <- t_donations[,c("DonorID", "Visit_Date", "rbc_loss_last_12_months", "rbc_loss_last_24_months")]
# to_merge <- merge(x=left, y=right, by= c("DonorID", "Visit_Date", "rbc_loss_last_12_months", "rbc_loss_last_24_months"), all.x=TRUE)  
# t_donations$race <- to_merge$race   # merge in to get DER_DaysRBCLoss column

######### external validation 
# race recode 0, 1, 2, 3  to actual races W, B, ... etc
# as.factor(race, sex, blood type)

d.labeled <- t_donations


# # Create 2 datasets
# Dataset 1: index donation (hgb, ferritin present) ---> predict follow up (hgb, ferritin separately)
# Dataset 2 (more samples): index donation (hgb only, ferritin can be missing) --> predict follow up (hgb, ferritin separately)

# Dataset 1
# Remove index donations with NA in hgb or with NA in ferritin
# Only keep rows with no NAs in hgb and fu_hgb value; same for ferritin
d.hgb_ferr <- d.labeled[!is.na(d.labeled$index_ferritin) &
                          !is.na(d.labeled$index_hgb) &
                          !is.na(d.labeled$fu_hgb) &
                          !is.na(d.labeled$fu_ferritin),]

# Dataset 2: index donation can have missing ferritin
d.hgb_only <- d.labeled[!is.na(d.labeled$index_hgb) &
                          !is.na(d.labeled$fu_hgb) &
                          !is.na(d.labeled$fu_ferritin),]

# remove index donation ferritin column (also need to remove log_ferritin col)
d.hgb_only <- subset(d.hgb_only, select = -c(index_ferritin, index_log_ferritin))


# days_since_last_rbc_loss: set NAs (%) to 10 years ago (10*365 days)

# check NA percentage
sum(is.na(d.hgb_ferr$days_since_last_rbc_loss)) / length(d.hgb_ferr$days_since_last_rbc_loss)  # 11.9%
sum(is.na(d.hgb_only$days_since_last_rbc_loss)) / length(d.hgb_only$days_since_last_rbc_loss)  # 10.4%


d.hgb_ferr$days_since_last_rbc_loss[is.na(d.hgb_ferr$days_since_last_rbc_loss)] <- 3650
d.hgb_only$days_since_last_rbc_loss[is.na(d.hgb_only$days_since_last_rbc_loss)] <- 3650

# days_since_last_drbc_loss: set NAs (%) to 10 years ago (10*365 days)
# check NA percentage
sum(is.na(d.hgb_ferr$days_since_last_drbc_loss)) / length(d.hgb_ferr$days_since_last_drbc_loss)  # 99.5%
sum(is.na(d.hgb_only$days_since_last_drbc_loss)) / length(d.hgb_only$days_since_last_drbc_loss)  # 99.6%

d.hgb_ferr$days_since_last_drbc_loss[is.na(d.hgb_ferr$days_since_last_drbc_loss)] <- 3650
d.hgb_only$days_since_last_drbc_loss[is.na(d.hgb_only$days_since_last_drbc_loss)] <- 3650

str(d.hgb_ferr)


######### external validation for d.hgb_ferr, d.hgb_only
# as.factor(race, sex, blood type)
# race convert to factor
d.hgb_ferr$blood_type <- as.factor(d.hgb_ferr$blood_type)
d.hgb_ferr$sex <- as.factor(d.hgb_ferr$sex)
d.hgb_only$blood_type <- as.factor(d.hgb_only$blood_type)
d.hgb_only$sex <- as.factor(d.hgb_only$sex)

str(d.hgb_ferr)
str(d.hgb_only)

fwrite(d.hgb_ferr, "./1_data/sanbs_external_validation/sanbs_hgb_ferr_08_Jan_2023.csv")
fwrite(d.hgb_only, "./1_data/sanbs_external_validation/sanbs_hgb_only_08_Jan_2023.csv")



### read in 
d.hgb_ferr <- fread("./1_data/sanbs_external_validation/sanbs_hgb_ferr_08_Jan_2023.csv")  
d.hgb_only <- fread("./1_data/sanbs_external_validation/sanbs_hgb_only_08_Jan_2023.csv") 

str(d.hgb_ferr)
str(d.hgb_only)

# > str(d.hgb_ferr)

# > str(d.hgb_only)




