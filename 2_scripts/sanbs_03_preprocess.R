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
# Classes ‘data.table’ and 'data.frame':	17331 obs. of  17 variables:
# $ DonorID                  : int  18544 18544 18653 18653 18654 19412 19412 19893 20332 20342 ...
# $ Visit_Date               : IDate, format: "2022-07-25" "2022-09-23" "2022-04-11" "2022-07-04" ...
# $ rbc_loss_last_12_months  : int  4 5 5 5 6 5 6 7 6 7 ...
# $ rbc_loss_last_24_months  : int  9 9 9 9 12 9 9 13 9 11 ...
# $ days_since_last_rbc_loss : int  61 60 80 84 91 56 56 56 63 56 ...
# $ days_since_last_drbc_loss: int  3650 3650 3650 3650 3650 3650 3650 3650 3650 3650 ...
# $ cum_lifetime_donations   : int  63 64 209 210 121 158 159 219 49 99 ...
# $ index_hgb                : num  13.9 13.8 14.9 14.2 17.3 14.9 13.3 15.3 14.3 13.2 ...
# $ blood_type               : chr  "O-" "O-" "A+" "A+" ...
# $ age                      : num  49.7 49.9 69.6 69.9 47.5 50.4 50.5 70.6 48 59.5 ...
# $ sex                      : int  0 0 0 0 0 0 0 0 1 0 ...
# $ race                     : chr  "W" "W" "W" "W" ...
# $ index_ferritin           : num  7.3 5.8 9.3 5.5 9.8 6 5.9 8.7 8.5 6.9 ...
# $ index_log_ferritin       : num  0.863 0.763 0.968 0.74 0.991 ...
# $ time_to_fu               : int  60 59 84 133 56 56 84 56 63 67 ...
# $ fu_hgb                   : num  13.8 14.9 14.2 13.8 17.1 13.3 16.1 14.5 13.6 13 ...
# $ fu_ferritin              : num  5.8 6.1 5.5 6.1 8.9 5.9 17 9.7 6.7 6.9 ...

# > str(d.hgb_only)
# Classes ‘data.table’ and 'data.frame':	374096 obs. of  15 variables:
# $ DonorID                  : int  18353 18361 18366 18378 18399 18399 18399 18406 18481 18481 ...
# $ Visit_Date               : IDate, format: "2021-02-24" "2021-02-28" "2021-02-22" "2020-08-08" ...
# $ rbc_loss_last_12_months  : int  5 6 2 2 7 6 6 4 4 4 ...
# $ rbc_loss_last_24_months  : int  9 8 2 2 9 11 12 8 9 7 ...
# $ days_since_last_rbc_loss : int  58 59 72 126 60 65 77 77 77 65 ...
# $ days_since_last_drbc_loss: int  3650 3650 3650 3650 3650 3650 3650 3650 3650 3650 ...
# $ cum_lifetime_donations   : int  35 35 26 22 52 56 60 39 46 50 ...
# $ index_hgb                : num  14.6 12.5 15.5 15.4 14.7 14.1 14.3 16.3 16.2 14.3 ...
# $ blood_type               : chr  "O+" "A+" "A+" "O+" ...
# $ age                      : num  44.6 62.4 51.1 47.9 51.4 52.1 52.8 67.3 47.7 48.9 ...
# $ sex                      : int  0 1 1 1 0 0 0 0 0 0 ...
# $ race                     : chr  "W" "W" "W" "W" ...
# $ time_to_fu               : int  134 215 236 336 59 57 56 80 185 177 ...
# $ fu_hgb                   : num  16.1 14.6 15.3 15.6 14.5 13.6 13.3 15.5 15.5 15 ...
# $ fu_ferritin              : num  17.5 17.2 94.5 338.8 11.3 ...




