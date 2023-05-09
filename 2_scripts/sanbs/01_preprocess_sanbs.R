library(data.table)
library(dplyr)
library(summarytools)  # for creating data summaries
library(lubridate)  # for working with time
library(runner)  # for sum_run() function

# Takes ~15 min to run

# Deriving variables ----'

df <- fread("./3_intermediate/private/sanbs_intermediate_to_del0.csv")

# first create variables that can be used
setkey(df, DonorID, Visit_Date)  # sorts the dataframe

# create unit_loss for counting units loss
df$unit_rbc_loss <- ifelse(df$Outcome == "SUCCESSFUL DONATION" & df$DonProc == "WHOLE BLOOD", 1,
                       ifelse(df$Outcome == "SUCCESSFUL DONATION" & df$DonProc == "DOUBLE RBC", 2, 0))

# df$past_year <- as.POSIXct(df$Visit_Date,tz="UTC",origin="1970-01-01") - years(1)
# df$past_2years <- as.POSIXct(df$Visit_Date,tz="UTC",origin="1970-01-01") - years(2)



## time_to_fu ----

df[, `:=`(next.val = c(tail(Visit_Date, -1), NA)), by=DonorID]
df$time_to_fu <- df$next.val - df$Visit_Date
df <- df[, -c("next.val")]  # drop unneeded col

# save intermediate df
fwrite(df, "./3_intermediate/private/sanbs_intermediate_to_del1.csv")



## rbc_loss_last_12_months ----
## rbc_loss_last_24_months ----

df <- fread("./3_intermediate/private/sanbs_intermediate_to_del1.csv")

df <- df %>%
  group_by(DonorID) %>%
  mutate(rbc_loss_last_12_months = sum_run(x = unit_rbc_loss,
                                           k = 365,  # last 12 months
                                           idx = as.Date(Visit_Date, format="%Y-%m-%d")))

df <- df %>%
  group_by(DonorID) %>%
  mutate(rbc_loss_last_24_months = sum_run(x = unit_rbc_loss,
                                           k = 365*2,  # last 24 months
                                           idx = as.Date(Visit_Date, format="%Y-%m-%d")))


##  days_since_last_rbc_loss ----

# Logic: subset to whole blood donations (no deferrals). Then get number of days since previous row which is the previous date of RBC loss
df <- as.data.table(df)
success <- df[df$unit_rbc_loss != 0, ]  # subset to blood donations that lost any number of unit of rbc
success[, prev.rbc_loss_date := shift(Visit_Date, 1L, type="lag"), by=DonorID]  # get the previous rbc loss date
success$days_since_last_rbc_loss <- as.integer(success$Visit_Date - success$prev.rbc_loss_date)  # get the difference in days between current date and previous rbc loss date

# Note: after merging, will have NA for rows that are not whole blood donations for days_since_last_rbc_loss but this is fine since
# in the final dataframe, we subset to only whole blood donors
df <- merge(x=df, y=success[, c("DonorID", "Visit_Date", "days_since_last_rbc_loss")], by=c("DonorID", "Visit_Date"), all.x=TRUE)  # merge column days_since_last_rbc_loss back in



## days_since_last_drbc_loss ----

success <- df[df$unit_rbc_loss == 2, ]  # subset to double red blood cell donations
success[, prev.drbc_loss_date := shift(Visit_Date, 1L, type="lag"), by=DonorID]  # get the previous drbc loss date
success$days_since_last_drbc_loss <- as.integer(success$Visit_Date - success$prev.drbc_loss_date)  # get the difference in days between current date and previous drbc loss date

df <- merge(x=df, y=success[, c("DonorID", "Visit_Date", "days_since_last_drbc_loss")], by=c("DonorID", "Visit_Date"), all.x=TRUE)  # merge column days_since_last_rbc_loss back in

# After merging will have NAs. For each donor, if have a date where drbc loss, then need to fill in the subsequent days with that day + extra days till current date

drbc_gt2 <- success %>% group_by(DonorID) %>% filter(n() >= 2)  # get drbc donors that have 2 or more drbc donations. (if one drbc donation, then days_since_last_drbc_loss will be NA; similar for all samples of donor with zero drbc donatiosn)
drbc_gt2_donors <- unique(drbc_gt2$DonorID)  # get DonorIDs of these drbc donors

all_drbc_donors <- df[df$DonorID %in% drbc_gt2_donors]  # subset entire df based on these drbc donors

dates <- all_drbc_donors$days_since_last_drbc_loss

iter <- 1

for (donor in drbc_gt2_donors) {  # for each donor
  donor_df <- all_drbc_donors[all_drbc_donors$DonorID == donor]  # subset to that donor's samples
  prev_drbc_date <- NA  # initialize empty variable to store donor drbc date

  for (i in 1:nrow(donor_df)) {  # for each row of donor's samples
    donor_df_sample <- donor_df[i]  # get single sample of donor
    if (!is.na(donor_df_sample$days_since_last_drbc_loss)) { # if current sample lost drbc
      prev_drbc_date <- donor_df_sample$Visit_Date
    } else {  # current sample is NA for days_since_last_drbc_loss
      if (is.na(prev_drbc_date)) {  # if previous loss date is NA
        dates[iter] <- 3650  # set to 10 years ago
      } else {  # already have a prev lost date
        date_diff <- donor_df_sample$Visit_Date - prev_drbc_date  # get difference in days
        dates[iter] <- date_diff
      }
    }
    iter <- iter + 1
    print(iter)
  }
}
all_drbc_donors$days_since_last_drbc_loss <- dates

df <- merge(x=df[,-c("days_since_last_drbc_loss")], y=all_drbc_donors[, c("DonorID", "Visit_Date", "days_since_last_drbc_loss")], by=c("DonorID", "Visit_Date"), all.x=TRUE)  # merge column days_since_last_rbc_loss back in

df$days_since_last_rbc_loss[is.na(df$days_since_last_rbc_loss)] <- 3650  # replace remaining NA with 10 years
df$days_since_last_drbc_loss[is.na(df$days_since_last_drbc_loss)] <- 3650  # replace remaining NA with 10 years

# save intermediate df
fwrite(df, "./3_intermediate/private/sanbs_intermediate_to_del2.csv")



## index log ferritin ----

# load data back in
df <- fread("./3_intermediate/private/sanbs_intermediate_to_del2.csv")

# Add 1 to Ferritin values and recalculate index log ferritin
summary(df$index_ferritin)
df$index_ferritin <- df$index_ferritin + 1
summary(df$index_ferritin)
df$index_log_ferritin <- log10(df$index_ferritin)
summary(df$index_log_ferritin)



## fu_hgb ----
df[, `:=`(fu_hgb = c(tail(index_hgb, -1), NA)), by=DonorID]



## fu_log_ferritin ----
df[, `:=`(fu_log_ferritin = c(tail(index_log_ferritin, -1), NA)), by=DonorID]

# save intermediate df
fwrite(df, "./3_intermediate/private/sanbs_intermediate_to_del3.csv")



# Modify other variables ----
# load data back in
df <- fread("./3_intermediate/private/sanbs_intermediate_to_del3.csv")


## Blood type ----
table(df$blood_type)

# recode these
df[df$blood_type == "A2BPOS"]$blood_type <- "AB+"
df[df$blood_type == "A2NEG"]$blood_type <- "A-"
df[df$blood_type == "A2POS"]$blood_type <- "A+"
df[df$blood_type == "ANEGL"]$blood_type <- "A-"
df[df$blood_type == "BwPOS"]$blood_type <- "B+"
df[df$blood_type == "ONEGH"]$blood_type <- "O-"
df[df$blood_type == "OPOSH"]$blood_type <- "O+"
df[df$blood_type == "OPOSL"]$blood_type <- "O+"
df[df$blood_type == "wABNEG"]$blood_type <- "AB-"
df[df$blood_type == "wABPOS"]$blood_type <- "AB+"
df[df$blood_type == "wANEG"]$blood_type <- "A-"
df[df$blood_type == "wAPOS"]$blood_type <- "A+"

df[df$blood_type == "APOS"]$blood_type <- "A+"
df[df$blood_type == "ANEG"]$blood_type <- "A-"
df[df$blood_type == "BPOS"]$blood_type <- "B+"
df[df$blood_type == "BNEG"]$blood_type <- "B-"
df[df$blood_type == "ABPOS"]$blood_type <- "AB+"
df[df$blood_type == "ABNEG"]$blood_type <- "AB-"
df[df$blood_type == "OPOS"]$blood_type <- "O+"
df[df$blood_type == "ONEG"]$blood_type <- "O-"

# ignore NULL and UNK 
df <- df[!(df$blood_type == "UNK" | df$blood_type == "NULL"),]
table(df$blood_type)



## recode race variable ---- 
# ignore all other race classes for now
table(df$race)
# African Black         Asian    Mixed Race       unknown         White 
# 1832692               341417        280927         46590       2607833 
df[df$race == "African Black"]$race <- "Black"





# Subsetting dataframe ----
# Because of rbc_loss_last_12_months and rbc_loss_last_24_months, we lose 2 years worth of data for each donor
# so we remove rows in the first two years for each donor
df <- df %>% group_by(DonorID) %>% filter(Visit_Date >= Visit_Date[1] + years(2))

# Subset to whole blood donors only
df <- df[df$unit_rbc_loss == 1, ]  # subset to whole blood donations. i.e. exclude deferrals or double rbc (unit_rbc_loss == 2)

# remove NAs in time_to_fu because these are the last date of a donor
df <- df[!is.na(df$time_to_fu),]

df <- data.frame(df)

# convert columns to correct type
df$index_hgb <- as.numeric(df$index_hgb)
df$fu_hgb <- as.numeric(df$fu_hgb)

fwrite(df, "./3_intermediate/private/sanbs_intermediate_to_del4.csv")




# Subset to columns for analysis ----
df <- fread("./3_intermediate/private/sanbs_intermediate_to_del4.csv")

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

# Dataset 1
# Remove index donations with NA in hgb or with 0 in index_log_ferritin
# Only keep rows with no NAs in hgb and fu_hgb value; same for ferritin

d.hgb_ferr <- df[!(df$index_log_ferritin == 0) &
                   !is.na(df$index_hgb) &
                   !is.na(df$fu_hgb) &
                   !(df$fu_log_ferritin == 0),]

summary(d.hgb_ferr$fu_log_ferritin)
str(d.hgb_ferr)

# Dataset 2: index donation can have missing ferritin
d.hgb_only <- df[!is.na(df$index_hgb) &
                   !is.na(df$fu_hgb) &
                   !(df$fu_log_ferritin == 0),]  # follow up ferritin must be present since we will predict this

# remove index donation ferritin column (also need to remove log_ferritin col)
d.hgb_only <- subset(d.hgb_only, select = -c(index_ferritin, index_log_ferritin))
str(d.hgb_ferr)


# CREATE DATA SUMMARY ----
print(dfSummary(d.hgb_ferr), file="./3_intermediate/data_summaries/sanbs_data_summary_labeled_hgb_ferr.html")
print(dfSummary(d.hgb_only), file="./3_intermediate/data_summaries/sanbs_data_summary_labeled_hgb_only.html")

intermediate_directory <- './3_intermediate/private'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}

fwrite(d.hgb_ferr, "./3_intermediate/private/hgb_ferr_sanbs.csv")
fwrite(d.hgb_only, "./3_intermediate/private/hgb_only_sanbs.csv")