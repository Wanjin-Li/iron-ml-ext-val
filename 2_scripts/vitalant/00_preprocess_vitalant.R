# Preprocess Vitalant dataset and create labeled datasets for model training

library(data.table)
library(dplyr)
library(summarytools)  # for creating data summaries
library(haven)  # for read_sas()
library(lubridate)  # for working with time
library(runner)  # for sum_run() function



# # LOAD AND SAVE AS CSV ----
# 
# intermediate_directory <- './3_intermediate/private/parquet/'
# if (!dir.exists(intermediate_directory)) {
#   dir.create(intermediate_directory, recursive = TRUE)
# }
# 
# a<- read_sas("./1_data/private/vitalant/donation_donor.sas7bdat")
# b <- read_sas("./1_data/private/vitalant/medical_quest.sas7bdat")
# c <- read_sas("./1_data/private/vitalant/ferritin.sas7bdat")
# d <- read_sas("./1_data/private/vitalant/reaction.sas7bdat")
# 
# fwrite(a, "./3_intermediate/private/vitalant_donation_donor.csv")
# fwrite(b, "./3_intermediate/private/vitalant_medical_quest.csv")
# fwrite(c, "./3_intermediate/private/vitalant_ferritin.csv")


# Merge dataframes ----

a <- fread("./3_intermediate/private/vitalant_donation_donor.csv")  
b <- fread("./3_intermediate/private/vitalant_medical_quest.csv")  
c <- fread("./3_intermediate/private/vitalant_ferritin.csv")  

str(a)
str(b)
str(c)

unique(b$QUESTION_TEXT)

# > intersect(intersect(colnames(a), colnames(b)), colnames(c))
# [1] "DON_DATE_KEY"    "DONATION_NUMBER"
# 
# > intersect(colnames(a), colnames(b))
# [1] "DON_DATE_KEY"    "DONOR_NUMBER"    "DONATION_NUMBER"
# 
# > intersect(colnames(a), colnames(c))
# [1] "DON_DATE_KEY"    "DONOR_KEY"       "DONATION_NUMBER"
# 
# > intersect(colnames(b), colnames(c))
# [1] "DON_DATE_KEY"    "DONATION_NUMBER"  

# Hemoglobin value ----

# include only hgb answer
hgb <- b[b$QUESTION_CODE == "5U"| QUESTION_CODE=="5V"]

hgb[, table(MQ_ANSWER)]

# recode incorrect hgb values 

hgb[hgb$MQ_ANSWER=="0000000000000013.0"]$MQ_ANSWER <- "13.0"
hgb[hgb$MQ_ANSWER=="0000016.2"]$MQ_ANSWER <- "16.2"
hgb[hgb$MQ_ANSWER=="000017.7"]$MQ_ANSWER <- "17.7"
hgb[hgb$MQ_ANSWER=="00014.9"]$MQ_ANSWER <- "14.9"
hgb[hgb$MQ_ANSWER=="00017.2"]$MQ_ANSWER <- "17.2"
hgb[hgb$MQ_ANSWER=="0013.2"]$MQ_ANSWER <- "13.2"
hgb[hgb$MQ_ANSWER=="03"]$MQ_ANSWER <- "0.3"
hgb[hgb$MQ_ANSWER=="08.4"]$MQ_ANSWER <- "8.4"
hgb[hgb$MQ_ANSWER=="09.2"]$MQ_ANSWER <- "9.2"
hgb[hgb$MQ_ANSWER==".2"]$MQ_ANSWER <- "0.2"
hgb[hgb$MQ_ANSWER==".9"]$MQ_ANSWER <- "0.9"
hgb[hgb$MQ_ANSWER=="+15.3"]$MQ_ANSWER <- "15.3"
hgb[hgb$MQ_ANSWER=="0"]$MQ_ANSWER <- "0.0"
hgb[hgb$MQ_ANSWER=="00.0"]$MQ_ANSWER <- "0.0"
hgb[hgb$MQ_ANSWER=="10."]$MQ_ANSWER <- "10.0"
hgb[hgb$MQ_ANSWER=="11."]$MQ_ANSWER <- "11.0"
hgb[hgb$MQ_ANSWER=="12."]$MQ_ANSWER <- "12.0"
hgb[hgb$MQ_ANSWER=="13."]$MQ_ANSWER <- "13.0"
hgb[hgb$MQ_ANSWER=="14."]$MQ_ANSWER <- "14.0"
hgb[hgb$MQ_ANSWER=="15."]$MQ_ANSWER <- "15.0"
hgb[hgb$MQ_ANSWER=="16."]$MQ_ANSWER <- "16.0"
hgb[hgb$MQ_ANSWER=="17."]$MQ_ANSWER <- "17.0"
hgb[hgb$MQ_ANSWER=="18."]$MQ_ANSWER <- "18.0"

hgb[, MQ_ANSWER := ifelse(startsWith(MQ_ANSWER, "01"), substring(MQ_ANSWER,2), MQ_ANSWER)]
hgb[, MQ_ANSWER := ifelse(MQ_ANSWER=="Not Performed" | MQ_ANSWER=="NO RESPONSE"|MQ_ANSWER=="NOT RECHECKED" |MQ_ANSWER=="WALKOUT" , NA, MQ_ANSWER)]
hgb[, MQ_ANSWER := ifelse(MQ_ANSWER=="" , NA, MQ_ANSWER)]

table(hgb$MQ_ANSWER, useNA = "ifany")

# remove hgb=NA or hgb=0 or not?

# hgb_valid <- hgb[!is.na(hgb)]

setnames(hgb, "MQ_ANSWER", "hgb")
hgb$hgb <- as.numeric(hgb$hgb)

# calculate the number of hgb checks per each donation
hgb <- hgb[, numdonation:= seq_len(.N), by=list(DONATION_NUMBER)]
hgb[, table(numdonation)] # some have rechecks or just duplications


# extract first-time hgb values [QUESTION_CODE == "5U]
hgb_firsttime <- hgb[QUESTION_CODE=="5U"]

# extract rechecked hgb values [QUESTION_CODE == "5V]
hgb_recheck <- hgb[QUESTION_CODE=="5V"]

# left merge first-time hgb with rechecked hgb by DON_DATE_KEY
hgb_merge <- merge(hgb_firsttime, hgb_recheck, by="DONATION_NUMBER", all.x = TRUE)

# take the higher hgb if both first time and rechecked hgb are available
hgb_merge[, hgb_higher := case_when((!is.na(hgb.x) & is.na(hgb.y)) ~ hgb.x,
                                    (is.na(hgb.x) & !is.na(hgb.y)) ~ hgb.y,
                                    (!is.na(hgb.x) & !is.na(hgb.y) & (hgb.x > hgb.y)) ~ hgb.x,
                                    (!is.na(hgb.x) & !is.na(hgb.y) & (hgb.x < hgb.y)) ~ hgb.y,
                                    (!is.na(hgb.x) & !is.na(hgb.y) & (hgb.x = hgb.y)) ~ hgb.x,
                             TRUE ~ NA)]

summary(hgb_merge$hgb_higher)
setnames(hgb_merge, "hgb_higher", "hgb")

# select needed columns
hgb_select <- hgb_merge[, c("DONATION_NUMBER", "DON_DATE_KEY.x", "hgb")]
setnames(hgb_select, "DON_DATE_KEY.x", "DON_DATE_KEY")

# double check duplicates
length(unique(hgb_select$DONATION_NUMBER)) == nrow(hgb_select)

hgb_dups <- hgb_select[duplicated(hgb_select$DONATION_NUMBER),]
hgb_dups2 <- hgb_select[DONATION_NUMBER %in% hgb_dups$DONATION_NUMBER]
hgb_nodup <- hgb_select[!DONATION_NUMBER %in% hgb_dups2$DONATION_NUMBER]

# take the earliest hgb value for those who have rechecked hgb on different dates
setorder(hgb_dups2, DONATION_NUMBER, DON_DATE_KEY)
hgb_dups2[, numdup := seq_len(.N), by=list(DONATION_NUMBER)] # 4 visits on two different dates have the same donation number 
hgb_dups3 <- hgb_dups2[, .SD[1], DONATION_NUMBER][, -c("numdup")]

# merge
hgb_clean <- rbind(hgb_nodup, hgb_dups3)
length(unique(hgb_clean$DONATION_NUMBER)) == nrow(hgb_clean)


# Ferritin value ----
ferr <- c[, c("DONATION_NUMBER", "DONATION_TEST_RESULT")]

# check and remove duplicates
table(duplicated(c))
table(duplicated(c$DONATION_NUMBER))
which(duplicated(c$DONATION_NUMBER))

ferr_dups <- c[549395,]
ferr_valid <- c[DONATION_NUMBER!="W03582001592300R"]

# drop unneeded columns
ferr <- ferr_valid[, -c("DONATION_TEST_KEY")]

# check duplicates
length(unique(ferr$DONATION_NUMBER)) == nrow(ferr)  # FALSE indicates duplicated DONATION_NUMBER

str(ferr)
table(ferr$DONATION_TEST_RESULT)

# recoding ferritin values
ferr[ferr$DONATION_TEST_RESULT == ">450"]$DONATION_TEST_RESULT <- "450"
ferr[ferr$DONATION_TEST_RESULT == "GREATER THAN 450"]$DONATION_TEST_RESULT <- "450"

ferr[ferr$DONATION_TEST_RESULT == "<8"]$DONATION_TEST_RESULT <- "8"
ferr[ferr$DONATION_TEST_RESULT == "LESS THAN 8"]$DONATION_TEST_RESULT <- "8"

ferr[ferr$DONATION_TEST_RESULT == "TECHNICAL PROBLEM"]$DONATION_TEST_RESULT <- NA

ferr$DONATION_TEST_RESULT <- as.numeric(ferr$DONATION_TEST_RESULT)  # convert char to numeric for ferritin value
summary(ferr$DONATION_TEST_RESULT)
setnames(ferr, "DONATION_TEST_RESULT", "Ferritin")


# Remove duplicates ----

## Donation Donor dataset duplicates ----
table(duplicated(a$DONATION_NUMBER))
which(duplicated(a$DONATION_NUMBER))
dups <- a[c(941454, 13891356),]

dup1 <- a[a$DONATION_NUMBER == "5231476"]  # remove both of these two duplicates because all cols the same except PHLEB_STOP_TIME_KEY
a <- a[a$DONATION_NUMBER != "5231476", ]

dup2 <- a[a$DONATION_NUMBER == "0167937"]  # discard these two rows because same DONATION_NUMBER but different donors which is error
a <- a[a$DONATION_NUMBER != "0167937", ]

length(unique(a$DONATION_NUMBER)) == nrow(a)  # check that DONATION_NUMBER is unique



# Merge dataframes ----
summary(a$DONATION_NUMBER %in% hgb_clean$DONATION_NUMBER)
summary(hgb_clean$DONATION_NUMBER %in% a$DONATION_NUMBER)

df <- merge(x = a, y = hgb_clean, by = "DONATION_NUMBER", all.x = TRUE)  # merge in hgb value from medical quest dataset
sum(!is.na(df$hgb))

df <- merge(x = df, y = ferr, by = "DONATION_NUMBER", all.x = TRUE)  # merge in ferritin values
sum(!is.na(df$Ferritin))



# Rename variables ----
df <- df %>% rename(rbc_loss_in_ml = DONATION_VOLUME_DRAWN,
                    #cum_lifetime_donations = NA,
                    index_hgb = hgb,
                    blood_type = DONOR_ABORH,
                    weight = DONOR_WEIGHT,
                    height = DONOR_HEIGHT,
                    age = DONOR_AGE_AT_DONATION,
                    sex = DONOR_GENDER,
                    race = RACE_ETHNICITY,
                    index_ferritin = Ferritin)

str(df)

setnames(df, "DON_DATE_KEY.x", "DON_DATE_KEY")
setnames(df, "DONOR_KEY.x", "DONOR_KEY")

df <- df[, -c("DON_DATE_KEY.y", "DONOR_KEY.y")]

# Convert DON_DATE_KEY to date time ----
res <- as.POSIXct(as.character(df$DON_DATE_KEY), format = "%Y%m%d", tz ="UTC")

df$DON_DATE_KEY <- res


#####################################################################
#TODO: missing cumulative lifetime donations ----
# for now create proxy as cumulative sum 
head(df)

# donor key vs donor number  - use DONOR_NUMBER
length(unique(df$DONOR_KEY))
length(unique(df$DONOR_NUMBER))

setkey(df, DONOR_NUMBER, DON_DATE_KEY)  # sorts the dataframe
head(df)

df <- df[, -c(32)]
df$donation <- ifelse(df$PHLEBOTOMY_STATUS == "Successful Phlebotomy", 1, 0)
df <- df %>% group_by(DONOR_NUMBER) %>% mutate(cum_lifetime_donations = cumsum(donation))
##################################################################


## Rename variables ----
df <- df %>% rename(DonorID = DONOR_NUMBER,
                    Visit_Date = DON_DATE_KEY)


fwrite(df, "./3_intermediate/private/2023-11-08/vitalant_intermediate_to_del0.csv")










