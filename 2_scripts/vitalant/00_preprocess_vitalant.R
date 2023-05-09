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

# Hemoglobin value 
hgb <- b[b$QUESTION_CODE == "5U"]

# Ferritin value
ferr <- c[, c("DONATION_NUMBER", "DONATION_TEST_RESULT")]



# Remove duplicates ----

## HGB dataset duplicates ----
hgb <- unique(hgb)  # get unique rows 

# duplicates where DONATION_NUMBER same but other 3 variables may be diff
# These are errors since DONATION_NUMBER is supposed to be unique so remove all of these rows 
hgb_dups <- hgb[duplicated(hgb$DONATION_NUMBER),] 
hgb <- hgb[!(hgb$DONATION_NUMBER %in% hgb_dups$DONATION_NUMBER),]  # get donation number of hgb that aren't in the duplicated donation numbers (essentially removing all these duplicates)

## Donation Donor dataset duplicates ----
dups <- a[duplicated(a$DONATION_NUMBER), ]
dup1 <- a[a$DONATION_NUMBER == "5231476"]  # remove both of these two duplicates because all cols the same except PHLEB_STOP_TIME_KEY
a <- a[a$DONATION_NUMBER != "5231476", ]

dup2 <- a[a$DONATION_NUMBER == "0167937"]  # discard these two rows because same DONATION_NUMBER but different donors which is error
a <- a[a$DONATION_NUMBER != "0167937", ]

length(unique(a$DONATION_NUMBER)) == nrow(a)  # check that DONATION_NUMBER is unique

## Ferritin dataset duplicates ----
length(unique(ferr$DONATION_NUMBER)) == nrow(ferr)  # FALSE so have duplicated DONATION_NUMBER
ferr_dups <- ferr[duplicated(ferr$DONATION_NUMBER), ]  # get duplicated row based on DONATION_NUMBER
ferr_dup1 <- ferr[ferr$DONATION_NUMBER == "W03582001592300R"]  # remove 1 of these two duplicates 
ferr <- unique(ferr)



# Preprocess hgb and ferritin datasets ----
## hgb ----
str(hgb)
hgb <- hgb[,c("DONATION_NUMBER", "MQ_ANSWER")]  # subset to DONATION_NUMBER and hgb value since DONATION_NUMBER is unique
hgb$MQ_ANSWER <- as.numeric(hgb$MQ_ANSWER)  # convert char to numeric for hemoglobin value
summary(hgb$MQ_ANSWER)

## ferr ----
str(ferr)
table(ferr$DONATION_TEST_RESULT)

### recoding ferritin values ----
ferr[ferr$DONATION_TEST_RESULT == "GREATER THAN 450"]$DONATION_TEST_RESULT <- "450"
ferr[ferr$DONATION_TEST_RESULT == ">450"]$DONATION_TEST_RESULT <- "450"

ferr[ferr$DONATION_TEST_RESULT == "LESS THAN 8"]$DONATION_TEST_RESULT <- "8"
ferr[ferr$DONATION_TEST_RESULT == "<8"]$DONATION_TEST_RESULT <- "8"

ferr[ferr$DONATION_TEST_RESULT == "TECHNICAL PROBLEM"]$DONATION_TEST_RESULT <- NA

ferr$DONATION_TEST_RESULT <- as.numeric(ferr$DONATION_TEST_RESULT)  # convert char to numeric for hemoglobin value
summary(ferr$DONATION_TEST_RESULT)



# Merge dataframes ----
summary(a$DONATION_NUMBER %in% hgb$DONATION_NUMBER)
summary(hgb$DONATION_NUMBER %in% a$DONATION_NUMBER)

df <- merge(x = a, y = hgb, by = "DONATION_NUMBER", all.x = TRUE)  # merge in hgb value from medical quest dataset
sum(!is.na(df$MQ_ANSWER))

df <- merge(x = df, y = ferr, by = "DONATION_NUMBER", all.x = TRUE)  # merge in ferritin values
sum(!is.na(df$DONATION_TEST_RESULT))



# Rename variables ----
df <- df %>% rename(rbc_loss_in_ml = DONATION_VOLUME_DRAWN,
                    #cum_lifetime_donations = NA,
                    index_hgb = MQ_ANSWER,
                    blood_type = DONOR_ABORH,
                    weight = DONOR_WEIGHT,
                    height = DONOR_HEIGHT,
                    age = DONOR_AGE_AT_DONATION,
                    sex = DONOR_GENDER,
                    race = RACE_ETHNICITY,
                    index_ferritin = DONATION_TEST_RESULT)

str(df)



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

df$donation <- ifelse(df$PHLEBOTOMY_STATUS == "Successful Phlebotomy", 1, 0)
df <- df %>% group_by(DONOR_NUMBER) %>% mutate(cum_lifetime_donations = cumsum(donation))
##################################################################


## Rename variables ----
df <- df %>% rename(DonorID = DONOR_NUMBER,
                    Visit_Date = DON_DATE_KEY)


fwrite(df, "./3_intermediate/private/vitalant_intermediate_to_del0.csv")










