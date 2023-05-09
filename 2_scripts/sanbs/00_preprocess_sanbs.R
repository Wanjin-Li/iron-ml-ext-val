# Preprocess SANBS dataset

# First step:

############### Modify SANBS data first due to error with extra comma ----
## Copy data from Alton directory
# cp ~/../alton/donor-return-dynamics/1_data/private/SANBS_Donations_13_Dec_2022.csv ~/iron-ml-ext-val/1_data/private/SANBSdata

## Run:
# t_donations_new <- fread("./1_data/private/SANBSdata/SANBS_Donations_13_Dec_2022.csv") 

## Error message: 
# In fread("./1_data/private/SANBSdata/SANBS_Donations_13_Dec_2022.csv") :
# Stopped early on line 8258. Expected 27 fields but found 28. Consider fill=TRUE and comment.char=. First discarded non-empty line: <<2694073,2021-01-25 00:00:00.000,2021,1,1550,20,81,30.4,F,Mobile Clinic,WRE356,DEF PROD,DEFERRAL,DEFERRAL,OPOS,NEG,NULL,4,African Black,0.0,HIV current, former sex partne,Hi013 - HIV current  former sex partne,2020-12-31 00:00:00.000,2021-03-31 00:00:00.000,Temp,2020,12>>

## Reason:
# line 8258 column 21 is called "HIV current, former sex partne" - the extra comma gives the error since it splits this single column into two columns which we don't want. Rename it to "HIV current former sex partne".

## Example:
# cat SANBS_Donations_13_Dec_2022.csv | grep 'HIV current, ' | wc -l
# 177 samples with extra comma

## Modify dataset:
# sed -i 's/current,/current/' SANBS_Donations_13_Dec_2022.csv  # remove extra comma
# sed -i 's/current  /current /' SANBS_Donations_13_Dec_2022.csv  # replace double space with single space: "Hi013 - HIV current  former sex partne" -> "Hi013 - HIV current former sex partne"

## Rename dataset to distinguish it from original
# mv SANBS_Donations_13_Dec_2022.csv Modified_SANBS_Donations_13_Dec_2022.csv
##################################

#### Note:
# In donation time, have one value 10002 which should be 1002 since donatino time is military time

library(data.table)
library(dplyr)
library(summarytools)  # for creating data summaries
library(lubridate)  # for working with time

df <- fread("./1_data/private/SANBSdata/Modified_SANBS_Donations_13_Dec_2022.csv")
str(df)

# Remove duplicates ----

## Remove duplicates (where every entry is the exact same) ----
# I found 910 duplicated samples (where every entry is the exact same). I removed those duplicates first.

to_remove <- df[duplicated(df)]
df <- anti_join(df, to_remove)  # removes all duplicates (910 x 2)
df <- rbind(df, to_remove)  # add back 910 samples

## Remove rows where all columns same except ferritin ----
# Then, I removed the ferritin column and checked the resulting dataframe for duplicates in the remaining columns.
# It appears that we have 87 samples that are duplicated (31 unique donors IDs)
# Delete the row with Ferritin == 0 and keep the one where Ferritin != 0

df_no_ferr <- df[, -c("Ferritin")]
cols <- colnames(df_no_ferr)
possible_removal <- df_no_ferr[duplicated(df_no_ferr)]  # 87 samples

to_remove1_all <- df[df$DonorID %in% possible_removal$DonorID &  # 174 samples
        df$Visit_Date %in% possible_removal$Visit_Date &
        df$donation_time %in% possible_removal$donation_time &
        df$Visit_Age %in% possible_removal$Visit_Age,]
to_remove1 <- to_remove1_all[to_remove1_all$Ferritin == 0]

df <- anti_join(df, to_remove1)  # removes duplicates (87 samples that have Ferritin == 0)

## Remove cases where DonorID and Visit_Date are same but some of the other columns differ ----
# Compile the other duplicate samples.
# There are 1970 unique duplicate samples (based on DonorID and Visit_Date) accounting for 4204 samples.
# Number of duplicates:   2       3    4    8
# Number of samples:      1877    6   66   21
# total samples: 1877*2 + 6*3 + 66*4 + 21*8 = 4204

dups <- df[duplicated(df[, c("DonorID", "Visit_Date")])]

donor_ids <- dups$DonorID
visit_dates <- dups$Visit_Date

sub_df <- df[df$DonorID == donor_ids[1] & df$Visit_Date == visit_dates[1]]  # initialize dataframe
sub_df <- sub_df[-c(1:nrow(sub_df)), ]  # remoe entries in dataframe

for (i in 1:length(donor_ids)) {
  print(i)
  sub <- df[df$DonorID == donor_ids[i] & df$Visit_Date == visit_dates[i]]
  sub_df <- rbind(sub_df, sub)
}
df <- anti_join(df, sub_df) # removes 3875 rows

# Rename variables in SANBS ----
df <- df %>% rename(cum_lifetime_donations = don_Sequence,
                    index_hgb = HB_Value,
                    blood_type = ABO_RH,
                    age = Visit_Age,
                    index_ferritin = Ferritin)
str(df)

# create summary first to see variable values
# print(dfSummary(df), file="./3_intermediate/data_summaries/sanbs_prederiving_variables_summary.html")

fwrite(df, "./3_intermediate/private/sanbs_intermediate_to_del0.csv")


