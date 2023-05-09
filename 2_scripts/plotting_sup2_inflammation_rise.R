
# preprocess rise data ----
# Same as 01_preprocess_rise except include sTfR column

# Preprocess RISE dataset and create labeled datasets for model training

library(data.table)
library(dplyr)
library(jsonlite)
library("sas7bdat")
library(summarytools)  # for creating data summaries

TEN_YEARS_IN_DAYS = 10*365
KG_PER_POUND = 0.453592 
METERS_PER_INCH = 0.0254
THRESH_ABSENT_FEMALE = 12
THRESH_ABSENT_MALE = 12
THRESH_LOW_FEMALE = 20
THRESH_LOW_MALE = 30

### LOAD DATA -----------------------------

# RISE dataset can only be accessed with permission from biolincc
# it is free but need to submit request with IRB exemption or approval

d.bl <- as.data.table(
  read.sas7bdat("./1_data/private/RISEdata/rise_baseline.sas7bdat"))
d.fu <- as.data.table(
  read.sas7bdat("./1_data/private/RISEdata/rise_followup.sas7bdat"))

### PROCESS AND CLEAN -----------------------------

# # DIFFERENCES BETWEEN BASELINE [BL] AND FOLLOWUP [FU] IN COLUMN NAMES
# setdiff(colnames(d.bl), colnames(d.fu))
# setdiff(colnames(d.fu), colnames(d.bl))

# Count bl donations with no fu
d.bl[ RandID %in% setdiff(d.bl$RandID, d.fu$RandID), .N]  # .N = # of instances

# Remove baseline donations with no followup visits
d.bl <- d.bl[ ! RandID %in% setdiff(d.bl$RandID, d.fu$RandID)]

intermediate_directory <- './3_intermediate/data_summaries'
if (!dir.exists(intermediate_directory)) {
  dir.create(intermediate_directory)
}

### CREATE SUMMARIES
#print(dfSummary(d.bl), file="./3_intermediate/data_summaries/rise_data_summary_baseline.html")
# print(dfSummary(d.fu), file="./3_intermediate/data_summaries/rise_data_summary_followup.html")

### LOOK FOR ERRONEOUS VALUES AND REMOVE
# For donor 378-5276-3 visit 1, 
# RQ19_ever_pregnant = 8 and Number of pregnancies is NA.
# Changing to never pregnant, number of pregnancies 0
# Since she has no subsequent ferritin or Hgb, removing from dataset
d.bl[RandID=="378-5276-3", c(RQ19_Ever_Pregnant, RQ20_NumberOfPregnancies)]
d.fu[RandID=="378-5276-3", c("DER_Fingerstick_HCT", "DER_Fingerstick_HGB", 
                             "ARUP_Ferritin", "STS_VEN_HemocueHgb", 
                             "DER_AdjVenousHgb")]
d.bl<-d.bl[!(RandID=="378-5276-3")]
d.fu<-d.fu[!(RandID=="378-5276-3")]

# Assign donor with missing blood type (DD_ABO_RH) to most common
d.bl[DD_ABO_RH=="UNT", .N]  # Counting; 1 donor at baseline
d.fu[DD_ABO_RH=="UNT", .N]  # They gave 2 subsequent donations
d.bl[DD_ABO_RH=="UNT", DD_ABO_RH := "O+"]
d.fu[DD_ABO_RH=="UNT", DD_ABO_RH := "O+"]
# d.bl$DD_ABO_RH <- droplevels(d.bl$DD_ABO_RH)  # drop levels
# d.fu$DD_ABO_RH <- droplevels(d.fu$DD_ABO_RH)  # drop levels

# Make days since Double Red Cell donation (DRLoss) = 10 years for patients 
# with no record. Do same for RBC loss
d.bl[, DER_DaysDRLoss := 
       ifelse(is.na(DER_DaysDRLoss), TEN_YEARS_IN_DAYS, DER_DaysDRLoss)]
d.fu[, DER_DaysDRLoss := 
       ifelse(is.na(DER_DaysDRLoss), TEN_YEARS_IN_DAYS, DER_DaysDRLoss)]
d.bl[, DER_DaysRBCLoss := 
       ifelse(is.na(DER_DaysRBCLoss), TEN_YEARS_IN_DAYS, DER_DaysRBCLoss)]
d.fu[, DER_DaysRBCLoss := 
       ifelse(is.na(DER_DaysRBCLoss), TEN_YEARS_IN_DAYS, DER_DaysRBCLoss)]

### Explore data

# BL missing both fingerstick:
table(is.na(d.bl$DER_Fingerstick_HGB), is.na(d.bl$DER_Fingerstick_HCT))
# 67 are missing both fingerstick HGB and HCT at baseline

# BL missing both fingerstick and venous:
table(is.na(d.bl$DER_Fingerstick_HGB) & is.na(d.bl$DER_Fingerstick_HCT),
      is.na(d.bl$STS_VEN_HemocueHgb))
# none are missing venous

# FU missing both fingerstick
table(is.na(d.fu$DER_Fingerstick_HGB), is.na(d.fu$DER_Fingerstick_HCT)) 
# 1326(!) are missing both at followup

# FU missing both fingerstick by visit result
table(is.na(d.fu$DER_Fingerstick_HGB) & is.na(d.fu$DER_Fingerstick_HCT),
      d.fu$DER_VisitResult)

# FU missing venous and both fingerstick
table(is.na(d.fu$DER_Fingerstick_HGB) & is.na(d.fu$DER_Fingerstick_HCT),
      is.na(d.fu$STS_VEN_HemocueHgb))
# 1028 are missing both fingersticks and venous

# FU missing all venous and both fingerstick
table(is.na(d.fu$DER_Fingerstick_HGB) & is.na(d.fu$DER_Fingerstick_HCT)
      & is.na(d.fu$STS_VEN_HemocueHgb), d.fu$DER_VisitResult)
table(d.fu$DER_Fingerstick_HGB < 12.5,
      d.fu$DER_VisitResult) 
# Most coded as deferrals are actual deferrals (VisitResult=3)

table(d.fu$DER_RBCLoss_Units, d.fu$DER_VisitResult)
table(d.fu$DV_Outcome, d.fu$DER_VisitResult)
table(d.fu$DV_Deferral_Cat1, d.fu$DER_VisitResult)
table(d.fu$DV_Deferral_Cat2, d.fu$DER_VisitResult)
table(d.fu[DER_VisitResult==3]$DV_Deferral_Cat2,
      d.fu[DER_VisitResult==3]$DV_Deferral_Cat1)
table(d.bl$DER_VisitResult)

### IMPUTATION -------------------------

### CREATE HB deferral field
# Hemoglobin deferral if:
#   DV_Outcome == 3 
# or
#   DV_Outcome != 1 AND
# CREATE Fingerstick_HGB_equiv
# If fingerstick HGB measured use it.
# Otherwise use fingerstick_HCT/3.04
# Otherwise use DER_AdjVenousHgb (venous HGB, if taken post-donation 
# then adjusted)

d.bl[, FingerstickHGB_equiv := 
       fifelse(is.na(DER_Fingerstick_HGB),
               fifelse(is.na(DER_Fingerstick_HCT), 
                       DER_AdjVenousHgb, DER_Fingerstick_HCT/3.04),
               DER_Fingerstick_HGB)
]
d.fu[, FingerstickHGB_equiv := 
       fifelse(is.na(DER_Fingerstick_HGB),
               fifelse(is.na(DER_Fingerstick_HCT),
                       DER_AdjVenousHgb, DER_Fingerstick_HCT/3.04),
               DER_Fingerstick_HGB)
]

table(is.na(d.bl$FingerstickHGB_equiv), useNA="always") 
# No bl visits missing HGB equiv

table(is.na(d.fu$FingerstickHGB_equiv), d.fu$DER_VisitResult, useNA="always")
# 713 completed donations; 28 QNS, and 151 deferrals are missing any HGB

# For donations missing venous HGB, replace with Fingerstick HGB
d.bl[, DER_AdjVenousHgb := 
       fifelse(is.na(DER_AdjVenousHgb), FingerstickHGB_equiv, DER_AdjVenousHgb)]
d.fu[, DER_AdjVenousHgb := 
       fifelse(is.na(DER_AdjVenousHgb), FingerstickHGB_equiv, DER_AdjVenousHgb)]

# For missing race, assume other
d.bl$DD_Raceth[which(is.na(d.bl$DD_Raceth))] = "O"
d.fu$DD_Raceth[which(is.na(d.fu$DD_Raceth))] = "O"
d.bl[,DD_Raceth:= ifelse(DD_Raceth=="", "O", DD_Raceth)]
d.fu[,DD_Raceth:= ifelse(DD_Raceth=="", "O", DD_Raceth)]

# Missing height, weight, and BMI
table(is.na(d.bl$DER_Height), is.na(d.bl$DER_Weight), is.na(d.bl$BMI))
table(is.na(d.fu$DER_Height), is.na(d.fu$DER_Weight), is.na(d.fu$BMI))

unique(d.fu[is.na(DER_Weight)]$RandID)
unique(d.bl[is.na(DER_Weight)]$RandID)
d.fu[is.na(DER_Weight), .N]

# Impute mean weight by gender for those age > 25
weight_by_gender <- d.bl[DER_Age > 25, mean(DER_Weight, na.rm=TRUE), 
                         by=DD_Gender]
d.bl[, DER_Weight := fifelse(!is.na(DER_Weight), DER_Weight, 
                             fifelse(DD_Gender=="M", 
                                     weight_by_gender[DD_Gender == "M", V1],
                                     weight_by_gender[DD_Gender == "F", V1]))]
d.fu[, DER_Weight := fifelse(!is.na(DER_Weight), DER_Weight, 
                             fifelse(DD_Gender=="M", 
                                     weight_by_gender[DD_Gender == "M", V1],
                                     weight_by_gender[DD_Gender == "F", V1]))]

# Impute mean height by gender for those age > 25
d.fu[is.na(DER_Height), .N]
height_by_gender <- d.bl[DER_Age > 25, mean(DER_Height, na.rm=TRUE), 
                         by=DD_Gender]
d.bl[, DER_Height := fifelse(!is.na(DER_Height), DER_Height, 
                             fifelse(DD_Gender=="M", 
                                     height_by_gender[DD_Gender == "M", V1],
                                     height_by_gender[DD_Gender == "F", V1]))]
d.fu[, DER_Height := fifelse(!is.na(DER_Height), DER_Height, 
                             fifelse(DD_Gender=="M", 
                                     height_by_gender[DD_Gender == "M", V1],
                                     height_by_gender[DD_Gender == "F", V1]))]
# Calculate BMI if missing
d.bl[, BMI := ifelse(!is.na(BMI), BMI,
                     DER_Weight*KG_PER_POUND / (DER_Height*METERS_PER_INCH)^2)] 

d.fu[, BMI := ifelse(!is.na(BMI), BMI,
                     DER_Weight*KG_PER_POUND / (DER_Height*METERS_PER_INCH)^2)] 

# Missing total lifetime donations: set to 0
d.bl[, RQ2_Total_Lifetime_Donations := fifelse(
  is.na(RQ2_Total_Lifetime_Donations), 0, RQ2_Total_Lifetime_Donations)]

# For fields in baseline but not followup table, merge by randID
setdiff(colnames(d.bl), colnames(d.fu))  # fields in BL table but not FU table
setdiff(colnames(d.fu), colnames(d.bl))  # fields in FU table but not BL table

# Remove all fields from d.fu that aren't in d.bl
d.fu[ , setdiff(colnames(d.fu), colnames(d.bl)) := NULL ]

# Merge in all fields in d.bl that aren't in d.fu
d.fu <- d.bl[, .SD, .SDcols = c("RandID", 
                                setdiff(colnames(d.bl), 
                                        colnames(d.fu))),][d.fu, , on="RandID"]
d.all <- rbind(d.bl, d.fu)  # Combine into one table

# Total lifetime
d.all <- d.all[ order(RandID, VisitNum)]  # sort by RandID then VisitNum
d.all[ , donationMade := ifelse(DER_RBCLoss_Units > 0, 1, 0)]
d.all[ , cumStudyDonations := cumsum(donationMade), by = RandID]
d.all[ , cumLifetimeDonations := 
         RQ2_Total_Lifetime_Donations + cumStudyDonations]  # includes current visit

head(d.all[, c("RandID", "VisitNum", "donationMade", "cumStudyDonations", 
               "DER_RBCLoss_Units", "cumLifetimeDonations", 
               "RQ2_Total_Lifetime_Donations")],20)

## Define features to use; remove other fields
features_all <- c('DER_RBC_Last12months', 'DER_RBC_Last24months',
                  'DER_RBCLoss_Units', 'DER_RBCLoss_mL', 'DER_DaysRBCLoss', 
                  'DER_DaysDRLoss', 'cumLifetimeDonations', 
                  'FingerstickHGB_equiv', 'DD_ABO_RH', 'DER_AdjVenousHgb', 
                  'DER_Weight', 'DER_Height', 'BMI', 'DER_EBV', 
                  'DER_RedCellVolume', 'DER_PercentRBCLoss', 'DER_Age',
                  'DD_Gender', 'DD_Raceth', 'ARUP_STR')

features_fe_as_predictor <- c('ARUP_Ferritin', 'DER_ARUP_log_Ferr')

identifiers <- c("RandID", "VisitDate", "VisitNum", "DER_VisitResult", 
                 "DV_Donproc")

d.all <- d.all[ , .SD, .SDcols = c(features_all, features_fe_as_predictor,
                                   identifiers)]

## CREATE SUMMARY (summarytools)
# print(dfSummary(d.all), file="./3_intermediate/rise_data_summary_prelabelling.html")


# BUILD MODEL DEVELOPMENT DATASET ----------------------
# Generate outcome of visit (used in fu_outcome)
d.all[, outcome := 
        fifelse(DER_VisitResult == 3, 1,  # 1 = HGB deferral
                fifelse(is.na(ARUP_Ferritin), -1,
                        fifelse((DD_Gender=="F" & ARUP_Ferritin < THRESH_ABSENT_FEMALE) |
                                  (DD_Gender=="M" & ARUP_Ferritin < THRESH_ABSENT_MALE),
                                3, # 3 = absent iron donation
                                fifelse((DD_Gender=="F" & ARUP_Ferritin < THRESH_LOW_FEMALE) |
                                          (DD_Gender=="M" & ARUP_Ferritin < THRESH_LOW_MALE),
                                        2,  # 2 = low iron donation
                                        0  # 0 = no adverse event
                                ))))]

# Main analysis: Index donations must have > 150ml RBC lost, not including 2RBC
d.all[, iron_loss_visit := ifelse(DER_RBCLoss_mL > 55, 1, 0)]
d.all[, index_donation := ifelse(DER_RBCLoss_mL > 150 & DER_RBCLoss_Units < 2, 
                                 1, 0)]

d.labeled <- cbind(d.all[is.na(RandID)],
                   "time_to_fu" = numeric(),
                   "fu_outcome"= character(),
                   "fu_hgb"  = numeric(),
                   "fu_ferritin" = numeric())

# add time_to_fu and fu_outcome columns
for (row_idx in 1:nrow(d.all)) {
  if (d.all[row_idx, index_donation]==1) {
    # if qualifies as index visit, get all subsequent visits from donor
    d.index_visit <- d.all[row_idx]
    d.fu_visits <- d.all[RandID==d.index_visit$RandID & VisitNum > d.index_visit$VisitNum]
    reached_iron_loss_visit <- FALSE
    row_fu <- 1
    while (reached_iron_loss_visit == FALSE & row_fu <= nrow(d.fu_visits)){
      d.labeled <- rbind(
        d.labeled,
        cbind(d.index_visit,
              "time_to_fu" = d.fu_visits[row_fu, VisitDate] - d.index_visit$VisitDate,
              "fu_outcome" = d.fu_visits[row_fu, outcome],
              "fu_hgb" = d.fu_visits[row_fu, FingerstickHGB_equiv],
              "fu_ferritin" = d.fu_visits[row_fu, ARUP_Ferritin]
        )
      )
      reached_iron_loss_visit <- fifelse(d.fu_visits[row_fu]$iron_loss_visit == 1, TRUE, FALSE)
      row_fu = row_fu+1
    }
  }
}

# Distribution of outcome including undetermined
table(d.labeled$fu_outcome)
d.labeled[, .N/nrow(d.labeled), by=fu_outcome]
d.labeled <- rbind(d.labeled[fu_outcome != -1])  # Delete undetermined outcomes
d.labeled[, .N/nrow(d.labeled), by=fu_outcome]  # Distribution of outcomes
table(d.labeled[, .N, by=RandID]$N)  # Distribution of outcomes

# Number of rows per unique index donation
table(d.labeled[, .N, by=paste0(RandID, VisitNum)]$N)

# Remove excessive columns 
d.labeled <- d.labeled[, !c("outcome", "index_donation", "iron_loss_visit")]

# remove DER_RBCLoss_Units from analysis 
table(d.labeled$DER_RBCLoss_Units)  # only 1 distinct value so remove
d.labeled <- subset (d.labeled, select = -DER_RBCLoss_Units)

# remove DER_VisitResult from analysis 
table(d.labeled$DER_VisitResult)  # only 1 distinct value so remove
d.labeled <- subset (d.labeled, select = -DER_VisitResult)

# remove fu_outcome since will not use this as a predictor for regression task
d.labeled <- subset(d.labeled, select = -fu_outcome)

# remove DER_AdjVenousHgb since external validation does not have 
# and not commonly measured
d.labeled <- subset(d.labeled, select = -DER_AdjVenousHgb)

# Use Donation procedure = whole blood (WB)
d.labeled <- d.labeled[d.labeled$DV_Donproc == "WB"]
d.labeled <- subset(d.labeled, select = -DV_Donproc)

# recode race: 2 = Asian, 3 = Black, 4 = Hispanic, 5 = Other, 6 = White
table(d.labeled$DD_Raceth) 

d.labeled <- d.labeled %>% mutate(DD_Raceth=recode(DD_Raceth, 
                                                   'H'= 'O',  # code 4=Hispanic as O (Other)
))
# '2'= 'A',  # code 2=Asian as A
# '3'= 'B',  # code 3=Black as B
# '4'= 'O',  # code 4=Hispanic as O (Other)
# '5'= 'O',  # Code 5=Other as O (Other)
# '6'= 'W',  # code 6=White as W 

table(d.labeled$DD_Raceth)

# rename columns
# rename columns
d.labeled <- d.labeled %>%
  rename(
    rbc_loss_last_12_months = DER_RBC_Last12months,
    rbc_loss_last_24_months = DER_RBC_Last24months,
    rbc_loss_in_ml = DER_RBCLoss_mL,
    days_since_last_rbc_loss = DER_DaysRBCLoss,
    days_since_last_drbc_loss = DER_DaysDRLoss,
    cum_lifetime_donations = cumLifetimeDonations,
    index_hgb = FingerstickHGB_equiv,
    blood_type = DD_ABO_RH,
    weight = DER_Weight,
    height = DER_Height,
    bmi = BMI,
    ebv = DER_EBV,
    red_cell_volume = DER_RedCellVolume,
    percent_rbc_loss = DER_PercentRBCLoss,
    age = DER_Age,
    sex = DD_Gender,
    race = DD_Raceth,
    index_ferritin = ARUP_Ferritin,
    index_log_ferritin = DER_ARUP_log_Ferr,
  )

# add 1 to ferritin
d.labeled$index_ferritin <- d.labeled$index_ferritin + 1
d.labeled$index_log_ferritin <- log10(d.labeled$index_ferritin)
d.labeled$fu_ferritin <- d.labeled$fu_ferritin + 1
d.labeled$fu_log_ferritin <- log10(d.labeled$fu_ferritin)

# Create 2 datasets ----
# Dataset 1: index donation (hgb, ferritin present) ---> predict follow up (hgb, ferritin separately)
# Dataset 2 (more samples): index donation (hgb only, ferritin can be missing) --> predict follow up (hgb, ferritin separately)

# Dataset 1
# Remove index donations with NA in hgb or with NA in ferritin 
# Only keep rows with no NAs in hgb and fu_hgb value; same for ferritin

d.hgb_ferr <- d.labeled[!is.na(d.labeled$index_ferritin) &
                          !is.na(d.labeled$index_hgb) & 
                          !is.na(d.labeled$fu_hgb) &
                          !is.na(d.labeled$fu_ferritin)]
summary(d.hgb_ferr$fu_log_ferritin)

# Dataset 2: index donation can have missing ferritin
d.hgb_only <- d.labeled[!is.na(d.labeled$index_hgb) & 
                          !is.na(d.labeled$fu_hgb) &
                          !is.na(d.labeled$fu_ferritin)]  # follow up ferritin must be present since we will predict this

# remove index donation ferritin column (also need to remove log_ferritin col)
d.hgb_only <- subset(d.hgb_only, select = -c(index_ferritin, index_log_ferritin))



stop()

# Start plotting ----




############################# For plotting, refer to link:
# https://www.google.com/search?q=repeat+blood+donors+hemoglobin+and+ferritin&tbm=isch&ved=2ahUKEwj188bemYH9AhXgFGIAHTJeDcUQ2-cCegQIABAA&oq=repeat+blood+donors+hemoglobin+and+ferritin&gs_lcp=CgNpbWcQAzoECCMQJzoECAAQQzoFCAAQgAQ6CAgAEIAEELEDOgYIABAIEB46BwgAEIAEEBhQ7wdYiTRgpzVoA3AAeAGAAbkBiAH4GZIBBDM4LjaYAQCgAQGqAQtnd3Mtd2l6LWltZ8ABAQ&sclient=img&ei=XRvhY7XRJ-CpiLMPsry1qAw&bih=929&biw=1920&rlz=1C1CHBF_enCA971CA971#imgrc=9KxNOMGhJ8pe_M
#############################




# Thresholds
THRESH_ABSENT_FEMALE = 12
THRESH_ABSENT_MALE = 12
THRESH_LOW_FEMALE = 20
THRESH_LOW_MALE = 30


dt.md <- d.hgb_ferr

#### Extra code plotting Ferritin (X) vs. sTfR (Y) for males vs. females
dt.md.recode <- dt.md %>%
  mutate(sex = recode(sex, 'F' = 'Female', 'M' = 'Male'))
# Set ferritin cutoff at 300
#dt.md.ferritin.cutoff <- dt.md.recode[dt.md.recode$ARUP_Ferritin < 300, ]
dt.md.ferritin.cutoff <- dt.md.recode
stfr_threshold_f <- c(1.9, 4.4)
stfr_threshold_m <- c(2, 5)
ferritin_threshold <- c(12, 20)

ggplot(data = dt.md.ferritin.cutoff, aes(x = index_ferritin, y = ARUP_STR)) +
  geom_point(alpha=0.5) +
  geom_hline(yintercept=stfr_threshold_m, linetype="dashed", color = "blue")+
  geom_hline(yintercept=stfr_threshold_f, linetype="dashed", color = "red")+
  geom_vline(xintercept=ferritin_threshold, linetype="dashed", color = "orange")+
  scale_y_continuous(trans='log10') +
  scale_x_continuous(trans='log10') +
  facet_grid(rows = vars(sex)) +
  xlab("Log10 Ferritin (mg/dL)") +
  ylab("Log10 sTfR")

# number of individuals
# males
dt_m <- dt.md.recode[dt.md.recode$sex == "Male", ]  # 1139

# females
dt_f <- dt.md.recode[dt.md.recode$sex == "Female", ]  # 2903

# Inflammation masking iron deficiency: normal ferritin > 20, high sTfR 
dt_m[dt_m$index_ferritin > 20 & dt_m$ARUP_STR > 5.0, .N]  # 3
dt_f[dt_f$index_ferritin > 20 & dt_f$ARUP_STR > 4.4, .N]  # 43

