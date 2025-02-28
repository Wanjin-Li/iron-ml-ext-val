# This script prepares for test data from outer folds for RISE internal validation.

# HGB ONLY ----

# original data
hgb_training_data <- fread("./3_intermediate/private/hgb_only_rise.csv")
hgb_training_data$sex <- as.character(hgb_training_data$sex)

# remove extraneous fields
identifiers <- c("VisitDate", "VisitNum", 
                 "rbc_loss_in_ml",
                 "weight", "height", "bmi", "ebv", "red_cell_volume", "percent_rbc_loss", 
                 "race")

hgb_training_data[, c(identifiers) := NULL]

## PREDICT HGB ----
dt.md.hgb.outer <- subset(hgb_training_data, select = -c(fu_ferritin, fu_log_ferritin))  # predict hgb so remove ferr outcomes

### FACTOR ----
dt.factor.hgb <- copy(dt.md.hgb.outer)
char_columns <- colnames(dt.factor.hgb)[unlist(dt.factor.hgb[, lapply(.SD, is.character),][1,])]
char_columns <- setdiff(char_columns, "RandID")
dt.factor.hgb <- dt.factor.hgb %>%
  mutate_at(vars(char_columns), as.factor)


# actual training set with no Donor ID 
# repeat the following for the other two outer folds: outer fold 1 and outer fold 2
rsplit_factors_hgb_only_outer_fold <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_factors_hgb_only_outer_fold_3.rds")

actual_train_dt_h_pred_h_outer_fold <- rsplit_factors_hgb_only_outer_fold$splits[[1]]$data

# match actual training set to the original data
# inner join to get all rows in original data that are in actual training data
matched_train_dt <- merge(actual_train_dt_h_pred_h_outer_fold, dt.factor.hgb, by=c("rbc_loss_last_12_months", 
                                                                                       "rbc_loss_last_24_months",
                                                                                       "days_since_last_rbc_loss",
                                                                                       "days_since_last_drbc_loss",
                                                                                       "cum_lifetime_donations",
                                                                                       "index_hgb", "blood_type", "age", "sex", "time_to_fu", "fu_hgb"))

matched_train_dt_idx <- matched_train_dt$RandID
unique_train_dt_idx <- unique(matched_train_dt_idx)
length(unique_train_dt_idx) # same as the theoretical donor split in train set

all_dt_idx <- dt.factor.hgb$RandID
unique_all_dt_idx <- unique(all_dt_idx)
length(unique_all_dt_idx)

matched_test_dt_idx <- all_dt_idx[!(all_dt_idx %in% matched_train_dt_idx)]
unique_test_dt_idx <- unique(matched_test_dt_idx)
length(unique_test_dt_idx) # same as the theoretical donor split in test set

# recovered test set that should be used for nested CV
matched_test_dt <- dt.factor.hgb[RandID %in% unique_test_dt_idx]
print(nrow(matched_test_dt)) # same as the theoretical and actual donation test split within this outer fold

fwrite(matched_test_dt, "./3_intermediate/outer_fold_test_data/pred_hgb/test_factors_hgb_only_outer_fold_3.csv")

### ONE-HOT CODED ----
column_to_exclude <- "RandID"
dt.OH.hgb <- copy(dt.md.hgb.outer)
dt.OH.hgb <- data.table(model.matrix(fu_hgb ~., data = dt.md.hgb.outer[, !column_to_exclude, with = FALSE]))
dt.OH.hgb <- cbind(dt.OH.hgb,
                   "RandID" = dt.md.hgb.outer$RandID,
                   "fu_hgb" = dt.md.hgb.outer$fu_hgb)

# actual training set with no Donor ID 
# repeat the following for the other two outer folds: outer fold 1 and outer fold 2
rsplit_oh_hgb_only_outer_fold <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_OH_hgb_only_outer_fold_3.rds")

actual_train_dt_h_pred_h_outer_fold <- rsplit_oh_hgb_only_outer_fold$splits[[1]]$data

# match actual training set to the original data
# inner join to get all rows in original data that are in actual training data
matched_train_dt <- merge(actual_train_dt_h_pred_h_outer_fold, dt.OH.hgb, by=c("(Intercept)", "rbc_loss_last_12_months", 
                                                                                   "rbc_loss_last_24_months",
                                                                                   "days_since_last_rbc_loss",
                                                                                   "days_since_last_drbc_loss",
                                                                                   "cum_lifetime_donations",
                                                                                   "index_hgb", "blood_typeA+", "blood_typeAB-", "blood_typeAB+", "blood_typeB-", "blood_typeB+", "blood_typeO-", "blood_typeO+",
                                                                               "age", "sexM", "time_to_fu", "fu_hgb"))

matched_train_dt_idx <- matched_train_dt$RandID
unique_train_dt_idx <- unique(matched_train_dt_idx)
length(unique_train_dt_idx) # same as the theoretical donor split in train set

all_dt_idx <- dt.OH.hgb$RandID
unique_all_dt_idx <- unique(all_dt_idx)
length(unique_all_dt_idx)

matched_test_dt_idx <- all_dt_idx[!(all_dt_idx %in% matched_train_dt_idx)]
unique_test_dt_idx <- unique(matched_test_dt_idx)
length(unique_test_dt_idx) # same as the theoretical donor split in test set

# recovered test set that should be used for nested CV
matched_test_dt <- dt.OH.hgb[RandID %in% unique_test_dt_idx]
print(nrow(matched_test_dt)) # same as the theoretical and actual donation test split within this outer fold

fwrite(matched_test_dt, "./3_intermediate/outer_fold_test_data/pred_hgb/test_OH_hgb_only_outer_fold_3.csv")


## PREDICT FERR ----
dt.md.ferr.outer <- subset(hgb_training_data, select = -c(fu_hgb, fu_ferritin))  # predict log ferritin so remove hgb outcome and ferr outcome

### FACTOR ----
dt.factor.ferr <- copy(dt.md.ferr.outer)
char_columns <- colnames(dt.factor.ferr)[unlist(dt.factor.ferr[, lapply(.SD, is.character),][1,])]
char_columns <- setdiff(char_columns, "RandID")
dt.factor.ferr <- dt.factor.ferr %>%
  mutate_at(vars(char_columns), as.factor)


# actual training set with no Donor ID 
# repeat the following for the other two outer folds: outer fold 1 and outer fold 2
rsplit_factors_hgb_only_outer_fold <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_factors_hgb_only_outer_fold_3.rds")

actual_train_dt_h_pred_f_outer_fold <- rsplit_factors_hgb_only_outer_fold$splits[[1]]$data

# match actual training set to the original data
# inner join to get all rows in original data that are in actual training data
matched_train_dt <- merge(actual_train_dt_h_pred_f_outer_fold, dt.factor.ferr, by=c("rbc_loss_last_12_months", 
                                                                                   "rbc_loss_last_24_months",
                                                                                   "days_since_last_rbc_loss",
                                                                                   "days_since_last_drbc_loss",
                                                                                   "cum_lifetime_donations",
                                                                                   "index_hgb", "blood_type", "age", "sex", "time_to_fu", "fu_log_ferritin"))

matched_train_dt_idx <- matched_train_dt$RandID
unique_train_dt_idx <- unique(matched_train_dt_idx)
length(unique_train_dt_idx) # same as the theoretical donor split in train set

all_dt_idx <- dt.factor.ferr$RandID
unique_all_dt_idx <- unique(all_dt_idx)
length(unique_all_dt_idx)

matched_test_dt_idx <- all_dt_idx[!(all_dt_idx %in% matched_train_dt_idx)]
unique_test_dt_idx <- unique(matched_test_dt_idx)
length(unique_test_dt_idx) # same as the theoretical donor split in test set

# recovered test set that should be used for nested CV
matched_test_dt <- dt.factor.ferr[RandID %in% unique_test_dt_idx]
print(nrow(matched_test_dt)) # same as the theoretical and actual donation test split within this outer fold

fwrite(matched_test_dt, "./3_intermediate/outer_fold_test_data/pred_ferr/test_factors_hgb_only_outer_fold_3.csv")

### ONE-HOT CODED ----
column_to_exclude <- "RandID"
dt.OH.ferr <- copy(dt.md.ferr.outer)
dt.OH.ferr <- data.table(model.matrix(fu_log_ferritin ~., data = dt.md.ferr.outer[, !column_to_exclude, with = FALSE]))
dt.OH.ferr <- cbind(dt.OH.ferr,
                   "RandID" = dt.md.ferr.outer$RandID,
                   "fu_log_ferritin" = dt.md.ferr.outer$fu_log_ferritin)

# actual training set with no Donor ID 
# repeat the following for the other two outer folds: outer fold 1 and outer fold 2
rsplit_oh_hgb_only_outer_fold <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_OH_hgb_only_outer_fold_3.rds")

actual_train_dt_h_pred_f_outer_fold <- rsplit_oh_hgb_only_outer_fold$splits[[1]]$data

# match actual training set to the original data
# inner join to get all rows in original data that are in actual training data
matched_train_dt <- merge(actual_train_dt_h_pred_f_outer_fold, dt.OH.ferr, by=c("(Intercept)", "rbc_loss_last_12_months", 
                                                                               "rbc_loss_last_24_months",
                                                                               "days_since_last_rbc_loss",
                                                                               "days_since_last_drbc_loss",
                                                                               "cum_lifetime_donations",
                                                                               "index_hgb", "blood_typeA+", "blood_typeAB-", "blood_typeAB+", "blood_typeB-", "blood_typeB+", "blood_typeO-", "blood_typeO+",
                                                                               "age", "sexM", "time_to_fu", "fu_log_ferritin"))

matched_train_dt_idx <- matched_train_dt$RandID
unique_train_dt_idx <- unique(matched_train_dt_idx)
length(unique_train_dt_idx) # same as the theoretical donor split in train set

all_dt_idx <- dt.OH.ferr$RandID
unique_all_dt_idx <- unique(all_dt_idx)
length(unique_all_dt_idx)

matched_test_dt_idx <- all_dt_idx[!(all_dt_idx %in% matched_train_dt_idx)]
unique_test_dt_idx <- unique(matched_test_dt_idx)
length(unique_test_dt_idx) # same as the theoretical donor split in test set

# recovered test set that should be used for nested CV
matched_test_dt <- dt.OH.ferr[RandID %in% unique_test_dt_idx]
print(nrow(matched_test_dt)) # same as the theoretical and actual donation test split within this outer fold

fwrite(matched_test_dt, "./3_intermediate/outer_fold_test_data/pred_ferr/test_OH_hgb_only_outer_fold_3.csv")

#______________________________________________________________________________________________


# HGB FERR ----

# original data
hgb_ferr_training_data <- fread("./3_intermediate/private/hgb_ferr_rise.csv")
hgb_ferr_training_data$sex <- as.character(hgb_ferr_training_data$sex)

# remove extraneous fields
identifiers <- c("VisitDate", "VisitNum", 
                 "rbc_loss_in_ml",
                 "weight", "height", "bmi", "ebv", "red_cell_volume", "percent_rbc_loss", 
                 "race")

hgb_ferr_training_data[, c(identifiers) := NULL]

## PREDICT HGB ----
dt.md.hgb.outer <- subset(hgb_ferr_training_data, select = -c(fu_ferritin, fu_log_ferritin))  # predict hgb so remove ferr outcomes

### FACTOR ----
dt.factor.hgb <- copy(dt.md.hgb.outer)
char_columns <- colnames(dt.factor.hgb)[unlist(dt.factor.hgb[, lapply(.SD, is.character),][1,])]
char_columns <- setdiff(char_columns, "RandID")
dt.factor.hgb <- dt.factor.hgb %>%
  mutate_at(vars(char_columns), as.factor)


# actual training set with no Donor ID 
# repeat the following for the other two outer folds: outer fold 1 and outer fold 2
rsplit_factors_hgb_ferr_outer_fold <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_factors_hgb_ferr_outer_fold_3.rds")

actual_train_dt_hf_pred_h_outer_fold <- rsplit_factors_hgb_ferr_outer_fold$splits[[1]]$data

# match actual training set to the original data
# inner join to get all rows in original data that are in actual training data
matched_train_dt <- merge(actual_train_dt_hf_pred_h_outer_fold, dt.factor.hgb, by=c("rbc_loss_last_12_months", 
                                                                                   "rbc_loss_last_24_months",
                                                                                   "days_since_last_rbc_loss",
                                                                                   "days_since_last_drbc_loss",
                                                                                   "cum_lifetime_donations",
                                                                                   "index_hgb", "blood_type", "age", "sex", "time_to_fu", "fu_hgb"))

matched_train_dt_idx <- matched_train_dt$RandID
unique_train_dt_idx <- unique(matched_train_dt_idx)
length(unique_train_dt_idx) # same as the theoretical donor split in train set

all_dt_idx <- dt.factor.hgb$RandID
unique_all_dt_idx <- unique(all_dt_idx)
length(unique_all_dt_idx)

matched_test_dt_idx <- all_dt_idx[!(all_dt_idx %in% matched_train_dt_idx)]
unique_test_dt_idx <- unique(matched_test_dt_idx)
length(unique_test_dt_idx) # same as the theoretical donor split in test set

# recovered test set that should be used for nested CV
matched_test_dt <- dt.factor.hgb[RandID %in% unique_test_dt_idx]
print(nrow(matched_test_dt)) # same as the theoretical and actual donation test split within this outer fold

fwrite(matched_test_dt, "./3_intermediate/outer_fold_test_data/pred_hgb/test_factors_hgb_ferr_outer_fold_3.csv")

### ONE-HOT CODED ----
column_to_exclude <- "RandID"
dt.OH.hgb <- copy(dt.md.hgb.outer)
dt.OH.hgb <- data.table(model.matrix(fu_hgb ~., data = dt.md.hgb.outer[, !column_to_exclude, with = FALSE]))
dt.OH.hgb <- cbind(dt.OH.hgb,
                   "RandID" = dt.md.hgb.outer$RandID,
                   "fu_hgb" = dt.md.hgb.outer$fu_hgb)

# actual training set with no Donor ID 
# repeat the following for the other two outer folds: outer fold 1 and outer fold 2
rsplit_oh_hgb_ferr_outer_fold <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_hgb/rsplit_OH_hgb_ferr_outer_fold_3.rds")

actual_train_dt_hf_pred_h_outer_fold <- rsplit_oh_hgb_ferr_outer_fold$splits[[1]]$data

# match actual training set to the original data
# inner join to get all rows in original data that are in actual training data
matched_train_dt <- merge(actual_train_dt_hf_pred_h_outer_fold, dt.OH.hgb, by=c("(Intercept)", "rbc_loss_last_12_months", 
                                                                               "rbc_loss_last_24_months",
                                                                               "days_since_last_rbc_loss",
                                                                               "days_since_last_drbc_loss",
                                                                               "cum_lifetime_donations",
                                                                               "index_hgb", "blood_typeA+", "blood_typeAB-", "blood_typeAB+", "blood_typeB-", "blood_typeB+", "blood_typeO-", "blood_typeO+",
                                                                               "age", "sexM", "time_to_fu", "fu_hgb",
                                                                               "index_ferritin", "index_log_ferritin"))

matched_train_dt_idx <- matched_train_dt$RandID
unique_train_dt_idx <- unique(matched_train_dt_idx)
length(unique_train_dt_idx) # same as the theoretical donor split in train set

all_dt_idx <- dt.OH.hgb$RandID
unique_all_dt_idx <- unique(all_dt_idx)
length(unique_all_dt_idx)

matched_test_dt_idx <- all_dt_idx[!(all_dt_idx %in% matched_train_dt_idx)]
unique_test_dt_idx <- unique(matched_test_dt_idx)
length(unique_test_dt_idx) # same as the theoretical donor split in test set

# recovered test set that should be used for nested CV
matched_test_dt <- dt.OH.hgb[RandID %in% unique_test_dt_idx]
print(nrow(matched_test_dt)) # same as the theoretical and actual donation test split within this outer fold

fwrite(matched_test_dt, "./3_intermediate/outer_fold_test_data/pred_hgb/test_OH_hgb_ferr_outer_fold_3.csv")


## PREDICT FERR ----
dt.md.ferr.outer <- subset(hgb_ferr_training_data, select = -c(fu_hgb, fu_ferritin))  # predict log ferritin so remove hgb outcome and ferr outcome

### FACTOR ----
dt.factor.ferr <- copy(dt.md.ferr.outer)
char_columns <- colnames(dt.factor.ferr)[unlist(dt.factor.ferr[, lapply(.SD, is.character),][1,])]
char_columns <- setdiff(char_columns, "RandID")
dt.factor.ferr <- dt.factor.ferr %>%
  mutate_at(vars(char_columns), as.factor)


# actual training set with no Donor ID 
# repeat the following for the other two outer folds: outer fold 1 and outer fold 2
rsplit_factors_hgb_ferr_outer_fold <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_factors_hgb_ferr_outer_fold_3.rds")

actual_train_dt_hf_pred_f_outer_fold <- rsplit_factors_hgb_ferr_outer_fold$splits[[1]]$data

# match actual training set to the original data
# inner join to get all rows in original data that are in actual training data
matched_train_dt <- merge(actual_train_dt_hf_pred_f_outer_fold, dt.factor.ferr, by=c("rbc_loss_last_12_months", 
                                                                                    "rbc_loss_last_24_months",
                                                                                    "days_since_last_rbc_loss",
                                                                                    "days_since_last_drbc_loss",
                                                                                    "cum_lifetime_donations",
                                                                                    "index_hgb", "blood_type", "age", "sex", "time_to_fu", "fu_log_ferritin",
                                                                                    "index_ferritin", "index_log_ferritin"))

matched_train_dt_idx <- matched_train_dt$RandID
unique_train_dt_idx <- unique(matched_train_dt_idx)
length(unique_train_dt_idx) # same as the theoretical donor split in train set

all_dt_idx <- dt.factor.ferr$RandID
unique_all_dt_idx <- unique(all_dt_idx)
length(unique_all_dt_idx)

matched_test_dt_idx <- all_dt_idx[!(all_dt_idx %in% matched_train_dt_idx)]
unique_test_dt_idx <- unique(matched_test_dt_idx)
length(unique_test_dt_idx) # same as the theoretical donor split in test set

# recovered test set that should be used for nested CV
matched_test_dt <- dt.factor.ferr[RandID %in% unique_test_dt_idx]
print(nrow(matched_test_dt)) # same as the theoretical and actual donation test split within this outer fold

fwrite(matched_test_dt, "./3_intermediate/outer_fold_test_data/pred_ferr/test_factors_hgb_ferr_outer_fold_3.csv")

### ONE-HOT CODED ----
column_to_exclude <- "RandID"
dt.OH.ferr <- copy(dt.md.ferr.outer)
dt.OH.ferr <- data.table(model.matrix(fu_log_ferritin ~., data = dt.md.ferr.outer[, !column_to_exclude, with = FALSE]))
dt.OH.ferr <- cbind(dt.OH.ferr,
                    "RandID" = dt.md.ferr.outer$RandID,
                    "fu_log_ferritin" = dt.md.ferr.outer$fu_log_ferritin)

# actual training set with no Donor ID 
# repeat the following for the other two outer folds: outer fold 1 and outer fold 2
rsplit_oh_hgb_ferr_outer_fold <- readRDS("./3_intermediate/rsplits/nested_model/inner_fold/pred_ferr/rsplit_OH_hgb_ferr_outer_fold_3.rds")

actual_train_dt_hf_pred_f_outer_fold <- rsplit_oh_hgb_ferr_outer_fold$splits[[1]]$data

# match actual training set to the original data
# inner join to get all rows in original data that are in actual training data
matched_train_dt <- merge(actual_train_dt_hf_pred_f_outer_fold, dt.OH.ferr, by=c("(Intercept)", "rbc_loss_last_12_months", 
                                                                                "rbc_loss_last_24_months",
                                                                                "days_since_last_rbc_loss",
                                                                                "days_since_last_drbc_loss",
                                                                                "cum_lifetime_donations",
                                                                                "index_hgb", "blood_typeA+", "blood_typeAB-", "blood_typeAB+", "blood_typeB-", "blood_typeB+", "blood_typeO-", "blood_typeO+",
                                                                                "age", "sexM", "time_to_fu", "fu_log_ferritin",
                                                                                "index_ferritin", "index_log_ferritin"))

matched_train_dt_idx <- matched_train_dt$RandID
unique_train_dt_idx <- unique(matched_train_dt_idx)
length(unique_train_dt_idx) # same as the theoretical donor split in train set

all_dt_idx <- dt.OH.ferr$RandID
unique_all_dt_idx <- unique(all_dt_idx)
length(unique_all_dt_idx)

matched_test_dt_idx <- all_dt_idx[!(all_dt_idx %in% matched_train_dt_idx)]
unique_test_dt_idx <- unique(matched_test_dt_idx)
length(unique_test_dt_idx) # same as the theoretical donor split in test set

# recovered test set that should be used for nested CV
matched_test_dt <- dt.OH.ferr[RandID %in% unique_test_dt_idx]
print(nrow(matched_test_dt)) # same as the theoretical and actual donation test split within this outer fold

fwrite(matched_test_dt, "./3_intermediate/outer_fold_test_data/pred_ferr/test_OH_hgb_ferr_outer_fold_3.csv")


