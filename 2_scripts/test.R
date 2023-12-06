
# This script is to test new codes for nested cv

dt.md <- fread("./3_intermediate/private/hgb_ferr_rise.csv")
# dt.md <- fread( "./3_intermediate/private/hgb_ferr_rise.csv")
# dt.md <- fread( "./3_intermediate/private/hgb_only_rise.csv")

str(dt.md)

# set Gender_F as character since loads in as int
dt.md$sex <- as.character(dt.md$sex)

## Remove race variable ----
# identifiers <- c("RandID", "VisitDate", "VisitNum", 
#                  "rbc_loss_in_ml",
#                  "weight", "height", "bmi", "ebv", "red_cell_volume", "percent_rbc_loss", 
#                  "race")

identifiers <- c("VisitDate", "VisitNum", 
                 "rbc_loss_in_ml",
                 "weight", "height", "bmi", "ebv", "red_cell_volume", "percent_rbc_loss", 
                 "race")


#remove extraneous fields
setDT(dt.md)

dt.md[, c(identifiers) := NULL]

#set column order
setcolorder(dt.md, c("RandID", "rbc_loss_last_12_months"))

# 2 prediction tasks: predict hgb, predict ferritin
dt.md.hgb <- subset(dt.md, select = -c(fu_ferritin, fu_log_ferritin))  # predict hgb so remove ferr outcomes
dt.md.ferr <- subset(dt.md, select = -c(fu_hgb, fu_ferritin))  # predict log ferritin so remove hgb outcome and ferr outcome

# Create 2 versions: one with characters as factors and
# one with categorical variables 1-hot encoded for model types that require that

# One hot encoding for XGB -----
# Create dummy one-hot variables for categoricals

# remove donation ID first to do one hot encoding
dt.md.hgb.no.id <- dt.md.hgb[, -c("RandID")]
dt.OH.hgb <- data.table(model.matrix(fu_hgb ~ ., data=dt.md.hgb.no.id))
dt.OH.hgb <- cbind(dt.OH.hgb, "fu_hgb" = dt.md.hgb.no.id$fu_hgb)

# add donation ID back for random splits
dt.OH.hgb <- cbind(dt.OH.hgb, "RandID" = dt.md.hgb$RandID)
setcolorder(dt.OH.hgb, c("RandID", "rbc_loss_last_12_months"))


dt.md.ferr.no.id <- dt.md.ferr[, -c("RandID")]
dt.OH.ferr <- data.table(model.matrix(fu_log_ferritin ~ ., data=dt.md.ferr.no.id))
dt.OH.ferr <- cbind(dt.OH.ferr, "fu_log_ferritin" = dt.md.ferr.no.id$fu_log_ferritin)

# add donation ID back for random splits
dt.OH.ferr <- cbind(dt.OH.ferr, "RandID" = dt.md.ferr$RandID)
setcolorder(dt.OH.ferr, c("RandID", "rbc_loss_last_12_months"))

# library(lares)
# lares::corr_cross(dt.OH.hgb)  # Look at variance in predictors, near-zero variance
# lares::corr_cross(dt.OH.ferr)  # Look at variance in predictors, near-zero variance

# Code all Character as Factor -----
colnames(dt.md.hgb)[unlist(dt.md.hgb[ , lapply(.SD, is.character),][1,])]
dt.md.hgb <- dt.md.hgb %>% mutate_if(is.character, as.factor)
str(dt.md.hgb)

colnames(dt.md.ferr)[unlist(dt.md.ferr[ , lapply(.SD, is.character),][1,])]
dt.md.ferr <- dt.md.ferr %>% mutate_if(is.character, as.factor)
str(dt.md.ferr)

# split data ----

dt.md.hgb.id <- dt.md.hgb[, c("RandID")]
dt.md.hgb.id.unique <- unique(dt.md.hgb.id)

test_rsplit_res <- nested_cv(dt.md.hgb.id.unique,
                             outside = vfold_cv(v=5),
                             inside = vfold_cv(v=5, repeats = 2))

# the 5-fold cv in the outer loop
test_rsplit_res$inner_resamples

# extract the first 5-fold cv in the outer loop
outer_rsplit1 <- test_rsplit_res$inner_resamples[[1]]

# extract the first 5-fold cv repeat in the inner loop from the first 5-fold cv in the outer loop
inner_rsplit1 <- outer_rsplit1$splits[[1]] # rsplit_obj being used in the mod_eval()

# extract the actual donation ID that are assigned into the first inner fold repeat
# contains both training and test sets

train_set <- analysis(inner_rsplit1)
test_set <- assessment(inner_rsplit1)
data_in_rsplit1 <- inner_rsplit1$data

# add other donation information back 
train_set <- dt.md.hgb[RandID %in% train_set$RandID]
test_set <- dt.md.hgb[RandID %in% test_set$RandID]
dataset <- dt.md.hgb[RandID %in% data_in_rsplit1$RandID]

### test mod_eval() ----

# change response variable to "fu_outcome" for easier coding below
lookup <- c(fu_outcome = "fu_hgb", fu_outcome = "fu_log_ferritin")
train_set <- train_set %>% rename(any_of(lookup))
test_set <- test_set %>% rename(any_of(lookup))

ncol_rsplit <- ncol(train_set)  

# remove id
train_set <- train_set[, -c("RandID")]

# RANDOM FOREST
set.seed(250)
rf_param_sets <- fread("./3_intermediate/hyperparameters/rf_hyperparameters.csv")

mod_eval()



# use CY's code ----
dt_split <- readRDS("./3_intermediate/rsplits/main_model/pred_hgb/rsplit_factors_hgb_only.rds")

outer_split <- dt_split$inner_resamples[[1]]
inner_split <- outer_split$splits[[1]]

train_data <- analysis(inner_split)
test_data <- assessment(inner_split)


lookup <- c(fu_outcome = "fu_hgb", fu_outcome = "fu_log_ferritin")
train_data <- train_data %>% rename(any_of(lookup))
test_data <- test_data %>% rename(any_of(lookup))

ncol_rsplit <- ncol(train_data)  


# RANDOM FOREST
set.seed(250)
rf_param_sets <- fread("./3_intermediate/hyperparameters/rf_hyperparameters.csv")
rf_fit <- randomForest(fu_outcome~.,  # fu_hgb, fu_log_ferritin
                       data = train_data,
                       nodesize = rf_param_sets$nodesize,
                       mtry=rf_param_sets$mtry,
                       ntree = rf_param_sets$ntree,
                       replace = rf_param_sets$replace)


fname_results_test <- paste0("./3_intermediate/tune_results/main_model/", 
                        "predict_hgb",
                        "/",
                        "data_hgb_only",
                        "/",
                        "RF",
                        "_")

source('./2_scripts/utility_functions.R') 

tune_subset(mod_name="RF", 
            param_sets=rf_param_sets, 
            dt_split=dt_split,
            fname_results=fname_results_test,
            start=1, 
            end=5)  

run_mod_assess("RF",
               rf_param_sets, dt_split,
               idx_start = 1,
               fname_results = fname_results_test)



















saveRDS(test2_rsplit_res, "./3_intermediate/test2_rsplit.rds")
fwrite(test2_rsplit_res, "./3_intermediate/dt.md.hgb.csv")

test2_rsplit_res

sapply(test2_rsplit_res, typeof)
test_rsplit_res[[1]]

table(sapply(test_rsplit_res[[1]], typeof))

dt.md.hgb.no.id <- dt.md.hgb[, -c("RandID")]

test3_rsplit_res <- nested_cv(dt.md.hgb.no.id,
                              outside = vfold_cv(v=5),
                              inside = vfold_cv(v=5, repeats = 2))

save_cv_split(dt.md.hgb.no.id, "./3_intermediate/test3_rsplit.rds")
fwrite(dt.md.hgb.no.id, "./3_intermediate/test3_rsplit_res_resample.csv")

dt.md.hgb.id <- dt.md.hgb[, c("RandID")]

test3_rsplit_res_id <- nested_cv(dt.md.hgb.id,
                              outside = vfold_cv(v=5),
                              inside = vfold_cv(v=5, repeats = 2))

save_cv_split(dt.md.hgb.id, "./3_intermediate/test3_rsplit_id.rds")
fwrite(dt.md.hgb.id, "./3_intermediate/test3_rsplit_res_resample_id.csv")

t1 <- test3_rsplit_res_id$inner_resamples[[1]]
t1$splits[[1]]


test_split <- nested_cv(dt.md.hgb,
                        outside = vfold_cv(v=5),
                        inside = vfold_cv(v=5, repeats = 2))

inner_split <- test_split$inner_resamples[[1]]
rsplit_obj1 <- inner_split$splits[[1]]

train_data <- analysis(rsplit_obj1)  
test_data <-  assessment(rsplit_obj1)


dt_inner_fold_preds <- data.table("prediction"= numeric(),
                                  "fu_outcome"= numeric())

for (row_inner in 1:nrow(inner_split)){
  #print(paste0("  inner row ", row_inner))
  tryCatch({
    dt_inner_fold_preds <- rbind(
      dt_inner_fold_preds,
      mod_eval(inner_split$splits[[row_inner]], params, mod_name)
    )
  }, error = function(error_condition){
    cat("Could not fit on this inner fold")
  })
}




