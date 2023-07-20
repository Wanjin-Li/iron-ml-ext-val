#calculate and plot shap values
library(tidyverse)
library(xgboost)
library(caret)
library(shapr)
library(kernelshap)
library(shapviz)
library(ggplot2)
library(stringr)
library(mltools)

#load datasets
#predicting HGB
dt_hf_h <- fread("./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_factors_hgb_ferr.csv")
dt_hf_h_OH <-fread("./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_OH_hgb_ferr.csv")

dt_h_h <- fread("./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_factors_hgb_only.csv")
dt_h_h_OH <- fread("./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_OH_hgb_only.csv")

### Predict ferritin ----

dt_hf_f <- fread("./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_factors_hgb_ferr.csv")
dt_h_f <- fread("./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_factors_hgb_only.csv")

dt_hf_f_OH <- fread("./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_OH_hgb_ferr.csv")
dt_h_f_OH <- fread("./3_intermediate/model_dev_data/main_model/pred_ferr/mdset_OH_hgb_only.csv")

list_of_dts <- list(dt_hf_h, dt_h_h, dt_hf_f, dt_h_f) #put all dt tables in a list for easy access
list_of_dts_OH <- list(dt_hf_h_OH, dt_h_h_OH, dt_hf_f_OH, dt_h_f_OH) #put all dt_OH tables in a list for easy access

# prediction function for ensemble models
pred_fun <- function(mods, X) {
  predictions <- c()
  mod_names<-c("RF","XGB","XGB","XGB","XGB","XGB")
  i<-1
  
  # changing data types
  X$blood_type<-as.factor(X$blood_type)
  X$sex<-as.factor(X$sex)
  
  for (mod in mods) {  # for each model in ensemble
    mod_name<- mod_names[i]
    print(mod_name)
    
    colnames(X)[colnames(X) == 'fu_log_ferritin'] <- 'fu_outcome' #change col name
    colnames(X)[colnames(X) == 'fu_hgb'] <- 'fu_outcome' #change col name
   
    # Decide which test data set (One hot or factors to use) 
    if (mod_name == "XGB") {
        test_data <- one_hot(X)
        # Select the specified columns from the data table to ensure it is the same as dt_OH
        test_data <- test_data[, -c('blood_type_A-', 'sex_F'), with=FALSE]
        col_names <- c("rbc_loss_last_12_months", "rbc_loss_last_24_months", "days_since_last_rbc_loss",
                           "days_since_last_drbc_loss", "cum_lifetime_donations", "index_hgb",
                           "blood_typeA+", "blood_typeAB-", "blood_typeAB+", "blood_typeB-",
                           "blood_typeB+", "blood_typeO-", "blood_typeO+", "age", "sexM",
                           "index_ferritin", "index_log_ferritin", "time_to_fu", 'fu_outcome')
       
        #change names to be consistent
        setnames(test_data, old = names(test_data), new = col_names)
        test_data['(Intercept)']<-1
        print(names(test_data))
        # Extract test set in xgb's special format
        xgb.test = xgb.DMatrix(data = as.matrix(test_data[,-"fu_outcome"]),  # fu_hgb, fu_log_ferritin
                               label = as.matrix(test_data[,"fu_outcome"]))  # fu_hgb, fu_log_ferritin
        predictions <- c(predictions, predict(mod, xgb.test, reshape = T))
    }
    else {
        test_data <- X
        predictions<-c(predictions, predict(mod, test_data)) 
    }
    i<-i+1
  }
  ensemble_prediction <- mean(predictions)  # Take the average of predictions as the ensemble prediction
  return(ensemble_prediction)
 
}

x<-pred_fun(fitted_model1, test_dt)

#____________________________________________________________________________________________________
# Predict hemoglobin top model using hgb ferr: Ensemble (6 base models)
i<-1
sample(1)
ind <- sample(nrow(list_of_dts[[i]]), 500) #random sample of 500
test_dt<- list_of_dts[[i]][ind,]

version = "hgb_ferr_predict_hgb"
res_dir='./3_intermediate/external_validation/sanbs/'

version_pattern <- paste0(version, "*")
model_paths <- list.files(path="./3_intermediate/trained_models", pattern=version_pattern, full.names = TRUE) 

fitted_model1<-list() #list of models in ensemble

for (path in model_paths) {  # for each model in ensemble
  base_model_name <- gsub("^.*_", "", path)  # gets the last element after splitting by "_"
  base_model_name <- gsub(".rds", "", base_model_name)  # remove ".rds" -> XGB.4076
  
  mod<- readRDS(path) # load model
  fitted_model1<-c(fitted_model1, list(mod))
}

system.time(
  shap_hf_h <- kernelshap(fitted_model1, test_dt, bg_X = test_dt, pred_fun=pred_fun)
)
sv_hf_h <- shapviz(shap_hf_h)
sv1<-sv_importance(sv_hf_h, kind='bee', show_numbers = TRUE)

#____________________________________________________________________________________________________
# Predict hemoglobin top model using hgb only: Ensemble (6 base models)
i<-2
sample(1)
ind <- sample(nrow(list_of_dts[[i]]), 500) #random sample of 500
test_dt<- list_of_dts[[i]][ind,]


#____________________________________________________________________________________________________
# Predict ferritin top model using hgb ferr: EN.1
i<-3
sample(1)
ind <- sample(nrow(list_of_dts[[i]]), 500) #random sample of 500
test_dt<- list_of_dts[[i]][ind,]

test_dt <- data.matrix(test_dt[ , 1:(ncol(test_dt)-1)])  # remove last column (outcome) 
fitted_model3 <- readRDS("./3_intermediate/trained_models/hgb_ferr_predict_ferr_1EN_EN.1.rds")  # load model

system.time(
  shap_hf_f <- kernelshap(fitted_model3, test_dt, bg_X = test_dt)
)
sv_hf_f <- shapviz(shap_hf_f)
sv3<-sv_importance(sv_hf_f, kind = "bee", show_numbers = TRUE)

#____________________________________________________________________________________________________
# Predict ferritin top model using hgb only: RF.125
i<-4
sample(1)
ind <- sample(nrow(list_of_dts[[i]]), 500) #random sample of 500
test_dt<- list_of_dts[[i]][ind,]

colnames(test_dt)[colnames(test_dt) == 'fu_log_ferritin'] <- 'fu_outcome' #change col name

# changing data types
test_dt$blood_type<-as.factor(test_dt$blood_type)
test_dt$sex<-as.factor(test_dt$sex)

fitted_model4 <- readRDS("./3_intermediate/trained_models/hgb_only_predict_ferr_1RF_RF.125.rds")  # load model

system.time(
  shap_h_f <- kernelshap(fitted_model4, test_dt, bg_X = test_dt)
)
sv_h_f <- shapviz(shap_h_f)
sv4<-sv_importance(sv_h_f, kind = "bee", show_numbers = TRUE)

#________________________________________________________________________________________________________

#Generate and save 2x2 figure 
# Save the figure as an .svg file
svg(file = "./4_output/figs/shap/shap.svg", width = 6, height = 6)  # Adjust width and height as needed
par(mfrow = c(2, 2))
sv1
sv2
sv3
sv4
par(mfrow = c(1, 1))
dev.off()

