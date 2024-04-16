# Help functions for creating cross_validation and nested cross validation objects

# Replace donors in rsplit_res with actual donations; remove DonorID
replace_rsplit_data <- function(rsplit_output, dt) {
  
  # Make a copy of the data without DonorID to replate data in rsplit
  dt_md_no_donorID <- dt[, -c('RandID')]
  
  splits_list <- rsplit_output$splits
  
  for (i in seq_along(splits_list)) {
    # print(i)
    # Access the rsplit object for the current fold
    current_split <- splits_list[[i]]
    
    # Get int[] in_id (list of indices in the training data of the fold)
    train_donor_idxs <- current_split$in_id 
    
    # Get all donors that are inside of training data
    donors_data <- current_split$data
    train_donorIDs <- donors_data[train_donor_idxs, ]
    
    # Get all donation indices that correspond to these donors
    train_donation_idxs <- dt[RandID %in% train_donorIDs, .I]
    train_donation_idxs <- unlist(train_donation_idxs) # make int[]
    
    # Replace donor data with donations data
    current_split$data <- dt_md_no_donorID
    # Replace in_ids with train_donation_idxs
    current_split$in_id <- train_donation_idxs
    
    rsplit_output$splits[[i]] <- current_split
  }
  
  return(rsplit_output)
}


# Replace donors in rsplit_res with actual donations; keep DonorID
replace_rsplit_data_with_id <- function(rsplit_output, dt) {
  
  # Make a copy of the data without DonorID to replate data in rsplit
  # dt_md_no_donorID <- dt[, -c('RandID')]
  
  splits_list <- rsplit_output$splits
  
  for (i in seq_along(splits_list)) {
    print(i)
    # Access the rsplit object for the current fold
    current_split <- splits_list[[i]]
    
    # Get int[] in_id (list of indices in the training data of the fold)
    train_donor_idxs <- current_split$in_id 
    
    # Get all donors that are inside of training data
    donors_data <- current_split$data
    train_donorIDs <- donors_data[train_donor_idxs, ]
    
    # Get all donation indices that correspond to these donors
    train_donation_idxs <- dt[RandID %in% train_donorIDs, .I]
    train_donation_idxs <- unlist(train_donation_idxs) # make int[]
    
    # Replace donor data with donations data
    current_split$data <- dt
    # Replace in_ids with train_donation_idxs
    current_split$in_id <- train_donation_idxs
    
    rsplit_output$splits[[i]] <- current_split
  }
  
  return(rsplit_output)
}

create_OH_var <- function(col_to_exclude, dt.md.outer, fu_outcome){
  column_to_exclude <- col_to_exclude
  if (fu_outcome == "fu_hgb"){
    dt.OH <- copy(dt.md.outer)
    dt.OH <- data.table(model.matrix(fu_hgb ~., data = dt.md.outer[, !column_to_exclude, with = FALSE]))
    dt.OH <- cbind(dt.OH,
                   "RandID" = dt.md.outer$RandID,
                   "fu_hgb" = dt.md.outer$fu_hgb)
  }else if (fu_outcome == "fu_log_ferritin"){
    dt.OH <- copy(dt.md.outer)
    dt.OH <- data.table(model.matrix(fu_log_ferritin ~., data = dt.md.outer[, !column_to_exclude, with = FALSE]))
    dt.OH <- cbind(dt.OH, 
                   "RandID" = dt.md.outer$RandID,
                   "fu_log_ferritin" = dt.md.outer$fu_log_ferritin)
  }
  return(dt.OH)
}

create_factor_var <- function(dt.md.outer){
  dt.factor <- copy(dt.md.outer)
  char_columns <- colnames(dt.factor)[unlist(dt.factor[, lapply(.SD, is.character),][1,])]
  char_columns <- setdiff(char_columns, "RandID")
  dt.factor <- dt.factor %>%
    mutate_at(vars(char_columns), as.factor)
  return(dt.factor)
}

get_unique_donors <- function(dt.training){
  hgb_unique_donors <- unique(dt.training$RandID)
  print(paste0(length(hgb_unique_donors), " unique donors"))
  dt_hgb_unique_donors <- data.frame(DonorID = hgb_unique_donors)
  return(dt_hgb_unique_donors)
}

remove_extra_cols <- function(dt.training){
  dt.training$sex <- as.character(dt.training$sex)
  
  # remove extraneous fields
  identifiers <- c("VisitDate", "VisitNum", 
                   "rbc_loss_in_ml",
                   "weight", "height", "bmi", "ebv", "red_cell_volume", "percent_rbc_loss", 
                   "race")
  
  dt.training[, c(identifiers) := NULL]
  

  return(dt.training)
}

retrieve_outer_fold_data <- function(dt.training, rsplit, var_version, data_version){
  for (i in 1:length(rsplit$splits)){
    donor_outer <- analysis(rsplit$splits[[i]])
    training_outer <- dt.training[RandID %in% donor_outer$DonorID]
    fwrite(training_outer, paste0("./3_intermediate/private/outer_fold/", data_version, "_", var_version, "_outer_fold_", i, ".csv"))
  }
}
