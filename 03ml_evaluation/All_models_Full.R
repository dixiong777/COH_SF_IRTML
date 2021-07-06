## Run Models with all children and parent items
All_Models_Full <- function(C_ALL, P_ALL,
                       model_function, model_name,
                       n_fold, seed, 
                       imbalance_adjustment) {
  ###################################
  # Active Cavity
  #################################
  ## 1. Child
  X_child_ac = C_ALL[, -match(c('E_D', 'E_refer2'), colnames(C_ALL))]
  y_child_ac = factor(C_ALL$E_D > 0,
                    labels = c(0, 1))
  child_ac <- external_partition_ML(X_child_ac, y_child_ac, n_fold, seed = seed, 
                                       imbalance_adjustment, 
                                       model_function = model_function)
  print(paste0(model_name, ' Children-AC: Done \n'))
  
  ## 2. Parent
  X_parent_ac = P_ALL[, -match(c('E_D', 'E_refer2'), colnames(P_ALL))]
  y_parent_ac = factor(P_ALL$E_D > 0,
                labels = c(0, 1))
  parent_ac <- external_partition_ML(X_parent_ac, y_parent_ac, n_fold, seed = seed, 
                                    imbalance_adjustment, 
                                    model_function = model_function)
  print(paste0(model_name, ' Parent-AC: Done \n'))
  
  ## 3. ALL
  X_all_ac = cbind.data.frame(X_child_ac, X_parent_ac)
  X_all_ac <- X_all_ac[, !duplicated(colnames(X_all_ac))]
  y_all_ac = factor(P_ALL$E_D > 0,
                       labels = c(0, 1))
  all_ac <- external_partition_ML(X_all_ac, y_all_ac, n_fold, seed = seed, 
                                    imbalance_adjustment, 
                                    model_function = model_function)
  print(paste0(model_name, ' ALL-AC: Done \n'))

  ###################################
  # RFUTN
  #################################
  ## 1. Child
  X_child_RFUTN = C_ALL[, -match(c('E_D', 'E_refer2'), colnames(C_ALL))]
  y_child_RFUTN = factor(C_ALL$E_refer2 %in% c("See a dentist immediately", 
                                               "See a dentist within the next 2 weeks"),
                         labels = c(0, 1))
  child_RFUTN <- external_partition_ML(X_child_ac, y_child_ac, n_fold, seed = seed, 
                                    imbalance_adjustment, 
                                    model_function = model_function)
  print(paste0(model_name, ' Children-RFUTN: Done \n'))
  
  ## 2. Parent
  X_parent_RFUTN = P_ALL[, -match(c('E_D', 'E_refer2'), colnames(P_ALL))]
  y_parent_RFUTN = factor(P_ALL$E_refer2 %in% c("See a dentist immediately", 
                                             "See a dentist within the next 2 weeks"),
                       labels = c(0, 1))
  parent_RFUTN <- external_partition_ML(X_parent_ac, y_parent_ac, n_fold, seed = seed, 
                                    imbalance_adjustment, 
                                    model_function = model_function)
  print(paste0(model_name, ' Parent-RFUTN: Done \n'))
  
  ## 3. ALL
  X_all_RFUTN = cbind.data.frame(X_child_RFUTN, X_parent_RFUTN)
  X_all_RFUTN <- X_all_RFUTN[, !duplicated(colnames(X_all_RFUTN))]
  y_all_RFUTN = factor(P_ALL$E_refer2 %in% c("See a dentist immediately", 
                                          "See a dentist within the next 2 weeks"),
                    labels = c(0, 1))
  all_RFUTN <- external_partition_ML(X_all_RFUTN, y_all_RFUTN, n_fold, seed = seed, 
                                  imbalance_adjustment, 
                                  model_function = model_function)
  print(paste0(model_name, ' ALL-RFUTN: Done \n'))
  
  ### Results
  result_table <- get_fold_result(result_list = list(child_ac, parent_ac, all_ac,
                                                     child_RFUTN, parent_RFUTN, all_RFUTN))
  model_inf <-  data.frame(Method = model_name,
                           seed = seed,
                           Outcome = rep(c('AC', 'Referral'), each = 15),
                           Survey = rep(rep(c('child', 'parent', 'all'), each = 5), 2),
                           Demo = 'Yes',
                           Resampling = rep(imbalance_adjustment, 30))
  
  result_table <- cbind(model_inf, result_table)
  #write.csv(result_table, paste0('ML_result_', model_name, '_', seed, '_', imbalance_adjustment, '.csv'))
  return(result_table)
}

