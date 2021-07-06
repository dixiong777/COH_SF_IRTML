## Run all 12 models and get the results.
All_Models <- function(C_D, P_D, C_refer, P_refer,
                       model_function, model_name,
                       n_fold, seed, 
                       imbalance_adjustment) {
  ###################################
  # Active Cavity
  #################################
  ## 1. Child, noDemo: AC-SRO
  X_ac_sro_nodemo = C_D[, c(# demo
    # "C_AGE",
    # "C_GENDER",
    # "C_RACE",
    # "C_LANG",
    # "C_N_LIV",
    #"Theta_SF"
    # SF
    "C_CHOPI",
    "C_CHOPK",
    "C_CHOPJ",
    "C_CHOPF",
    "C_CHOPG",
    "C_PAIN",
    "C_SWALLOW",
    "C_ANXIET2A",
    "C_BRUSH",
    "C_AFRAID"
    # not SF
    #"C_SOCNETE",
    #"C_DEVELOP1"
  )] 
  
  y_ac_sro_nodemo = factor(C_D$E_D > 0,
                           labels = c(0, 1))
  
  child_ac_sf_nodemo <- external_partition_ML(X_ac_sro_nodemo, y_ac_sro_nodemo, 
                                              n_fold, seed = seed, 
                                              imbalance_adjustment, 
                                              model_function = model_function)
  print(paste0(model_name, ' Model 1: Done \n'))
  
  ## 2. Child, with Demo: AC-SRO
  X_ac_sro = C_D[, c(# demo
    "C_AGE",
    "C_GENDER",
    "C_RACE",
    "C_LANG",
    "C_N_LIV",
    #"Theta_SF"
    # SF
    "C_CHOPI",
    "C_CHOPK",
    "C_CHOPJ",
    "C_CHOPF",
    "C_CHOPG",
    "C_PAIN",
    "C_SWALLOW",
    "C_ANXIET2A",
    "C_BRUSH",
    "C_AFRAID"
    # not SF
    #"C_SOCNETE",
    #"C_DEVELOP1"
  )] 
  y_ac_sro = factor(C_D$E_D > 0,
                    labels = c(0, 1))
  child_ac_sf <- external_partition_ML(X_ac_sro, y_ac_sro, n_fold, seed = seed, 
                                       imbalance_adjustment, 
                                       model_function = model_function)
  print(paste0(model_name, ' Model 2: Done \n'))
  
  ## 3. Parent, noDemo: AC-PRO
  X_ac_pro_nodemo = P_D[, c(# demo
    # "P_AGE",
    # "P_CHL_RAC",
    # "P_GENDER",
    # "P_LANG",
    # "P_RACE",
    # "P_INSUR",
    # "P_EMP",
    # "P_EUC",
    # "P_MARRSTAT",
    # "Theta_SF"
    # SF
    "P_CHOPF",
    "P_CHOPE",
    "P_CHOPA",
    "P_CHOPC",
    "P_SCHMISS",
    "P_SH1",
    "P_CHOPG",
    "P_CHOPB",
    "P_CHOPD",
    "P_SELFEFF2"
    # not SF
    # "P_WORRY",
    # "P_OHEFFECT",
    # "P_AFRAID",
    # "P_SOCNET1B",
    # "P_SELFEFF",
    # "P_SOCNET1E",
    # "P_ACCESS1"
  )] 
  y_ac_pro_nodemo = factor(P_D$E_D > 0,
                           labels = c(0, 1))
  parent_ac_sf_nodemo <- external_partition_ML(X_ac_pro_nodemo, y_ac_pro_nodemo, 
                                               n_fold, seed = seed, 
                                               imbalance_adjustment, 
                                               model_function = model_function)
  print(paste0(model_name, ' Model 3: Done \n'))
  
  ## 4. Parent, noDemo: AC-PRO
  X_ac_pro = P_D[, c(# demo
    "P_AGE",
    "P_CHL_RAC",
    "P_GENDER",
    "P_LANG",
    "P_RACE",
    "P_INSUR",
    "P_EMP",
    "P_EUC",
    "P_MARRSTAT",
    # "Theta_SF"
    # SF
    "P_CHOPF",
    "P_CHOPE",
    "P_CHOPA",
    "P_CHOPC",
    "P_SCHMISS",
    "P_SH1",
    "P_CHOPG",
    "P_CHOPB",
    "P_CHOPD",
    "P_SELFEFF2"
    # not SF
    # "P_WORRY",
    # "P_OHEFFECT",
    # "P_AFRAID",
    # "P_SOCNET1B",
    # "P_SELFEFF",
    # "P_SOCNET1E",
    # "P_ACCESS1"
  )] 
  y_ac_pro = factor(P_D$E_D > 0,
                    labels = c(0, 1))
  parent_ac_sf <- external_partition_ML(X_ac_pro, y_ac_pro, n_fold, seed = seed, 
                                        imbalance_adjustment, 
                                        model_function = model_function)
  print(paste0(model_name, ' Model 4: Done \n'))
  
  ## 5. ALL, noDemo: AC-PRO
  X_ac_all_nodemo = cbind.data.frame(X_ac_sro_nodemo, X_ac_pro_nodemo)
  y_ac_all_nodemo = factor(P_D$E_D > 0,
                           labels = c(0, 1))
  all_ac_sf_nodemo <-  external_partition_ML(X_ac_all_nodemo, y_ac_all_nodemo, 
                                             n_fold, seed = seed, 
                                             imbalance_adjustment, 
                                             model_function = model_function)
  print(paste0(model_name, ' Model 5: Done \n'))
  
  ## 6. ALL, with Demo: AC-PRO
  X_ac_all = cbind.data.frame(X_ac_sro, X_ac_pro)
  y_ac_all = factor(P_D$E_D > 0,
                    labels = c(0, 1))
  all_ac_sf <-  external_partition_ML(X_ac_all, y_ac_all, n_fold, seed = seed, 
                                      imbalance_adjustment, 
                                      model_function = model_function)
  print(paste0(model_name, ' Model 6: Done \n'))
  
  
  #####################################
  # Referral Recommendation for Urgent Needs
  #####################################
  ## 7. Child, no Demo: RFUTN-SRO
  X_rfutn_sro_nodemo = C_refer[, c(# demo
    # "C_AGE",
    # "C_GENDER",
    # "C_RACE",
    # "C_LANG",
    # "C_N_LIV",
    #"Theta_SF"
    # SF
    "C_CHOPI",
    "C_CHOPK",
    "C_CHOPJ",
    "C_CHOPH",
    "C_CHOPF",
    "C_CHOPG",
    "C_CHOPB",
    "C_CHOPE",
    "C_CHOPA",
    "C_SCHMISS"
    # not SF
    # "C_PAIN",
    # "C_SWALLOW",
    # "C_SOCCOMP",
    # "C_BRUSH",
    # "C_AFRAID",
    # "C_BREATH1",
    # "C_SOCNET1F",
    # "C_SELFEFF1",
    # "C_SOCNET1D"
  )] 
  y_rfutn_sro_nodemo = factor(C_refer$E_refer2 %in% c("See a dentist immediately", 
                                                      "See a dentist within the next 2 weeks"),
                              labels = c(0, 1))
  child_refer_sf_nodemo <- external_partition_ML(X_rfutn_sro_nodemo, y_rfutn_sro_nodemo,
                                                 n_fold, seed = seed, 
                                                 imbalance_adjustment, 
                                                 model_function = model_function)
  print(paste0(model_name, ' Model 7: Done \n'))
  
  ## 8. Child, with Demo: RFUTN-SRO
  X_rfutn_sro = C_refer[, c(# demo
    "C_AGE",
    "C_GENDER",
    "C_RACE",
    "C_LANG",
    "C_N_LIV",
    #"Theta_SF"
    # SF
    "C_CHOPI",
    "C_CHOPK",
    "C_CHOPJ",
    "C_CHOPH",
    "C_CHOPF",
    "C_CHOPG",
    "C_CHOPB",
    "C_CHOPE",
    "C_CHOPA",
    "C_SCHMISS"
    # not SF
    # "C_PAIN",
    # "C_SWALLOW",
    # "C_SOCCOMP",
    # "C_BRUSH",
    # "C_AFRAID",
    # "C_BREATH1",
    # "C_SOCNET1F",
    # "C_SELFEFF1",
    # "C_SOCNET1D"
  )] 
  y_rfutn_sro = factor(C_refer$E_refer2 %in% c("See a dentist immediately", 
                                               "See a dentist within the next 2 weeks"),
                       labels = c(0, 1))
  child_refer_sf <- external_partition_ML(X_rfutn_sro, y_rfutn_sro, n_fold, seed = seed, 
                                          imbalance_adjustment, 
                                          model_function = model_function)
  print(paste0(model_name, ' Model 8: Done \n'))
  
  ## 9. Parent, noDemo: RFUTN-PRO
  X_rfutn_pro_nodemo = P_refer[, c(# demo
    # "P_AGE",
    # "P_CHL_RAC",
    # "P_GENDER",
    # "P_LANG",
    # "P_RACE",
    # "P_INSUR",
    # "P_EMP",
    # "P_EUC",
    # "P_MARRSTAT",
    # "Theta_SF"
    # sf
    "P_CHOPF",
    "P_CHOPE",
    "P_CHOPA",
    "P_CHOPC",
    "P_SCHMISS",
    "P_ACCESS6",
    "P_SH1",
    "P_AFRAID",
    "P_SOCCOM2",
    "P_SELFEFF7"
    # not-sf
    # "P_WORRY",
    # "P_OHEFFECT",
    # "P_SELFEFF1",
    # "P_COLOR_BAD",
    # "P_SOCNET1B"
  )] 
  y_rfutn_pro_nodemo = factor(P_refer$E_referral %in% c("See a dentist immediately", 
                                                            "See a dentist within the next 2 weeks"),
                                  labels = c(0, 1))
  parent_refer_sf_nodemo <- external_partition_ML(X_rfutn_pro_nodemo, y_rfutn_pro_nodemo, n_fold, seed = seed, 
                                                      imbalance_adjustment, 
                                                      model_function = model_function)
  print(paste0(model_name, ' Model 9: Done \n'))
  
  ## 10. Parent, Demo: RFUTN-PRO
  X_rfutn_pro = P_refer[, c(# demo
    "P_AGE",
    "P_CHL_RAC",
    "P_GENDER",
    "P_LANG",
    "P_RACE",
    "P_INSUR",
    "P_EMP",
    "P_EUC",
    "P_MARRSTAT",
    # "Theta_SF"
    # sf
    "P_CHOPF",
    "P_CHOPE",
    "P_CHOPA",
    "P_CHOPC",
    "P_SCHMISS",
    "P_ACCESS6",
    "P_SH1",
    "P_AFRAID",
    "P_SOCCOM2",
    "P_SELFEFF7"
    # not-sf
    # "P_WORRY",
    # "P_OHEFFECT",
    # "P_SELFEFF1",
    # "P_COLOR_BAD",
    # "P_SOCNET1B"
  )] 
  y_rfutn_pro = factor(P_refer$E_referral %in% c("See a dentist immediately", 
                                                 "See a dentist within the next 2 weeks"),
                       labels = c(0, 1))
  parent_refer_sf <- external_partition_ML(X_rfutn_pro, y_rfutn_pro, n_fold, seed = seed, 
                                           imbalance_adjustment, 
                                           model_function = model_function)
  print(paste0(model_name, ' Model 10: Done \n'))
  
  ## 11. ALL, noDemo: RFUTN-PRO
  X_rfutn_all_nodemo = cbind.data.frame(X_rfutn_sro_nodemo, X_rfutn_pro_nodemo)
  y_rfutn_all_nodemo = factor(P_refer$E_referral %in% c("See a dentist immediately", 
                                                        "See a dentist within the next 2 weeks"),
                              labels = c(0, 1))
  all_refer_sf_nodemo <-  external_partition_ML(X_rfutn_all_nodemo, y_rfutn_all_nodemo,
                                                n_fold, seed = seed, 
                                                imbalance_adjustment, 
                                                model_function = model_function)
  print(paste0(model_name, ' Model 11: Done \n'))
  
  ## 12. ALL, with Demo: RFUTN-PRO
  X_rfutn_all = cbind.data.frame(X_rfutn_sro, X_rfutn_pro)
  y_rfutn_all = factor(P_refer$E_referral %in% c("See a dentist immediately", 
                                                 "See a dentist within the next 2 weeks"),
                       labels = c(0, 1))
  all_refer_sf <-  external_partition_ML(X_rfutn_all, y_rfutn_all, n_fold, seed = seed, 
                                         imbalance_adjustment, 
                                         model_function = model_function)
  print(paste0(model_name, ' Model 12: Done \n'))
  
  ### Results
  result_table <- get_fold_result(result_list = list(child_ac_sf_nodemo,
                                                     child_ac_sf, 
                                                     parent_ac_sf_nodemo,
                                                     parent_ac_sf, 
                                                     all_ac_sf_nodemo,
                                                     all_ac_sf, 
                                                     child_refer_sf_nodemo,
                                                     child_refer_sf, 
                                                     parent_refer_sf_nodemo,
                                                     parent_refer_sf, 
                                                     all_refer_sf_nodemo,
                                                     all_refer_sf))
  model_inf <-  data.frame(Method = model_name,
                           seed = seed,
                           Outcome = rep(c('AC', 'Referral'), each = 30),
                           Survey = rep(rep(c('child', 'parent', 'all'), each = 10), 2),
                           Demo = rep(rep(c('No', 'Yes'), each = 5), 6),
                           Resampling = rep(imbalance_adjustment, 60))
  
  result_table <- cbind(model_inf, result_table)
  #write.csv(result_table, paste0('ML_result_', model_name, '_', seed, '_', imbalance_adjustment, '.csv'))
  return(result_table)
}

