## Group the validation results
result <- list()
result[[1]] <- read.csv('ML_all_methods_models_noresample_3253253.csv')
result[[2]] <- read.csv('ML_all_methods_models_noresample_453454.csv')
result[[3]] <- read.csv('ML_all_methods_models_noresample_6564535.csv')
result[[4]] <- read.csv('ML_all_methods_models_noresample_92837458.csv')
result[[5]] <- read.csv('ML_all_methods_models_noresample_738292.csv')
result[[6]] <- read.csv('ML_all_methods_models_resample_3253253.csv')
result[[7]] <- read.csv('ML_all_methods_models_resample_453454.csv')
result[[8]] <- read.csv('ML_all_methods_models_resample_6564535.csv')
result[[9]] <- read.csv('ML_all_methods_models_resample_92837458.csv')
result[[10]] <- read.csv('ML_all_methods_models_resample_738292.csv')


result_all <- do.call(rbind, result)
colnames(result_all) <- c("External_folder",
                          "Method",
                          "seed",
                          "Outcome",
                          "Survey",
                          "Demo",
                          "Resampling",
                          "AUC",
                          "cutoff1",
                          "Accuracy_train_0.9",
                          "Sensitivity_train_0.9",
                          "Specificity_train_0.9",
                          "Pos.Pred.Value_train_0.9",
                          "Neg.Pred.Value_train_0.9",
                          "F1.Sensitivity_train_0.9",
                          "Accuracy_test_0.9",
                          "Sensitivity_test_0.9",
                          "Specificity_test_0.9",
                          "Pos.Pred.Value_test_0.9",
                          "Neg.Pred.Value_test_0.9",
                          "F1.Sensitivity_test_0.9",
                          'cutoff2',
                          "Accuracy_train_0.95",
                          "Sensitivity_train_0.95",
                          "Specificity_train_0.95",
                          "Pos.Pred.Value_train_0.95",
                          "Neg.Pred.Value_train_0.95",
                          "F1.Sensitivity_train_0.95",
                          "Accuracy_test_0.95",
                          "Sensitivity_test_0.95",
                          "Specificity_test_0.95",
                          "Pos.Pred.Value_test_0.95",
                          "Neg.Pred.Value_test_0.95",
                          "F1.Sensitivity_test_0.95",
                          'cutoff3',
                          "Accuracy_train_0.85",
                          "Sensitivity_train_0.85",
                          "Specificity_train_0.85",
                          "Pos.Pred.Value_train_0.85",
                          "Neg.Pred.Value_train_0.85",
                          "F1.Sensitivity_train_0.85",
                          "Accuracy_test_0.85",
                          "Sensitivity_test_0.85",
                          "Specificity_test_0.85",
                          "Pos.Pred.Value_test_0.85",
                          "Neg.Pred.Value_test_0.85",
                          "F1.Sensitivity_test_0.85")

library(dplyr)

result_all <- as.data.frame(result_all)
summary_tab <- result_all %>% group_by(Method,
                                       Outcome,
                                       Survey,
                                       Demo,
                                       Resampling,
                                       seed) %>%
                          summarise(AUC_ave = median(AUC),
                                    Accuracy_train_0.9_ave = median(Accuracy_train_0.9),
                                    Sensitivity_train_0.9_ave = median(Sensitivity_train_0.9),
                                    Specificity_train_0.9_ave = median(Specificity_train_0.9),
                                    Pos.Pred.Value_train_0.9_ave = median(Pos.Pred.Value_train_0.9),
                                    Neg.Pred.Value_train_0.9_ave = median(Neg.Pred.Value_train_0.9),
                                    
                                    Accuracy_test_0.9_ave = median(Accuracy_test_0.9),
                                    Sensitivity_test_0.9_ave = median(Sensitivity_test_0.9),
                                    Specificity_test_0.9_ave = median(Specificity_test_0.9),
                                    Pos.Pred.Value_test_0.9_ave = median(Pos.Pred.Value_test_0.9),
                                    Neg.Pred.Value_test_0.9_ave = median(Neg.Pred.Value_test_0.9),
                                    
                                    Accuracy_train_0.95_ave = median(Accuracy_train_0.95),
                                    Sensitivity_train_0.95_ave = median(Sensitivity_train_0.95),
                                    Specificity_train_0.95_ave = median(Specificity_train_0.95),
                                    Pos.Pred.Value_train_0.9_ave = median(Pos.Pred.Value_train_0.9),
                                    Neg.Pred.Value_train_0.9_ave = median(Neg.Pred.Value_train_0.9),
                                    
                                    Accuracy_test_0.95_ave = median(Accuracy_test_0.95),
                                    Sensitivity_test_0.95_ave = median(Sensitivity_test_0.95),
                                    Specificity_test_0.95_ave = median(Specificity_test_0.95),
                                    
                                    Accuracy_train_0.85_ave = median(Accuracy_train_0.85),
                                    Sensitivity_train_0.85_ave = median(Sensitivity_train_0.85),
                                    Specificity_train_0.85_ave = median(Specificity_train_0.85),
                                    
                                    Accuracy_test_0.85_ave = median(Accuracy_test_0.85),
                                    Sensitivity_test_0.85_ave = median(Sensitivity_test_0.85),
                                    Specificity_test_0.85_ave = median(Specificity_test_0.85)) %>%
                        summarise(AUC_ave_ave = mean(AUC_ave),
                                  AUC_ave_sd = sd(AUC_ave),
                                  Accuracy_train_0.9_ave_ave = mean(Accuracy_train_0.9_ave),
                                  Accuracy_train_0.9_ave_sd = sd(Accuracy_train_0.9_ave),
                                  Sensitivity_train_0.9_ave_ave = mean(Sensitivity_train_0.9_ave),
                                  Sensitivity_train_0.9_ave_sd = sd(Sensitivity_train_0.9_ave),
                                  Specificity_train_0.9_ave_ave = mean(Specificity_train_0.9_ave),
                                  Specificity_train_0.9_ave_sd = sd(Specificity_train_0.9_ave),
                                  
                                  Accuracy_test_0.9_ave_ave = mean(Accuracy_test_0.9_ave),
                                  Accuracy_test_0.9_ave_sd = sd(Accuracy_test_0.9_ave),
                                  Sensitivity_test_0.9_ave_ave = mean(Sensitivity_test_0.9_ave),
                                  Sensitivity_test_0.9_ave_sd = sd(Sensitivity_test_0.9_ave),
                                  Specificity_test_0.9_ave_ave = mean(Specificity_test_0.9_ave),
                                  Specificity_test_0.9_ave_sd = sd(Specificity_test_0.9_ave),
                                  
                                  Accuracy_train_0.95_ave_ave = mean(Accuracy_train_0.95_ave),
                                  Accuracy_train_0.95_ave_sd = sd(Accuracy_train_0.95_ave),
                                  Sensitivity_train_0.95_ave_ave = mean(Sensitivity_train_0.95_ave),
                                  Sensitivity_train_0.95_ave_sd = sd(Sensitivity_train_0.95_ave),
                                  Specificity_train_0.95_ave_ave = mean(Specificity_train_0.95_ave),
                                  Specificity_train_0.95_ave_sd = sd(Specificity_train_0.95_ave),
                                  
                                  Accuracy_test_0.95_ave_ave = mean(Accuracy_test_0.95_ave),
                                  Accuracy_test_0.95_ave_sd = sd(Accuracy_test_0.95_ave),
                                  Sensitivity_test_0.95_ave_ave = mean(Sensitivity_test_0.95_ave),
                                  Sensitivity_test_0.95_ave_sd = sd(Sensitivity_test_0.95_ave),
                                  Specificity_test_0.95_ave_ave = mean(Specificity_test_0.95_ave),
                                  Specificity_test_0.95_ave_sd = sd(Specificity_test_0.95_ave),
                                  
                                  Accuracy_train_0.85_ave_ave = mean(Accuracy_train_0.85_ave),
                                  Accuracy_train_0.85_ave_sd = sd(Accuracy_train_0.85_ave),
                                  Sensitivity_train_0.85_ave_ave = mean(Sensitivity_train_0.85_ave),
                                  Sensitivity_train_0.85_ave_sd = sd(Sensitivity_train_0.85_ave),
                                  Specificity_train_0.85_ave_ave = mean(Specificity_train_0.85_ave),
                                  Specificity_train_0.85_ave_sd = sd(Specificity_train_0.85_ave),
                                  
                                  Accuracy_test_0.85_ave_ave = mean(Accuracy_test_0.85_ave),
                                  Accuracy_test_0.85_ave_sd = sd(Accuracy_test_0.85_ave),
                                  Sensitivity_test_0.85_ave_ave = mean(Sensitivity_test_0.85_ave),
                                  Sensitivity_test_0.85_ave_sd = sd(Sensitivity_test_0.85_ave),
                                  Specificity_test_0.85_ave_ave = mean(Specificity_test_0.85_ave),
                                  Specificity_test_0.85_ave_sd = sd(Specificity_test_0.85_ave)) %>%
                            arrange(Outcome,
                                    Survey,
                                    Demo,
                                    Resampling,
                                    AUC_ave_ave)




write.csv(summary_tab, 'summary_evaluation.csv')


## Print Table: auc summary
library(tidyr)
summary_tab_print <- summary_tab %>% as.data.frame %>%
                         # filter(Demo == 'Yes') %>%
                          mutate('AUC' = paste0(round(AUC_ave_ave, 2), 
                                                ' (', round(AUC_ave_sd, 2), 
                                                ')')) %>%
                          select(Outcome,
                                 Method,
                                 Survey,
                                 Resampling,
                                 Demo,
                                 AUC) %>% 
                          spread(Method, AUC)
write.csv(summary_tab_print, 'print_summary_evaluation.csv')

## Print Performance table: sensitiveity and specificity
library(tidyr)
summary_tab_print2 <- summary_tab %>% as.data.frame %>%
  #filter(Demo == 'Yes') %>%
  mutate('Accuracy_train' = paste0(round(Accuracy_train_0.85_ave_ave, 2), 
                        ' (', round(Accuracy_train_0.85_ave_sd, 2), 
                        ')'),
         'Sensitvity_train' = paste0(round(Sensitivity_train_0.85_ave_ave, 2), 
                                     ' (', round(Sensitivity_train_0.85_ave_sd, 2), 
                                     ')'),
         'Specificity_train' = paste0(round(Specificity_train_0.85_ave_ave, 2), 
                                     ' (', round(Specificity_train_0.85_ave_sd, 2), 
                                     ')'),
         'Accuracy_test' = paste0(round(Accuracy_test_0.85_ave_ave, 2), 
                                     ' (', round(Accuracy_test_0.85_ave_sd, 2), 
                                     ')'),
         'Sensitivity_test' = paste0(round(Sensitivity_test_0.85_ave_ave, 2), 
                                     ' (', round(Sensitivity_test_0.85_ave_sd, 2), 
                                     ')'),
         'Specificity_test' = paste0(round(Specificity_test_0.85_ave_ave, 2), 
                                     ' (', round(Specificity_test_0.85_ave_sd, 2), 
                                     ')')) %>%
  select(Outcome,
         Method,
         Survey,
         Resampling,
         Accuracy_train,
         Sensitvity_train,
         Specificity_train,
         Accuracy_test,
         Sensitivity_test,
         Specificity_test)
write.csv(summary_tab_print2, 'print_summary_evaluation2.csv')


best_tab <- result_all %>% group_by(Outcome,
                                    Survey,
                                    Demo) %>%
                            filter(Sensitivity_test_0.85 >= 0.8,
                                   Demo == 'Yes') %>%
                            filter(Specificity_test_0.85 == max(Specificity_test_0.85))

require(ggplot2)
ggplot(data = result_all %>% filter(Demo == 'Yes',
                                    Sensitivity_test_0.85 > 0.5), 
       aes(x = Sensitivity_test_0.85,
           y = Specificity_test_0.85,
           group = Method,
           color = Method)) + 
  facet_wrap(Resampling~ Outcome + Survey) + geom_point()
