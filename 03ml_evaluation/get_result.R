# get_result
# extract summary statistics for the results.
get_result <- function(result_list){
  return(t(sapply(result_list, summary_result)))
}

summary_fold_result <- function(result_list){
  return(t(apply(result_list, 2, summary_result)))
}

get_fold_result <- function(result_list){
  
  list_result <- lapply(result_list, summary_fold_result)
  matrix_result <- do.call(rbind, list_result)
  return(matrix_result)
  
}

summary_result <- function(data = data){
  tab0.9 <- c(0.9, each_result(data$train_data_preformance, data$testing_data_performance))
  tab0.95 <- c(0.95, each_result(data$train_data_preformance2, data$testing_data_performance2))
  tab0.85 <- c(0.85, each_result(data$train_data_preformance3, data$testing_data_performance3))
  roc <- as.numeric(max(data$model_tune$results['ROC'], na.rm = TRUE))
  return(c(roc, tab0.9, tab0.95, tab0.85))
}
  
each_result <- function(train_data, test_data){
  train_result <- c(train_data$overall[c('Accuracy')],
                    train_data$byClass[c('Sensitivity', 
                                        'Specificity', 
                                        'Pos Pred Value', 
                                        'Neg Pred Value')])
  F1_train <- 2 * train_result['Sensitivity'] * train_result['Pos Pred Value'] / (train_result['Sensitivity'] + train_result['Pos Pred Value'])
  
  test_result <- c(test_data$overall[c('Accuracy')],
                   test_data$byClass[c('Sensitivity', 
                                      'Specificity', 
                                      'Pos Pred Value', 
                                      'Neg Pred Value')])
  F1_test <- 2 * test_result['Sensitivity'] * test_result['Pos Pred Value'] / (test_result['Sensitivity'] + test_result['Pos Pred Value'])
  
  
  res <- c(train_result, F1 = F1_train, test_result, F1 = F1_test )
  return(res)
}
