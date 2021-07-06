## external partition on the selected algorithm

external_partition_ML <- function(X, y, n_fold, 
                               seed = 2021, 
                               imbalance_adjustment = FALSE,
                               model_function){
  
  set.seed(seed)
  test_index_list <- createFolds(y, k = n_fold, list = TRUE)
  index_full <- 1:nrow(X)
  train_index_list <- sapply(test_index_list, function(x) index_full[-x])
  result <- sapply(X = train_index_list, model_function, 
                   data_X = X, data_y = y, seed = seed, 
                   imbalance_adjustment = imbalance_adjustment)  
  
  return(result)
}