# RF_performance
#' @description Perform the rfoost on the selected dataset with the targeted 
#'   outcome, including data process, encoding, hyper-parameter tuning, and final
#'   results evaluation.
#'   
#' @param X Covariates (categorical data will be one-hot encoding)
#' @param y 
RF_performance <- function(data_X, data_y, seed = 2021, 
                           imbalance_adjustment = FALSE,
                           trainIndex = NULL){
  
  X = data_X
  y = data_y
  
  # Random Control
  set.seed(seed)
  
  # Transfer the input as factor
  one_hot_index <- which(sapply(X, class) == 'character')
  if(length(one_hot_index) > 0){
    X[,one_hot_index] <- as.data.frame(lapply(X[,one_hot_index], factor))
    dummy <- dummyVars(" ~ .", data=X)
    X <- data.frame(predict(dummy, newdata = X))
  }
  y <- factor(make.names(y), levels = c('X1', 'X0'))
  
  # training and testing set based on outcome.
  if(is.null(trainIndex)){
    trainIndex <- createDataPartition(y, p = 0.7, 
                                      list = FALSE)
  }
  
  train_X <- X[trainIndex,]
  test_X <- X[-trainIndex,]
  train_y <- y[trainIndex]
  test_y <- y[-trainIndex]
  
  res = list()
  
  tune_control <- caret::trainControl(
    method = "cv", # cross-validation
    number = 5, # with n folds 
    verboseIter = FALSE, # no training log
    allowParallel = TRUE, # FALSE for reproducible results 
    savePredictions = TRUE,
    # summaryFunction = prSummary, # Model: F optimal
    summaryFunction = twoClassSummary,
    classProbs = TRUE
  )
  
  if(imbalance_adjustment) {
    tune_control$sampling <- "smote"
  }
  
  # customRF <- list(type = "Classification",
  #                  library = "randomForest",
  #                  loop = NULL)
  # 
  # customRF$parameters <- data.frame(parameter = c("mtry", "ntree"),
  #                                   class = rep("numeric", 2),
  #                                   label = c("mtry", "ntree"))
  # 
  # customRF$grid <- function(x, y, len = NULL, search = "grid") {}
  # 
  # customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs) {
  #   randomForest(x, y,
  #                mtry = param$mtry,
  #                ntree=param$ntree)
  # }
  # 
  # #Predict label
  # customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  #   predict(modelFit, newdata)
  # 
  # #Predict prob
  # customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  #   predict(modelFit, newdata, type = "prob")
  # 
  # customRF$sort <- function(x) x[order(x[,1]),]
  # customRF$levels <- function(x) x$classes
  # 
  # tunegrid <- expand.grid(.mtry=c(1:15),.ntree=c(1000,1500,2000,2500))
  
  #create tunegrid
  model_tune <- res$model_tune <- train(x = train_X, 
                              y = train_y, 
                              # metric = 'F', # Model F optimal
                              metric = 'ROC',
                              trControl = tune_control,
                              preProcess = c("BoxCox", "center", "scale", "pca"),
                              #tuneGrid=tunegrid, 
                              method = 'rf')
  
  # Prediction Performance
  train_pred <- predict(model_tune, 
                        newdata = train_X,
                        type = 'prob')[, 'X1']
  pred <- ROCR::prediction(train_pred,
                           train_y)
  roc1 <- ROCR::performance(pred, "sens", "spec")
  cp_data <- data.frame(cutpoint = roc1@alpha.values[[1]],
                        sensitivity = roc1@y.values[[1]],
                        specificity = roc1@x.values[[1]])
  qual_data <- subset(cp_data, sensitivity > 0.9)
  CP<-qual_data[which.max(qual_data$specificity + qual_data$sensitivity), 'cutpoint']
  
  qual_data2 <- subset(cp_data, sensitivity > 0.95)
  CP2<-qual_data2[which.max(qual_data2$specificity + qual_data2$sensitivity), 'cutpoint']
  
  qual_data3 <- subset(cp_data, sensitivity > 0.85)
  CP3<-qual_data3[which.max(qual_data3$specificity + qual_data3$sensitivity), 'cutpoint']
 
  test_pred <- predict(model_tune, 
                       newdata = test_X,
                       type = 'prob')[, 'X1']
  
  if(dim(table(train_pred >= CP)) > 1) {
    res$train_data_preformance <-
      caret::confusionMatrix(reference = as.factor(make.names(train_y)), 
                             data = factor(train_pred >= CP,
                                           labels = c('X0', 'X1')),
                             positive = 'X1')
  } else {
    res$train_data_preformance <-
      caret::confusionMatrix(reference = as.factor(make.names(train_y)), 
                             data = factor(train_pred >= CP,
                                           labels = c('X1')),
                             positive = 'X1')
  }
  
  if(dim(table(test_pred >= CP)) > 1) {
    res$testing_data_performance <- 
      caret::confusionMatrix(reference = as.factor(make.names(test_y)), 
                             data = factor(test_pred >= CP,
                                           labels = c('X0', 'X1')),
                             positive = 'X1')
  } else {
    
    res$testing_data_performance <- 
      caret::confusionMatrix(reference = as.factor(make.names(test_y)), 
                             data = factor(test_pred >= CP,
                                           labels = c('X1')),
                             positive = 'X1')
  }
  
  
  
  if(dim(table(train_pred >= CP2)) > 1) {
    res$train_data_preformance2 <-
      caret::confusionMatrix(reference = as.factor(make.names(train_y)), 
                             data = factor(train_pred >= CP2,
                                           labels = c('X0', 'X1')),
                             positive = 'X1')
  } else {
    res$train_data_preformance2 <-
      caret::confusionMatrix(reference = as.factor(make.names(train_y)), 
                             data = factor(train_pred >= CP2,
                                           labels = c('X1')),
                             positive = 'X1')
  }
  
  if(dim(table(test_pred >= CP2)) > 1) {
    
    res$testing_data_performance2 <- 
      caret::confusionMatrix(reference = as.factor(make.names(test_y)), 
                             data = factor(test_pred >= CP2,
                                           labels = c('X0', 'X1')),
                             positive = 'X1')
  } else {
    
    res$testing_data_performance2 <- 
      caret::confusionMatrix(reference = as.factor(make.names(test_y)), 
                             data = factor(test_pred >= CP2,
                                           labels = c('X1')),
                             positive = 'X1')
  }
  
  if(dim(table(train_pred >= CP3)) > 1) {
    res$train_data_preformance3 <-
      caret::confusionMatrix(reference = as.factor(make.names(train_y)), 
                             data = factor(train_pred >= CP3,
                                           labels = c('X0', 'X1')),
                             positive = 'X1')
    
  } else {
    res$train_data_preformance3 <-
      caret::confusionMatrix(reference = as.factor(make.names(train_y)), 
                             data = factor(train_pred >= CP3,
                                           labels = c('X1')),
                             positive = 'X1')
    
  }
  
  if(dim(table(test_pred >= CP3)) > 1) {
    
    res$testing_data_performance3 <- 
      caret::confusionMatrix(reference = as.factor(make.names(test_y)), 
                             data = factor(test_pred >= CP3,
                                           labels = c('X0', 'X1')),
                             positive = 'X1')
  } else {
    
    res$testing_data_performance3 <- 
      caret::confusionMatrix(reference = as.factor(make.names(test_y)), 
                             data = factor(test_pred >= CP3,
                                           labels = c('X1')),
                             positive = 'X1')
  }
    
  return(res)
  
}

