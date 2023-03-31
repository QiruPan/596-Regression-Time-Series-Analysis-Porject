library(ggplot2)
library(randomForest)
library(class)
library(naivebayes)
library(e1071)
library(xgboost)

rm(list=ls())

wines <- read.csv("Project/whitewines.csv")
wines <- whitewine
wines_orig <- wines

# ggplot(wines, aes(y = quality)) + geom_histogram()

winestep <- step(lm(quality ~ 1, wines), formula(lm(quality ~ ., wines)), direction = "both", k = 2, trace = TRUE)

coefs <- names(winestep$coefficients)[-1]

wines <- wines[,c(coefs, "quality")]

# wines$density <- NULL

# Boruta::Boruta(quality ~ ., wines)

# Stepwise regression
if(FALSE)
{
  lm_step <- lm(quality ~ ., wines)
  
  summary(lm_step)
  
  ggplot(data.frame(Resid = lm_step$residuals, Fitted = lm_step$fitted.values), aes(x = Fitted, y = Resid)) +
    geom_point()
}


# RF Take 1
if(FALSE)
{
  wines$quality <- factor(wines$quality, levels = sort(unique(wines$quality)), ordered = TRUE)
  
  set.seed(2)
  
  rf <- randomForest(quality~., data=wines)
  
  print(rf)
}

normalize_wine <- function(wines, qCol)
{
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x))) }
  
  qval <- which(names(wines) %in% qCol)
  
  wines_norm <- cbind(data.frame(sapply(wines[,-qval], normalize)), data.frame(a1 = wines[,qval]))
  
  names(wines_norm)[names(wines_norm) %in% "a1"] <- qCol
  
  wines_norm
}

# KNN Take 1
if(FALSE)
{
  qCol <- "quality"
  
  wines_norm <- normalize_wine(wine, qCol)
  
  qval <- which(names(wines) %in% qCol)
  
  train_samples <- sample(1:nrow(wines_norm), size=nrow(wines_norm)*0.7, replace = FALSE)
  
  train <- wines_norm[train_samples, -qval]
  train_labels <- wines_norm[train_samples, qval]
  
  test <- wines_norm[-train_samples, -qval]
  test_labels <- wines_norm[-train_samples, qval]
  
  fits <- knn(train=train, test=test, cl=train_labels, k=200)
  
  table(fits, test_labels)
}

# RF Take 2
if(FALSE)
{
  wines$quality2 <- cut(as.numeric(as.character(wines$quality)), breaks = c(0, 5, 6, 9), labels = c("low", "medium", "high"))
  
  table(wines$quality, wines$quality2)
  
  quality <- wines$quality
  
  wines$quality <- NULL
  
  # wines[,qval]
  
  rf <- randomForest(quality2~., data=wines)
  
  print(rf)
}


# KNN Take 2
if(FALSE)
{
  qCol <- "quality2"
  
  wines_norm <- normalize_wine(wine, qCol)
  
  qval <- which(names(wines) %in% qCol)
  
  train_samples <- sample(1:nrow(wines_norm),size=nrow(wines_norm)*0.7,replace = FALSE)
  
  train <- wines_norm[train_samples, -qval]
  train_labels <- wines_norm[train_samples, qval]
  
  test <- wines_norm[-train_samples, -qval]
  test_labels <- wines_norm[-train_samples, qval]
  
  fits <- knn(train=train, test=test, cl=train_labels, k=70)
  
  table(fits, test_labels)
  
}


# RF Deeper Dive
if(FALSE)
{
  x1 <- data.frame()
  treeCount <- 500
  
  while(treeCount<10000)
  {
    print(treeCount)
    set.seed(2)
    rf <- randomForest(quality2~., data=wines, ntree = treeCount)
    conf <- rf$confusion[,-ncol(rf$confusion)]
    
    x1 <- rbind(x1, data.frame(Trees = treeCount, OOB = 100*(1-sum(diag(conf))/sum(conf))))
    treeCount <- treeCount+500
  }
  
  ggplot(x1, aes(x = Trees, y = OOB)) + geom_point() 
  
  
  set.seed(2)
  rf <- randomForest(quality2~., data=wines, ntree = 2000)
  
  print(rf)
  
  set.seed(2)
  train_samples <- sample(1:nrow(wines),size=nrow(wines)*0.7,replace = FALSE)
  
  qval <- which(names(wines) %in% "quality2")
  
  train <- wines[train_samples,]
  
  test <- wines[-train_samples, -qval]
  test_labels <- wines[-train_samples, qval]
  
  rf <- randomForest(quality2~., data=train, ntree = 2000)
  
  fits <- predict(rf, test)
  
  table(fits, test_labels)
  
  sum(diag(table(fits, test_labels)))/sum(table(fits, test_labels))
  
  wines_2 <- wines
  
  wines <- wines[!(wines$quality2 %in% "medium"),]
  wines$quality2 <- factor(wines$quality2)
  
  set.seed(2)
  train_samples <- sample(1:nrow(wines),size=nrow(wines)*0.7,replace = FALSE)
  
  qval <- which(names(wines) %in% "quality2")
  
  train <- wines[train_samples,]
  
  test <- wines[-train_samples, -qval]
  test_labels <- wines[-train_samples, qval]
  
  rf <- randomForest(quality2~., data=train, ntree = 2000, importance = TRUE)
  
  fits <- predict(rf, test)
  
  table(fits, test_labels)
  
  sum(diag(table(fits, test_labels)))/sum(table(fits, test_labels))
  
  test2 <- wines_2[wines_2$quality2 %in% "medium",-qval]
  
  fits2 <- predict(rf, test2)
  table(fits2, wines_2[wines_2$quality2 %in% "medium", qval])
}

# Naive Bayes
if(FALSE)
{
  set.seed(2)
  
  train_samples <- sample(1:nrow(wines),size=nrow(wines)*0.7,replace = FALSE)
  
  qval <- which(names(wines) %in% "quality2")
  
  train <- wines[train_samples,]
  
  test <- wines[-train_samples, -qval]
  test_labels <- wines[-train_samples, qval]
  
  nb <- naive_bayes(quality2 ~ ., data = train, usekernel = TRUE)
  
  fits <- predict(nb, test)
  
  table(fits, test_labels)
  
  sum(diag(table(fits, test_labels)))/sum(table(fits, test_labels))
  
  test2 <- wines_2[wines_2$quality2 %in% "medium",-qval]
  
  fits2 <- predict(nb, test2)
  table(fits2, wines_2[wines_2$quality2 %in% "medium", qval])
}

# SVM
if(FALSE)
{
  qCol <- "quality2"
  
  wines_norm <- normalize_wine(wine, qCol)
  
  qval <- which(names(wines) %in% qCol)
  
  train_samples <- sample(1:nrow(wines_norm),size=nrow(wines_norm)*0.7,replace = FALSE)
  
  train <- wines_norm[train_samples, ]
  train_labels <- wines_norm[train_samples, qval]
  
  test <- wines_norm[-train_samples, ]
  test_labels <- wines_norm[-train_samples, qval]
  
  svm_wine <- svm(quality2 ~ ., data = train)
  
  fits <- predict(svm_wine, test)
  
  table(fits, test_labels)
  
  sum(diag(table(fits, test_labels)))/sum(table(fits, test_labels))
  
  test2 <- wines_2[wines_2$quality2 %in% "medium",-qval]
  
  fits2 <- predict(svm_wine, test2)
  table(fits2, wines_2[wines_2$quality2 %in% "medium", qval])
  
  
  
  wines_norm <- normalize_wine(wines_2, qCol)
  
  wines_norm$quality2[wines_norm$quality2 %in% "medium"] <- "low"
  
  train_samples <- sample(1:nrow(wines_norm),size=nrow(wines_norm)*0.7,replace = FALSE)
  
  train <- wines_norm[train_samples, ]
  train_labels <- wines_norm[train_samples, qval]
  
  test <- wines_norm[-train_samples, ]
  test_labels <- wines_norm[-train_samples, qval]
  
  svm_wine <- svm(quality2 ~ ., data = train)
  
  fits <- predict(svm_wine, test)
  
  table(fits, test_labels)
  
  sum(diag(table(fits, test_labels)))/sum(table(fits, test_labels))
}

# XGBoost
if(FALSE)
{
  set.seed(2)
  
  train_samples <- sample(1:nrow(wines),size=nrow(wines)*0.7,replace = FALSE)
  
  qval <- which(names(wines) %in% "quality2")
  
  train <- wines[train_samples,-qval]
  train_labels <- as.numeric(wines_norm[train_samples, qval])-1
  
  test <- wines[-train_samples, -qval]
  test_labels <- as.numeric(wines[-train_samples, qval])-1
  
  xgb_train <- xgb.DMatrix(data = as.matrix(train), label = train_labels)
  xgb_test <- xgb.DMatrix(data = as.matrix(test), label = test_labels)
  
  xgb_params <- list(
    booster = "gbtree",
    eta = 0.3,
    max_depth = 8,
    gamma = 4,
    subsample = 0.75,
    colsample_bytree = 1,
    objective = "multi:softprob",
    eval_metric = "mlogloss",
    num_class = 3
  )
  
  xgb <- xgb.train(data = xgb_train, nrounds = 5000, verbose = 0, params = xgb_params)
  
  xgb_preds <- as.data.frame(predict(xgb, xgb_test, reshape = TRUE))
  
  fits <- apply(xgb_preds, 1, function(x) {
    y <- which.max(x)
    ifelse(x[y]>0, colnames(xgb_preds)[y], 0)
    
  })
  
  table(fits, test_labels)
  
  sum(diag(table(fits, test_labels)))/sum(table(fits, test_labels))
  
  fits2 <- fits[!(fits %in% 0)]
  test_labels2 <- test_labels[!(fits %in% 0)]
  
  sum(diag(table(fits2, test_labels2)))/sum(table(fits2, test_labels2))
  
  
  test2 <- wines_2[wines_2$quality2 %in% "medium",-qval]
  test_labels2 <- as.numeric(wines_2[wines_2$quality2 %in% "medium", qval])-1
  
  xgb_test2 <- xgb.DMatrix(data = as.matrix(test2), label = test_labels2)
  
  xgb_preds2 <- as.data.frame(predict(xgb, xgb_test2, reshape = TRUE))
  
  fits2 <- apply(xgb_preds2, 1, function(x) {
    y <- which.max(x)
    ifelse(x[y]>0, colnames(xgb_preds)[y], 0)
    
  })
  
  table(fits2, wines_2[wines_2$quality2 %in% "medium", qval])
}


# Logistic Regression
if(FALSE)
{
  wines_3 <- wines_2
  
  wines_3$quality2[wines_3$quality2 %in% "medium"] <- "low"
  
  wines_3$quality2 <- ifelse(wines_3$quality2 %in% "low", 0, 1)
  
  train_samples <- sample(1:nrow(wines_3),size=nrow(wines_3)*0.7,replace = FALSE)
  
  train <- wines_3[train_samples, ]
  
  test <- wines_3[-train_samples, -qval]
  test_labels <- wines_3[-train_samples, qval]
  
  logreg <- glm(quality2 ~ ., train, family = "binomial")
  
  fits <- predict(logreg, test, type = "response")
  
  fits <- sapply(fits, function(x) ifelse(x>0.5, 1, 0))
  
  table(fits, test_labels)
  
  sum(diag(table(fits, test_labels)))/sum(table(fits, test_labels))
}