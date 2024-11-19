library("caret")
library(e1071)
library(class)


wine <- read.csv("~/RPI/Fall 2024/DataAnalyticsLabs/Data Analytics F24 - Lab 05/wine.data", header=FALSE)

names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

wine$Type <- as.factor(wine$Type)
wine <- wine[,-c(4,5,10)] #remove according to lab04
wine <- wine[, -c(2,4,8)] #remove according to lab04
train.indices <- sample(nrow(wine),0.7*nrow(wine))
wine.train <- wine[train.indices,]
wine.test <- wine[-train.indices,]


#tune model
tune_out <- 
  tune.svm(x = wine.train[,-1], y = as.factor(wine.train$Type), 
           type = "C-classification", 
           kernel = "linear", degree = 2, cost = 2^seq(-6, 4, 2), 
           gamma = seq(1/2^nrow(wine.train),1, .01), coef0 = c(0.1, 1, 10))

gamma <- tune_out[["best.parameters"]][["gamma"]]
C <- tune_out$best.parameters$cost

svm.model <- svm(Type ~ ., data = wine.train, kernel = 'linear', gamma = gamma, cost = C)

svm.model

train.pred <- predict(svm.model, wine.train)

cm = as.matrix(table(Actual = wine.train$Type, Predicted = train.pred))
cm

test.pred <- predict(svm.model, wine.test)

cm = as.matrix(table(Actual = wine.test$Type, Predicted = test.pred))

get_performance <- function(cm) {
  print(cm)
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted 
  
  recall = diag / rowsums 
  precision = diag / colsums
  f1 = 2 * precision * recall / (precision + recall) 
  
  print(data.frame(precision, recall, f1))
}

#performance of test on linear svm
get_performance(cm)

#tune polynomial model
tune_out <- 
  tune.svm(x = wine.train[,-1], y = as.factor(wine.train$Type), 
           type = "C-classification", 
           kernel = "polynomial", degree = 2, cost = 2^seq(-6, 4, 2), 
           gamma = seq(1/2^nrow(wine.train),1, .01), coef0 = c(0.1, 1, 10))

gamma <- tune_out[["best.parameters"]][["gamma"]]
C <- tune_out$best.parameters$cost

svm.model <- svm(Type ~ ., data = wine.train, kernel = 'polynomial', gamma = gamma, cost = C)

svm.model

train.pred <- predict(svm.model, wine.train)

cm = as.matrix(table(Actual = wine.train$Type, Predicted = train.pred))

get_performance(cm)

test.pred <- predict(svm.model, wine.test)

cm = as.matrix(table(Actual = wine.test$Type, Predicted = test.pred))

get_performance(cm)

k<-10
knn.pred <- knn(train = wine[,-1], test = wine[,-1], cl = wine$Type, k = k)

cm <- as.matrix(table(Actual = wine$Type, Predicted = knn.pred))
get_performance(cm)

###------------------ SVM Regression with NY House Dataset -------------

NY.House.Dataset <- read.csv("~/RPI/Fall 2024/DataAnalyticsLabs/Data Analytics F24 - Lab 05/NY-House-Dataset.csv")

