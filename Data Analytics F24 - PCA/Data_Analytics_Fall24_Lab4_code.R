##########################################
### Principal Component Analysis (PCA) ###
##########################################

library(ggfortify)
library(e1071)
library(class)
library(psych)

# PCA with iris dataset
wine <- read.csv("~/RPI/Fall 2024/Data Analytics/Data Analytics F24 - PCA/wine.data", header=FALSE)
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
head(wine)

wine$Type <- as.factor(wine$Type)

wine <- wine[,-c(4,5,10)]

pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

wine.pc <- prcomp(wine[,-1], center=TRUE, scale.=TRUE)

summary(wine.pc)
#Get loadings to see which variables contribute most
print(wine.pc$rotation[,1])
## It seems Flavanoids, Total Phenols, and Od280/Od315 of 
## diluted wines influence PC1 the most, all with correlations
## of around -0.4

plot(wine.pc)

autoplot(wine.pc, data = wine, colour = 'Type',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)


k<-10
knn.pred <- knn(train = wine[,-1], test = wine[,-1], cl = wine$Type, k = k)

test_model <- function(preds) {
  ## evaluate
  cm <- table(Predicted=preds, Actual = wine$Type, dnn=list('predicted','actual'))
  
  print(cm)
  
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted 
  
  precision = diag / colsums 
  recall = diag / rowsums 
  f1 = 2 * precision * recall / (precision + recall) 
  
  print(data.frame(recall, precision, f1))
  accuracy <- sum(diag(cm))/(nrow(wine))
  
  
  print(paste('accuracy = ',accuracy))
}

test_model(knn.pred)

first_3_PCs <- wine.pc$x[,1:3]
preds <- knn(train = first_3_PCs, test = first_3_PCs, cl = wine$Type, k = k)
test_model(preds)

data.vars.removed <- wine[,-c(1,2,4,8)]
wine.pc <- prcomp(data.vars.removed, center=TRUE, scale.=TRUE)
first_3_PCs <- wine.pc$x[,1:3]

preds <- knn(train = first_3_PCs, test = first_3_PCs, cl = wine$Type, k = k)
test_model(preds)
