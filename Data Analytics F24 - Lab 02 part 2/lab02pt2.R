setwd('C:/Users/Mike/Documents/RPI/Fall 2024/DataAnalyticsLabs/Data Analytics F24 - Lab 02 part 2')

abalone = read.csv("abalone/abalone.data")

#set the colnames of abalone with cols from abalone.names
names(abalone) = c("Sex", "Length", "Diameter", "Height", "Whole weight", "Shucked weight", "Viscera weight", "Shell weight", "Rings")

abalone$age.group <- cut(abalone$Rings, br=c(0,8,11,35), labels = c('young', 'adult', 'old'))

#for NaiveBayes classifier
library("e1071")

# ---------------------- EXERCISE 1 ---------------------------------
#make classifier with 3 different subsets of features

#2:4 = Length, Diameter, Height
#5:8 = Whole weight, Shucked weight, Viscera weight, Shell weight
#2,5,8 = Length, Whole weight, Shell weight
#Predicting abalone[10] = age.group

classifier_1 <- naiveBayes(abalone[2:4], abalone[10])
classifier_2 <- naiveBayes(abalone[5:8], abalone[10])
classifier_3 <- naiveBayes(abalone[c(2,5,8)], abalone[10])


table(predict(classifier_1, abalone[2:4]), abalone[,10], dnn=list('predicted', 'actual'))
table(predict(classifier_2, abalone[5:8]), abalone[,10], dnn=list('predicted', 'actual'))
table(predict(classifier_3, abalone[c(2,5,8)]), abalone[,10], dnn=list('predicted', 'actual'))

classifier_1$apriori
classifier_1$tables$Length[,1]['young']

#plot against length
stats_1 <- classifier_1$tables$Length
plot(function(x) dnorm(x, stats_1[,1]['young'], stats_1[,2]['young']), 0, 1.5, col="red", main="Length distribution for the 3 different ages") 

curve(dnorm(x, stats_1[,1]['adult'], stats_1[,2]['adult']), add=TRUE, col="blue") 
curve(dnorm(x, stats_1[,1]['old'], stats_1[,2]['old']), add=TRUE, col="green") 


#plot against Whole weight
stats_2 <- classifier_2$tables$`Whole weight`

plot(function(x) dnorm(x, stats_2[,1]['young'], stats_2[,2]['young']), 0, 3, col="red", main="Whole weight distribution for the 3 different ages") 

curve(dnorm(x, stats_2[,1]['adult'], stats_2[,2]['adult']), add=TRUE, col="blue") 
curve(dnorm(x, stats_2[,1]['old'], stats_2[,2]['old']), add=TRUE, col="green") 


#plot against shell weight for classifier 3

stats_3 <- classifier_3$tables$`Shell weight`

plot(function(x) dnorm(x, stats_3[,1]['young'], stats_3[,2]['young']), 0, 1, col="red", main="Shell weight distribution for the 3 different ages") 

curve(dnorm(x, stats_3[,1]['adult'], stats_3[,2]['adult']), add=TRUE, col="blue") 
curve(dnorm(x, stats_3[,1]['old'], stats_3[,2]['old']), add=TRUE, col="green") 



#------------------------------EXERCISE 2-----------------------------

#normalize iris for knn
iris.norm <- iris[,]
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }
iris.norm[1:4] <- as.data.frame(lapply(iris.norm[1:4], normalize))


training_size = ceiling(0.7*nrow(iris.norm))
testing_size = nrow(iris.norm) - training_size

s_iris = sample(nrow(iris), training_size)

iris.train <-iris.norm[s_iris,]
iris.test <-iris.norm[-s_iris,]


k <- ceiling(sqrt(training_size))
library(class)
KNNpred <- knn(train = iris.train[1:2], test = iris.test[1:2], cl = iris.train[,5], k = k)
contingency.table <- table(KNNpred,iris.test[,5])

contingency.matrix = as.matrix(contingency.table)
contingency.matrix

sum(diag(contingency.matrix))/length(iris.test[,5])
accuracy <- c()
ks <- c(7,8,9,10,11,12,13,14,15)
for (k in ks) {
  KNNpred <- knn(train = iris.train[1:2], test = iris.test[1:2], cl = iris.train$Species, k = k)
  cm = as.matrix(table(Actual=KNNpred, Predicted = iris.test$Species, dnn=list('predicted','actual')))
  accuracy <- c(accuracy,sum(diag(cm))/length(iris.test$Species))
}
plot(ks,accuracy,type = "b", ylim = c(0.60,1.0), main="k value vs acc for sepal.length and sepal.width")


## Same analysis with features 3:4 = Petal.length and Petal.width

#new sample
s_iris = sample(nrow(iris), training_size)

iris.train <-iris.norm[s_iris,]
iris.test <-iris.norm[-s_iris,]

KNNpred <- knn(train = iris.train[3:4], test = iris.test[3:4], cl = iris.train[,5], k = k)
contingency.table <- table(KNNpred,iris.test[,5])

contingency.matrix = as.matrix(contingency.table)
contingency.matrix

sum(diag(contingency.matrix))/length(iris.test[,5])
accuracy <- c()
ks <- c(1,2,3,4,5,6,7,8,9,10,11)
for (k in ks) {
  KNNpred <- knn(train = iris.train[3:4], test = iris.test[3:4], cl = iris.train$Species, k = k)
  cm = as.matrix(table(Actual=KNNpred, Predicted = iris.test$Species, dnn=list('predicted','actual')))
  accuracy <- c(accuracy,sum(diag(cm))/length(iris.test$Species))
}
plot(ks,accuracy,type = "b", ylim = c(0.95,1.0), main="k value vs acc for sepal.length and sepal.width")

library(tidyverse)
# Plot iris petal length vs. petal width, color by species
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point()


# set seed for random number generator
set.seed(123)
# run k-means
iris.km <- kmeans(iris[,-5], centers = 3)
assigned.clusters <- as.factor(iris.km$cluster)
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = assigned.clusters)) +
  geom_point()
    

wss <- c()
ks <- c(2,3,4,5,6,7,8,9,10,11,12,13)
for (k in ks) {
  iris.km <- kmeans(iris[,-5], centers = k)
  wss <- c(wss,iris.km$tot.withinss)
}
plot(ks,wss,type = "b", main = "wss vs k-value for kmeans iris")


labeled.clusters <- as.character(assigned.clusters)
labeled.clusters[labeled.clusters==1] <- "setosa"
labeled.clusters[labeled.clusters==2] <- "versivolor"
labeled.clusters[labeled.clusters==3] <- "virginica"
table(labeled.clusters, iris[,5])

#k=12 seems to work well, run k-means with k=12

# set seed for random number generator
set.seed(123)
# run k-means
iris.km <- kmeans(iris[,-5], centers = 12)
assigned.clusters <- as.factor(iris.km$cluster)
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = assigned.clusters)) +
  geom_point()


#plot actual clusters
ggplot(abalone, aes(x = Length, y = Diameter, colour = age.group)) +
  geom_point()

# set seed for random number generator
set.seed(123)
# run k-means
abalone.km <- kmeans(abalone[,2:9], centers = 3)
assigned.clusters <- as.factor(abalone.km$cluster)
ggplot(abalone, aes(x = Length, y = Diameter, colour = assigned.clusters)) +
  geom_point()


wss <- c()
ks <- c(2,3,4,5,6,7,8,9,10,11,12,13)
for (k in ks) {
  iris.km <- kmeans(abalone[, 2:9], centers = k)
  wss <- c(wss,abalone.km$tot.withinss)
}
plot(ks,wss,type = "b", main = "wss vs k-value for kmeans abalone")


labeled.clusters <- as.character(assigned.clusters)
labeled.clusters[labeled.clusters==1] <- "young"
labeled.clusters[labeled.clusters==2] <- "adult"
labeled.clusters[labeled.clusters==3] <- "old"
table(labeled.clusters, abalone[,10])