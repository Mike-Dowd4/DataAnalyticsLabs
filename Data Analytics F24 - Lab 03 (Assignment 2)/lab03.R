EPI <- read.csv("~/RPI/Fall 2024/DataAnalyticsLabs/Data Analytics F24 - Lab 03 (Assignment 2)/epi2024results_DA_F24_lab03.csv")

attach(EPI)

epi.southern_asia <- EPI[which(EPI$region == "Southern Asia"), ]

epi.eastern_europe <- EPI[which(EPI$region == 'Eastern Europe'), ]

#histogram for southern asia MHP
hist(epi.southern_asia$MHP, prob=TRUE, ylim=c(0,0.1))

lines(density(epi.southern_asia$MHP,na.rm=TRUE,bw=1))
lines(density(epi.southern_asia$MHP,na.rm=TRUE,bw="SJ"))


#histogram for eastern europe MHP
hist(epi.eastern_europe$MHP, prob=TRUE, ylim=c(0,0.1))

lines(density(epi.eastern_europe$MHP,na.rm=TRUE,bw=1))
lines(density(epi.eastern_europe$MHP,na.rm=TRUE,bw="SJ"))


plot(qqnorm(epi.eastern_europe$MHP))
qqline(epi.eastern_europe$MHP)

plot(qqplot(epi.eastern_europe$MHP, qchisq(ppoints(length(epi.eastern_europe$MHP)), df=3)))

plot(qqnorm(epi.southern_asia$MHP))
qqline(epi.southern_asia$MHP)




#---------------------Linear Models---------------------------
attach(EPI)
data <- EPI[, c('ECO', 'BDH', 'PAR', 'SPI', 'TBN')]
#Linear Model in R
lin.mod.epi <- lm(EPI$EPI~ECO+BDH+PAR+SPI+TBN, data)
summary(lin.mod.epi)


##ECO has the largest coefficient and lowest P-value, influences EPI the most
plot(ECO, EPI$EPI, main = "ECO vs EPI")
abline(lin.mod.epi$coefficients['(Intercept)'], lin.mod.epi$coefficients['ECO'])



#repeat previous model with subset of 1 region
attach(epi.eastern_europe)

data <- epi.eastern_europe[, c('ECO', 'BDH', 'PAR', 'SPI', 'TBN')]
#Linear Model in R
lin.mod.epi.eastern_europe <- lm(epi.eastern_europe$EPI~ECO+BDH+PAR+SPI+TBN, data)
summary(lin.mod.epi.eastern_europe)


##ECO has the largest coefficient and lowest P-value, influences EPI the most
##ECO p-value is only 0.273, which is not even close to significant enough to
##rule out that there's no association(that the coeff is zero)

plot(ECO, epi.eastern_europe$EPI, main = "ECO vs EPI for eastern europe")
abline(lin.mod.epi.eastern_europe$coefficients['(Intercept)'], lin.mod.epi.eastern_europe$coefficients['ECO'])



##--------------------------Classification(KNN)--------------------------
library(class)

#Get 3 regions and choose 5 variables
kNNData = EPI[which(EPI$region %in% c('Eastern Europe', 'Greater Middle East', 'Asia-Pacific')), ]
kNNData = kNNData[, c('EPI', 'ECO', 'BDH', 'TBN', 'TKP', 'region')]

n = nrow(kNNData)
train.indices = sample(n, n*0.7)

kNN.training = kNNData[train.indices, ]
kNN.test = kNNData[-train.indices, ]
kNN.training[-6]
k = ceiling(sqrt(n))
kNNpred = knn(kNN.training[,-6], kNN.test[-6], kNN.training[,'region'] )

kNNpred

contingency.table <- table(Predicted = kNNpred, Actual=kNN.test[,'region'])

print(contingency.table)

contingency.matrix = as.matrix(contingency.table)
accuracy_1 = sum(diag(contingency.matrix))/(n-length(train.indices))
##accuracy = 0.66666666667


##Try with 3 different regions
kNNData = EPI[which(EPI$region %in% c('Global West', 'Former Soviet States', 'Latin America & Caribbean')), ]
kNNData = kNNData[, c('EPI', 'ECO', 'BDH', 'TBN', 'TKP', 'region')]

n = nrow(kNNData)
train.indices = sample(n, n*0.7)

kNN.training = kNNData[train.indices, ]
kNN.test = kNNData[-train.indices, ]


##--------------------Clustering--------------------------------



k = ceiling(sqrt(n))
kNNpred = knn(kNN.training[,-6], kNN.test[-6], kNN.training[,'region'] )

contingency.table <- table(Predicted = kNNpred, Actual=kNN.test[,'region'])

print(contingency.table)

contingency.matrix = as.matrix(contingency.table)
accuracy_2 = sum(diag(contingency.matrix))/(n-length(train.indices))
##accuracy = 0.8