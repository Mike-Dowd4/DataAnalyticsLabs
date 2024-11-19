
setwd("C:/Users/Mike/Documents/RPI/Fall 2024/DataAnalyticsLabs/Data Analytics F24 - Lab 01")

EPI_data_csv <- read.csv("epi2024results06022024.csv", header = TRUE)

attach(EPI_data_csv)

View(EPI_data_csv)

tf <- is.na(EPI.new) #records True values if value is NA

E <- EPI.new[!tf] #filters out NA values, new array

summary(EPI.new) #stats
fivenum(EPI.new, na.rm=TRUE)
stem(EPI.new)#stem and leaf plot
hist(EPI.new)
hist(EPI.new, seq(20.,80.,1.0), prob=TRUE)
lines(density(EPI.new, na.rm=TRUE, bw=1.))
rug(EPI.new)


#comparing distributions
label = c("EPI", "APO")
boxplot(EPI.new, APO.new, names=label)


#more histograms
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new, na.rm=TRUE, bw=1.))
rug(EPI.new)

hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new, na.rm=TRUE, bw="SJ"))
rug(EPI.new)

x <- seq(20,80,1)
q <- dnorm(x, mean=42, sd=5, log=FALSE)
lines(x,q)
lines(x, .4*q)
ln <- dnorm(xn, mean=65, sd=5, log=FALSE)
lines(x, .12*q)


# Exercise 2
#Cumulative density function
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE)

#quantile-quantile
qqnorm(EPI.new)
qqline(EPI.new)

##Q-Q plot against norm dst
qqplot(rnorm(ppoints(250)), EPI.new, xlab = "Q-Q plot for norm dsn")
qqline(EPI.new)


#Q-Q plot against t dst
qqplot(rt(ppoints(250), df=5), EPI.new, xlab = "Q-Q plot for t dsn")
qqline(EPI.new)



#Exercise 2a - same exploration for APO
#Cumulative density function
plot(ecdf(APO.new), do.points=FALSE, verticals=TRUE)

#quantile-quantile
qqnorm(APO.new)
qqline(APO.new)

##Q-Q plot against norm dst
qqplot(rnorm(ppoints(250)), APO.new, xlab = "Q-Q plot for norm dsn")
qqline(APO.new)


#Q-Q plot against t dst
qqplot(rt(ppoints(250), df=5), APO.new, xlab = "Q-Q plot for t dsn")
qqline(APO.new)