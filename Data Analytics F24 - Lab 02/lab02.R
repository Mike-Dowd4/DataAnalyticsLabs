setwd("C:/Users/Mike/Documents/RPI/Fall 2024/DataAnalyticsLabs/Data Analytics F24 - Lab 02")

populations_2023 <- read.csv("countries_populations_2023.csv", header = TRUE)
epi_results <- read.csv("epi2024results06022024.csv", header = TRUE)
epi_weights <- read.csv("epi2024weights.csv", header = TRUE)

attach(epi_weights)

qqnorm(EPI.new)
qqline(EPI.new)

# Q-Q plot against the generating distribution
x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)
qqplot(qnorm(ppoints(200)), EPI.new)
qqline(EPI.new)

#Cumulative Density Function
plot(ecdf(EPI.new), do.points=FALSE)
plot(ecdf(rnorm(1000,45,10)), do.points=FALSE) # ecdf of normal distr with mean=45, sd= 10
lines(ecdf(EPI.new))


#same exploration with ECO and SPI
qqnorm(ECO.new)
qqline(ECO.new)

qqnorm(SPI.new)
qqline(SPI.new)

# Q-Q plot against the generating distribution ECO and SPI
qqplot(qnorm(ppoints(200)), ECO.new)
qqline(ECO.new)

qqplot(qnorm(ppoints(200)), SPI.new)
qqline(SPI.new)

#Cumulative Density Function with ECO and SPI
plot(ecdf(ECO.new), do.points=FALSE)
plot(ecdf(rnorm(1000,45,10)), do.points=FALSE) # ecdf of normal distr with mean=45, sd= 10
lines(ecdf(ECO.new))

plot(ecdf(SPI.new), do.points=FALSE)
plot(ecdf(rnorm(1000,45,10)), do.points=FALSE) # ecdf of normal distr with mean=45, sd= 10
lines(ecdf(SPI.new))

#tried against qchisq but idk if right
plot(ecdf(SPI.new), do.points=FALSE)
plot(ecdf(qchisq(qnorm(ppoints(200)), df=3)), do.points=FALSE) # ecdf of normal distr with mean=45, sd= 10
lines(ecdf(SPI.new))


#Comparing Distributions

boxplot(EPI.old, EPI.new, names=c("EPI.old", "EPI.new"))


plot(ecdf(EPI.old), do.points=FALSE, main="EPI.old vs. EPI.new ECDF")
lines(ecdf(EPI.new))



#integrating datasets for linear regression

#drop countries not in epi results
populations <- populations_2023[-which(!populations_2023$Country %in% epi_results$country),]

#sort populations by country
populations <- populations[order(populations$Country),]

#drop countries not in populations
epi_results.sub <- epi_results[-which(!epi_results$country %in% populations$Country),]

#sort epi results by country
epi_results.sub <- epi_results.sub[order(epi_results.sub$country),]

#only keep necessary columns
epi_results.sub <- epi_results.sub[, c("country", "EPI.old", "EPI.new")]

#convert population to numberic
epi_results.sub$population <- as.numeric(populations$Population)

#compute population log base 10
epi_results.sub$population_log <- log10(epi_results.sub$population)


#Linear Model in R
