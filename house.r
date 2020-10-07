rm(list=ls()) 
graphics.off()
house <- read.csv ("C:/Users/Vivinne/Desktop/Test/kc_house_data.csv")
plot(house$price,house$squft)
install.packages
library(corrplot)
library(MASS)
library(ISLR)
library(corrplot)
library(Hmis)
data=na.omit(house)
head(house)
data.frame(colnames(house))
quants <- subset(house, select = -date)
quals <- data$date
multi_func <- function(x){
c(min=min(x),mean=mean(x),max=max(x),std_dev=sd(x))  
}
sapply(quants,multi_func)
house$zipcode<-factor(house$zipcode)
hist(x)
initial <- read.table(house,nrows=100)
Classes <- sapply(initial, class)
tabALL <- read.table("kc_house_data.csv",colClasses = Classes)
cor <- round(cor(quant),2)
corrplot(cor,method="number")
aggregate(house$price~house$waterfront,FUN = mean)
set.seed(100) #set seed
x <- rnorm(500) #predictor
e <- rnorm(500,0,2)
y <- 1 + 2*x + e
summary(y)
plot(x,y)
aggregate(house$price~house$bedrooms,FUN = mean)
set.seed(100) #set seed
x <- rnorm(500) #predictor
e <- rnorm(500,0,2)
y <- 10 + 2*x + e
summary(y)
plot(x,y)
hist(data)
boxplot(data$price, ylab="price")
hist(data)
boxplot(data$price, ylab="price")
hist(data$price, main = "Histogram of Price", breaks = sqrt(nrow(data)))