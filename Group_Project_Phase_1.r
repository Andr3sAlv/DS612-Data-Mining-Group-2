setwd("C:/Users/sethh/OneDrive/Desktop/SFSU/Fall 2020/DS 612/Project/DS612-Data-Mining-Group-2")
#Load data
data = read.csv("DS 612 Group Project data set.csv")

#install packages
library(corrplot)
library(MASS)
library(ISLR)

#omit na values 
data = na.omit(data)
#summarize data 
summary(data)
str(data)

head(data)
data.frame(colnames(data))
#remove unecessary columns
data <- data[-c(1, 18, 19)]

#Define qunat and qual variables
quants <- subset(data, select = -date)
quals <- data$date

#Examine - what do they mean, dow we drop them? Joel
#sqrft_lot(drop), sqrftlt_15(drop), sqft_living15, rate

#examine relationshps between quant variables
cor <- round(cor(quants), 2)
corrplot(cor, method = "number")

pairs(quants)
#Determine what these results mean and starting drafting models (Tatiana, Seth and V)
#Determine outliers

#drop waterfront?
#price as dep var?
#zip as predictor var

Fi<-lm(date~ all variable, data= data)



