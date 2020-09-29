setwd("C:/Users/sethh/OneDrive/Desktop/SFSU/Fall 2020/DS 612/Project/DS612-Data-Mining-Group-2")
#Load data
data = read.csv("DS 612 Group Project data set.csv")

#install packages
library(corrplot) #if not previously installed run:   install.packages("corrplot")
library(Hmisc) #if not previously installed run:   install.packages("Hmisc")

#omit na values 
data = na.omit(data)
#summarize data 
summary(data)
str(data)

head(data)
data.frame(colnames(data))
#remove unnecessary columns
data <- data[-c(1, 18, 19)]


#Define quant and qual variables
quants <- subset(data, select = -date)
quals <- data$date

#Outlier detection
multi_func <- function(x){
  c(min = min(x), mean = mean(x), max = max(x), std_dev = sd(x))
}

sapply(quants, multi_func)
#Investigate further: price, bedrooms, yr_renovated

#Define categorical variables 
data$zipcode <- factor(data$zipcode)
  #Potentially view, condition, and grade

#Distribution of all variables 
hist.data.frame(quants)

#Examine - what do they mean, do we drop them? Joel
#sqrft_lot(drop), sqrftlt_15(drop), sqft_living15, rate

#examine relationships between quant variables
cor <- round(cor(quants), 2)
corrplot(cor, method = "number")

#Explore data set add as many descriptive statistics as possible 
aggregate(data$price ~ data$waterfront, FUN = mean)
#visualize ^



pairs(quants)
#Determine what these results mean and starting drafting models (Tatiana, Seth and V)
#Determine outliers -- histrogram for distribution

#drop waterfront?
#zip as predictor var

#Fi<-lm(date~ all variable, data= data)





