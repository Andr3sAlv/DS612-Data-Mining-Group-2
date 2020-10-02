#install packages
library(corrplot) #if not previously installed run:   install.packages("corrplot")
library(Hmisc) #if not previously installed run:   install.packages("Hmisc")
library("stringr") #if not previously installed run:  install.packages("stringr") 

#_______________________________________________________________________________
#1.) Clean data

setwd("")
#Load data
data = read.csv("DS 612 Group Project data set.csv")

#omit na values 
data = na.omit(data)
#summarize data 
summary(data)
#data types
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

#Price
hist(data$price)
#logarithmic transformation to fix right skew in outcome variable
data['LogPrice'] <- log(data['price'])
#Insure correct transformation
hist(data$LogPrice) #normal
#Redefine quants to include LogPrice
quants <- subset(data, select =-date)

#Bedrooms
filter(data, bedrooms == 33)
  #Only 1 house with 33 bedrooms, I think we can drop this observation

#yr_renovated
hist(data$yr_renovated)
#why are there so many zero values? - these are houses that hvaen't been renovated
t <- as.data.frame(table(data$yr_renovated))
  #Don't need to drop, but this could be a categorical (renovated vs not renovated)

#Define categorical variables 
data$zipcode <- factor(data$zipcode)
  #Potentially view, condition, and grade

as.data.frame(table(data$view)) #potential categories: Popular: view >= 2
as.data.frame(table(data$condition)) #category for each condition level?
as.data.frame(table(data$grade)) #category = high if grade >= 7?

#Convert dates 
str_locate(data$date, 'T') #check to make sure location is the same for each row 
data$date <- sapply(data$date, substring, 1, 8)
data$date <- as.Date(data$date, "%Y%m%d", tz = tz)

#Distribution of all variables 
hist.data.frame(quants)

#examine relationships between quant variables
cor <- round(cor(quants), 2)
corrplot(cor, method = "number")

#Examine - what do they mean, do we drop them? Joel
#sqrft_lot(drop), sqrftlt_15(drop), sqft_living15

pairs.panels(quants) #Determine any remaining transformations of variables 

#_______________________________________________________________________________
#2.) Descriptive Statistics
#write code here 



#_______________________________________________________________________________
#3.) Model Regression problem
#write code here



#_______________________________________________________________________________
#4.) Model classification problem
#write code here



