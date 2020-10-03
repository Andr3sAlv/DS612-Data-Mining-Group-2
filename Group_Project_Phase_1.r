#install packages
library(corrplot) #if not previously installed run:   install.packages("corrplot")
library(Hmisc) #if not previously installed run:   install.packages("Hmisc")
library("stringr") #if not previously installed run:  install.packages("stringr") 
library(dplyr)

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
data['log_price'] <- log(data['price'])
#Insure correct transformation
hist(data$log_price) #normal
#Redefine quants to include LogPrice
quants <- subset(data, select =-date)

#Bedrooms
filter(data, bedrooms == 33)
  #Only 1 house with 33 bedrooms, I think we can drop this observation
which(data$bedroom == 33)
data <- data[-c(15871), ]

#yr_renovated
hist(data$yr_renovated)
#why are there so many zero values? - these are houses that haven't been renovated
t <- as.data.frame(table(data$yr_renovated))
  #Don't need to drop, but this could be a categorical (renovated vs not renovated) yes

#Define categorical variables 
data$zipcode <- factor(data$zipcode)
  #Potentially view, condition, and grade


as.data.frame(table(data$view)) #potential categories: Popular: view > 2 Yes
as.data.frame(table(data$condition)) #category for each condition level? High >= 3 YEs
as.data.frame(table(data$grade)) #category = high if grade >= 7? YEs

data["popular"] <- ifelse(data$view >2, 1, 0)
data["good_condition"] <- ifelse(data$condition >= 3, 1, 0)
data["high_grade"] <- ifelse(data$grade >= 7, 1, 0)

#Convert dates 
str_locate(data$date, 'T') #check to make sure location is the same for each row 
data$date <- sapply(data$date, substring, 1, 8)
data$date <- as.Date(data$date, "%Y%m%d", tz = tz)

#Distribution of all variables 
hist.data.frame(quants)

#examine relationships between quant variables
cor <- round(cor(quants), 2)
corrplot(cor, method = "number")

pairs.panels(quants) #Determine any remaining transformations of variables: none 

#export to csv save as clean_data
write.csv(data, file = "clean_data.csv", col.names = TRUE)

#_______________________________________________________________________________
#2.) Descriptive Statistics: JOel and V
#Write code in seperate file, make pull request to upload to repo
#Feel free to use these to start
#These plots focus on the variables with highest correlation
clean_data <- read.csv(clean_data.csv)

#summary
summary(clean_data)

#correlation matrix of cleaned data
clean_quants <- subset(clean_data, select = -date)
cor <- round(cor(), 2)
corrplot(cor, method = "number", title = "Correlation Matrix of Quantitative Variables")

#Average price per bathroom class 
a <- aggregate(clean_data$price ~ data$bathroom, FUN = mean)
names(a)[1] = "Bathrooms"
names(a)[2] = "Avg Price"
a

#Another aggregation?
#Average price per grade 
b <- aggregate(clean_data$price ~ data$grade, FUN = mean)
names(b)[1] = "Grade"
names(b)[2] = "Avg Price"
b

#Some scatter plots 
#Sqft_living vs price 
plot(clean_data$sqft_living, clean_data$price)

#Prices through time 
plot(clean_data$date, clean_data$price)

#Histogram 

#Box plot


#_______________________________________________________________________________
#3.) Model Regression problem: Jimmy and Tatiana
#make sure to use log(price) not price
#write code in separate file, make pull request to upload to repo
#Correlation matrix might be helpful if using forward selection



#_______________________________________________________________________________
#4.) Model classification problem Seth, etc. : 
#write in seperate file, make pull request to upload to repo

#R-markdown: V, Tatina, and anyone else 


