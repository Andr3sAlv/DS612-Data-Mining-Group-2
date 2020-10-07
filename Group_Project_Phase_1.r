#install packages

install.packages("corrplot")
install.packages("Hmisc")
install.packages("stringr")
install.packages("dplyr")


library(corrplot) #if not previously installed run:   install.packages("corrplot")
library(Hmisc) #if not previously installed run:   install.packages("Hmisc")
library("stringr") #if not previously installed run:  install.packages("stringr") 
library(dplyr)


#_______________________________________________________________________________
#1.) Clean data

setwd("")
#Load data
data = read.csv ("C:/users/Vivinne/Desktop/Test/clean_data.csv")

#omit na values 
data = na.omit(data)
#summarize data 
summary(data)
#data types
str(data)

head(data)
data.frame(colnames(data))
#remove unnecessary columns
data <- data[-c(1,3, 18, 19,20)]


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
#2.) Descriptive Statistics: V
#Write code in seperate file, make pull request to upload to repo
#Feel free to use these to start
#These plots focus on the variables with highest correlation


#summary of all data
summary(data)

#Frequency Table for Grade

t <- as.data.frame(table(data$grade))
names(t)[1]='grade'
t

#Frequency Table for sqft_living
u <- as.data.frame(table(data$sqft_living))
names(u)[1]='Square Foot of Living'
u

#Frequency Table for sqft_above
v <- as.data.frame(table(data$sqft_above))
names(v)[1]='Square Foot above'
v

#Frequency Table for Price
w <- as.data.frame(table(data$price))
names(w)[1]='Price'
w



#correlation matrix of cleaned data

corr <- round(cor(data),2)
corrplot (corr,method ="circle")


#Average price per grade class 

a <- aggregate(data$price ~ data$grade, FUN = mean)
names(a)[1] = "Grade"
names(a)[2] = "Avg Price"
a

#Average price per sqft_living class 
b <- aggregate(data$price ~ data$sqft_living, FUN = mean)
names(b)[1] = "Square Foot of Living"
names(b)[2] = "Avg Price"
b

#Average price per sqft_above class 
a <- aggregate(data$price ~ data$sqft_above, FUN = mean)
names(a)[1] = "Sqft Above"
names(a)[2] = "Avg Price"
a

#Some scatter plots 
#Sqft_living vs price 
plot(data$grade,data$price)
plot(data$sqft_living, data$price)
plot(data$sqft_above, data$price)




#Histogram 
hist(data$grade, 
     main = "Histogram of Housing Grade",
     xlab = "Housing Grade",
     ylab = "Frequency")

hist(data$sqft_living, 
     main = "Histogram of Sqft_Living",
     xlab = "Housing Sqft_Living",
     ylab = "Frequency")

hist(data$sqft_above, 
     main = "Histogram of Sqft_Above",
     xlab = "Housing Sqft_Above",
     ylab = "Frequency")

#barplot
barplot(table(data$grade),
        main = "Bar Chart of Housing Grade",
        xlab = "Housing Grade",
        ylab = "Frequency")

barplot(table(data$sqft_living),
        main = "Bar Chart of sqft_living",
        xlab = "sqft_living",
        ylab = "Frequency")

barplot(table(data$sqft_above),
        main = "Bar Chart of sqft_above",
        xlab = "sqft_above",
        ylab = "Frequency")

barplot(table(data$price),
        main = "Bar Chart of price",
        xlab = "Price",
        ylab = "Frequency")

#Box plot
plot(factor(data$grade),data$price,
     main = "Box Plot of Price vs. Grade",
     xlab = "Grade",
     ylab = "Price")

plot(factor(data$sqft_living),data$price,
     main = "Box Plot of Price vs. sqft_living",
     xlab = "sqft_living",
     ylab = "Price")

plot(factor(data$sqft_above),data$price,
     main = "Box Plot of Price vs. sqft_above",
     xlab = "sqft_above",
     ylab = "Price")

#_______________________________________________________________________________
#3.) Model Regression problem: Jimmy and Tatiana
#make sure to use log(price) not price
#write code in separate file, make pull request to upload to repo
#Correlation matrix might be helpful if using forward selection



#_______________________________________________________________________________
#4.) Model classification problem Seth, etc. : 
#write in seperate file, make pull request to upload to repo

#R-markdown: V, Tatina, and anyone else 


