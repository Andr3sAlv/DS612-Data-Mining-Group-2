

#_______________________________________________________________________________
#2.) Descriptive Statistics: V
#Write code in seperate file, make pull request to upload to repo
#Feel free to use these to start
#These plots focus on the variables with highest correlation

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

