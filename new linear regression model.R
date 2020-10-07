#checking price, grade, sqft_living and both of them.
fit3 <- lm(log(price)~grade, data = clean_data)
summary(fit3)


fit4 <- lm(log(price)~sqft_living, data = clean_data)
summary(fit4)


fit5 <- lm(log(price)~grade+sqft_living, data = clean_data)
summary(fit5)
#The result shows statistically significant.

#Divides the plotting region into a 2x2 grid of panels
par(mfrow=c(2,2))


#Plot the residual fitted plots
plot(fit5)


