clean_data <- read.csv("C:/Users/jimmy/DS612-Data-Mining-Group-2/DS 612 Group Project data set.csv", header = TRUE, na.string="?")

names(clean_data)

attach(clean_data)



#Compute correlation matrix
clean_quants <- subset(clean_data, select = -date)
cor <- round(cor(clean_quants), 2)
cor(clean_quants, use = "complete.obs")


#Compute correlation matrix
res2 <- rcorr(as.matrix(clean_quants))
res2


# Extract the correlation coefficients
res2$r


# Extract p-values
res2$P


# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}


flattenCorrMatrix(res2$r, res2$P)


# Insignificant correlations are leaved blank
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")


#checking price, grade, sqft_living and both of them.
fit3 <- lm(log(price)~grade, data = clean_data)
summary(fit3)


fit4 <- lm(log(price)~sqft_living, data = clean_data)
summary(fit4)



fit5 <- lm(log(price)~grade+sqft_living, data = clean_data)
summary(fit5)


#Divides the plotting region into a 2x2 grid of panels
par(mfrow=c(2,2))


#Plot the residual fitted plots
plot(fit5)


