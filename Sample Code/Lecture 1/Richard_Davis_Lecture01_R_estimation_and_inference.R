####Example codes for running a linear regression analysis####
##libraries
if(!require(tidyverse))
  install.packages(tidyverse)


# Read in the country-savings data set
savings  <- read.table("Lecture01_data_estimation_and_inference.txt", header=T)

# Summary statistics of the data
summary(savings)

# Two plots side by side: histogram and kernel density for personal savings
#GGplot
p1<-ggplot(data=savings, aes(savings$sr))+
  geom_histogram()
p2<-ggplot(data=savings, aes(savings$sr))+
  geom_density()
# hist(savings$sr) # Histogram
# plot(density(savings$sr)) # Kernel density plot 

# Put kernel density on the histogram
hist(savings$sr, prob=T) # Histogram
lines(density(savings$sr), col="red") # Kernel density plot 

# Plot personal savings (sr) vs percentage population under 15 yrs (pop15)
plot(x=savings$pop15, y=savings$sr, cex=1.5, pch=16, col="red", cex.lab=2,
	cex.axis=1.5, xlab="Percentage population <15 yrs",
	ylab="Personal savings")

# simple linear regression: y - personal savings, x - pop15
g1 <- lm(sr~pop15, data=savings)

# trend line for the regression
abline(g1$coef, lty=2, lwd=2, col="blue")

# Summary of regression results: parameter estimation & sd, t-test for 
#   individual predictor, R2, F-test for the model
summary(g1)

# ANOVA table for regression
anova(g1)

# Residual plot
plot(x=g1$fitted.values, y=g1$residuals, cex=1.5, pch=16, col="red", cex.lab=2,
	cex.axis=1.5, xlab="Fitted values",
	ylab="Residuals")


# 95% confidence interval for parameters 
confint(g1)

# Plot the confidence intervals for the regression and the future predicted 
#   values
tt = order(savings$pop15)
conf_lim = predict(g1, interval="confidence")
pred_lim = predict(g1, interval="prediction")
matplot(savings$pop15[tt], cbind(conf_lim, pred_lim[,-1])[tt,], type="l", 
	ylab="predicted sr", xlab="Pop15", lty=c(1, 2, 2, 3, 3),
	col=c(1, 2, 2, 3, 3))

# for powerpoint files
hist(savings$sr, cex.lab=2, cex.axis=2, main="", breaks=20, xlab="Personal savings", prob=T, col="grey")
lines(density(savings$sr), col="red", lwd=2)
curve(1/sqrt(2*pi*sd0*sd0)*exp(-(x-mm)^2/2/sd0/sd0), -1, 25, add=T, col="blue", lwd=2)
pairs(~sr+pop15+pop75+dpi, data=savings, cex=2, pch=16, col=1, cex.labels=4, panel=panel.smooth, lwd=2)

# QQ plot: normal vs t
plot(xx, yy, xlab="Standard normal", ylab="t (df=1)", cex.lab=2, cex.axis=1.5, cex=2)
lines(c(-2.5,2.5), c(-2.5,2.5), lty=1, lwd=2, col="red")
plot(xx, yy2, xlab="Standard normal", ylab="t (df=5)", cex.lab=2, cex.axis=1.5, cex=2)
lines(c(-2.5,2.5), c(-2.5,2.5), lty=1, lwd=2, col="red")
plot(xx, yy3, xlab="Standard normal", ylab="t (df=10)", cex.lab=2, cex.axis=1.5, cex=2)
lines(c(-2.5,2.5), c(-2.5,2.5), lty=1, lwd=2, col="red")
plot(xx, yy4, xlab="Standard normal", ylab="t (df=50)", cex.lab=2, cex.axis=1.5, cex=2)
lines(c(-2.5,2.5), c(-2.5,2.5), lty=1, lwd=2, col="red")

