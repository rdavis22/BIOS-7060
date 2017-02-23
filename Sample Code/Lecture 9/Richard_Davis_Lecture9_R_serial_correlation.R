# Example codes for Durbin-Watson test and runs test

# Read in the air quality data set
airquality  <- read.table("Lecture10_data_serial_correlation.txt", header=T)

# Fit a regular regression model
g <- lm(Ozone ~ Solar.R + Wind + Temp,airquality)
summary(g)

par(mfrow=c(2,2), mar=c(4,4,3,1))
# Ozone data were log transformed
gl <- lm(log(Ozone) ~ Solar.R + Wind + Temp, airquality)
# Plot of residuals vs fitted values
plot(gl$fit, gl$res, xlab="Fitted", ylab="Residuals", main="Logged Response")

# Remove missing data
res <- rep(NA,153)
res[as.numeric(row.names(na.omit(airquality)))] <- gl$res

# Plot of residuals vs index
plot(res, ylab="Residuals", main="Index plot of residuals")
# Plot of residuals vs residuals right before them
plot(res[-153], res[-1], xlab=expression(hat(epsilon)[i]), 
	ylab=expression(hat(epsilon)[i+1]))

# Durbin-Watson Test from "car" package
library(car)
durbinWatsonTest(gl)

# Runs test from "tseries" package
library(tseries)
xx = factor(sign(gl$res))
runs.test(xx)

