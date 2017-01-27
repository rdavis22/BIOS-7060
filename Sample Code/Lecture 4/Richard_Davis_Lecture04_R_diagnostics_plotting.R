# Example codes for diagnostics plotting

# Read in the country-savings data set
# 'Richard_Davis_Lecture04_data_diagnostics_plotting.txt'
savings  <- read.table(file=file.choose(), header=T)

par(mfrow=c(1,2), mar=c(5,5,1,1))
# Regression analysis: personal savings (Y) vs pop15 (X)
m01 <- lm(sr~pop15, data=savings)

# plot a histogram of the residuals
hist(m01$residuals, xlab="Residuals", main="", col="grey", cex.lab=2, cex.axis=1.5) 

# stem-and-leaf plot of the residuals
stem(m01$residuals) 

# Normal plot for the residuals
qqnorm(m01$residuals, main="", pch=16, col="red", cex=1.5, cex.lab=2, cex.axis=1.5)
abline(0,1, col="blue", lwd=2) # The 45 degree straight line (y=x)

# Shapiro-Wilk normality test
shapiro.test(m01$residuals) 