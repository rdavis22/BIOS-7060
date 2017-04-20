library(car)
library(MASS)

# Read in the data set
Nondemo <- read.table("Lecture22_data_robust_regression.txt", header=T)
summary(Nondemo)
rownames(Nondemo) = as.character(Nondemo[[1]])
attach(Nondemo)
Nondemo.ls <- lm(secpay ~ gini)

# Least Absolute Value Regression
library(quantreg)
Nondemo.lav <- rq(secpay ~ gini,.5)
summary(Nondemo.lav)

# M-Estimation with Huber weight
Nondemo.huber <- rlm(secpay ~ gini)
summary(Nondemo.huber)
# Residual plot, showing Czech and Slovakia having large residuals
plot(Nondemo.huber$residuals)
identify(1:26, Nondemo.huber$residuals, rownames(Nondemo))
# Weight plot, showing Czech and Slovakia given the least weight
plot(Nondemo.huber$w, ylab="Huber Weight")
identify(1:26, Nondemo.huber$w, rownames(Nondemo))

# M-Estimation with bisquare weight
Nondemo.bisq <- rlm(secpay ~ gini, data=Nondemo, method='MM')
summary(Nondemo.bisq)
# Residual plot, showing Czech and Slovakia having large residuals
plot(Nondemo.bisq$residuals)
identify(1:26, Nondemo.bisq$residuals, rownames(Nondemo))
# Weight plot, showing Czech and Slovakia given the least weight
plot(Nondemo.bisq$w, ylab="Bisquare Weight")
identify(1:26, Nondemo.bisq$w, rownames(Nondemo))

# Bounded Influence Methods
# Least Trimmed Squares Regression
Nondemo.lts<-ltsreg(secpay~gini, data=Nondemo)
Nondemo.lts

# Least Median Squares Regression
Nondemo.lms<-lmsreg(secpay~gini, data=Nondemo)
Nondemo.lms

# A plot comparing the fit of the different models fit above
plot(gini, secpay)
identify(gini, secpay, row.names(Nondemo))
abline(Nondemo.ls, lwd=2, col=1, lty=1)
abline(Nondemo.lts, lwd=2, col=2, lty=2)
abline(Nondemo.bisq, lwd=2, col=3, lty=3)
abline(Nondemo.huber, lwd=2, col=4, lty=4)
abline(Nondemo.lav, lwd=2, col=5, lty=5)
legend(45, 1.6, lty=1:5, lwd=2, col=1:5,
      legend=c('OLS', 'LTS', 'M-Bisquare', 'M-Huber', 'LAV' ))
