# Read in the data set for GLS
longley <- read.table("Lecture14_data_GLS.txt", header=T)

# GLS can be conducted via "gls" function in the "nlme" package
# the option "correlation" specifies the type of correlation structure. Here "corAR1" 
# indicates a lag-1 serial correlation, and "form=~Year" indicates that the correlation
# is observed along the years
library(nlme)
g1 <- gls(Employed ~ GNP + Population, correlation=corAR1(form=~Year), data=longley)
anova(g1)

# Read in the data set for WLS
# Four variables: momentum, energy, crossx, and sd
strongx <- read.table("Lecture14_data_WLS.txt", header=T)

# WLS can be conducted using "lm" function, with an option for weight specified by the
# option "weights". E.g., weights here are the reciprocals of the variance, i.e., 1/sd^2
g2 <- lm(crossx ~ energy, strongx, weights=sd^-2)
anova(g2)

# WLS example, textbook Table 9.1
d9.1 <- read.table("Lecture14_data_WLS2.txt", header=T)
# Weights can actually be specified using given values, or by a function. In addition, 
#   weights can be estimated along with regression using "gls" via MLE method.
# Here "weights" are specified using the same function from the textbook
g3 <- lm(Y ~ X, data=d9.1, weights=1/(1.5329-0.7334*X+0.0883*X^2))
anova(g3)

# Here "weights" are estimated via "gls" function from "nlme" package
g4 <- gls(Y ~ X, data=d9.1, weights=varPower())
anova(g4)

