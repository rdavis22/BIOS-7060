# This is the example for the Deming regression using function Deming() from
#     the package MethComp

# Need to have MethComp installed, if it is not included in your R software

library(MethComp)

# 'True' values 
M <- runif(100,0,5)

# Measurements:
x <-         M + rnorm(100)
y <- 2 + 3 * M + rnorm(100,sd=2)

# Deming regression with equal variances, variance ratio 2.
Deming(x,y)
Deming(x,y,vr=2)
Deming(x,y,boot=TRUE)
bb <- Deming(x,y,boot=TRUE,keep.boot=TRUE)
str(bb)

# Plot data with the two classical regression lines
plot(x,y)
abline(lm(y~x))
ir <- coef(lm(x~y))
abline(-ir[1]/ir[2],1/ir[2])
abline(Deming(x,y,sdr=2)[1:2],col="red")
abline(Deming(x,y,sdr=10)[1:2],col="blue")

# Comparing classical regression and "Deming extreme"
summary(lm(y~x))
Deming(x,y,vr=1000000)