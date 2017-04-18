# Read in the data set
longley  <- read.table("Lecture21_data_ridge_PC_regressions.txt", header=T)

# Ridge regression
library(MASS)
gr <- lm.ridge(Employed ~., longley,lambda = seq(0,0.1,0.001))

# Plot of the coefficients vs lambda
matplot(gr$lambda, t(gr$coef), type="l", xlab=expression(lambda),
ylab = expression(hat(beta)))
abline(h=0,lwd=2)

# Select the lambda value, based on various methods
select(gr)


# Principal components regression
# Eigenvalues and eigenvectors at the original scale
x <- as.matrix(longley[,-7])
e <- eigen(t(x) %*% x)
round(e$vec, 3)

# Eigenvalues/eigenvectors for the correlation matrix
e <- eigen(cor(x))
round(e$vec, 3)

# Elbow plot
plot(e$val, type="l")

# Principal components for scaled x
nx <- scale(x)
enx <- nx %*% e$vec

# PC regression using all PCs
g1 <- lm(longley$Employed ~ enx)
summary(g1)

# PC regression using the first two PCs
g2 <- lm(longley$Employed ~ enx[, 1] + enx[,2])
summary(g2)

