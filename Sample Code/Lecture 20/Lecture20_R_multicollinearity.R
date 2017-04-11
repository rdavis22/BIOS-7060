# Read in the country-savings data set
savings <- read.table("Lecture20_data_multicollinearity_1.txt", header=T)
longley  <- read.table("Lecture20_data_multicollinearity_2.txt", header=T)

# Change the data scales, using the data set "savings"
# Regression in original scale
g0 <- lm(sr ~ ., savings)
# Change  to dollars in thousand
g1 <- lm(sr ~ pop15 + pop75 + I(dpi/1000) + ddpi, savings)
# Scale/Center all variables
savings0 = savings[,-1]
savsc = data.frame(scale(savings0))
g2 <- lm(sr ~ ., data=savsc)


# Correlation among X's
round(cor(longley[, -7], use="complete.obs"), 3)

# VIF from faraway package
library(faraway)
g <- lm(Employed ~ ., longley)
x <- as.matrix(longley[, -7])
vif(x)

# Eigenvalue for X'X
e0 = eigen(t(x) %*% x)
e = e0$values
round(e, 3)

# Condition index
sqrt(e[1]/e)

# Decomposition of variation of estimators
library(MASS)
xsvd = svd(x)
v = xsvd$v
# The i-th column in the returned matrix represents the decomposition
#   of Var(b_i)
apply(v, 1, 
	function(x) {
		t = x^2/e
		return( round(t/sum(t), 3) )
	} 
)

