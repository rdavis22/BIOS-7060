# Example codes for running a linear regression analysis

# Read in the country-savings data set
savings  <- read.table("Lecture07_data_regression_in_matrix_terms.txt", header=T)

# Summary statistics of the data
summary(savings)

# multiple linear regression: y - personal savings, x1 - pop15, x2 - pop75
g1 <- lm(sr~pop15+pop75, data=savings)

# Summary of regression results: parameter estimation & sd, t-test for 
#   individual predictor, R2 and adjusted R2, F-test for the model
r1 = summary(g2)

# Variance-covariance matrix for estimations
# The unscaled matrix, the same as (X'X)^(-1)
r1$cov.unscaled 
# Estimate of standard deviation
r1$sigma 
# Scaled matrix, the same as (X'X)^(-1) * sigma^2
r1$cov.unscaled * (r1$sigma)^2 

# ANOVA table for regression
anova(g1)

# 95% confidence interval for parameters 
confint(g1)
