# Read in the country-savings data set
savings  <- read.table("Lecture13_data_linear_hypothesis.txt", header=T)

# Summary statistics of the data
summary(savings)

# Multiple linear regression with order of pop15, pop75, dpi, ddpi
g1 = lm(sr ~ pop15 + pop75 + dpi + ddpi, data=savings)

# The null model: sr = beta0 + epsilon
g0 = lm(sr ~ 1, data=savings)

# Test the significance of the whole model
#   H0: beta1 = beta2 = ... = beta_k = 0
anova(g0, g1)

# Test the significance of a particular predictor, say, pop15, given the
#   other three predictors
#   H0: beta_pop15 = 0
#   One can also conduct this by using the partial sum of squares (or type II
#   sum of squares in SAS). See Lecture 08 for more details
g2 = lm(sr ~ pop75 + dpi + ddpi, data=savings)
anova(g2, g1)

# Test the significane of a subset of predictors, given the other predictors
#   E.g., pop15 and pop75, given dpi and ddpi
#   H0: beta_pop15 = beta_pop75 = 0
g3 = lm(sr ~ dpi + ddpi, data=savings)
anova(g3, g1)

# Test a linear hypothesis, via linearHypothesis() from "car" package
#   The specification is based on beta's
#   Syntax: linearHypothesis(model, hypothesis.matrix)
#   where model is the full model, e.g., g1 specified above
#   hypothesis.matrix is the C matrix in Cb = 0. Make sure to only
#      include the independent linear functions. 
#      e.g., H0: beta_pop15 + beta_pop75 = 0, beta_dpi + beta_ddpi = 0
#      So the C matrix has two rows. The first row is 0, 1, 1, 0, 0
#      and the second row is 0, 0, 0, 1, 1. Thus, hypothesis matrix will be
#      matrix(c(0, 0, 1, 0, 1, 0, 0, 1, 0, 1), 2, 5)
linearHypothesis(g1, matrix(c(0, 0, 1, 0, 1, 0, 0, 1, 0, 1), 2, 5))

# Test a linear hypothesis, an alternative way based on predictors
# For above example, H0: beta_pop15 + beta_pop75 = 0, 
#                        beta_dpi + beta_ddpi = 0
# We can figure out the corresponding new predictors are pop15 - pop75, and 
#   dpi - ddpi. In other words, the restricted model is
#   sr = alpha0 + alpha1 * (pop15 - pop75) + alpha2 * (dpi - ddpi)
g4 = lm(sr ~ I(pop15 - pop75) + I(dpi - ddpi), data=savings)
anova(g4, g1)

# Note: if we have a new predictor 2*pop15 - pop75, we can simply specify it
#   as I(2*pop15 - pop75) in the model. So the following two are equivalent
linearHypothesis(g1, matrix(c(0, 1, 2, 0, 0), 1, 5))
g5 = lm(sr ~ I(2*pop15 - pop75) + dpi + ddpi, data=savings)
anova(g5, g1)
