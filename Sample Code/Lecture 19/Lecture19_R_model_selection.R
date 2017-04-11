# Read in the country-savings data set
statedata  <- read.table("Lecture19_data_model_selection.txt", header=T)

# Life.exp is used as Y, and others as X's
# Use the function "step", which uses AIC as selection criteria at default
# To use BIC, specify the value for k by using "k = log(n)", where in 
#   practice, replace "n" with the variable corresponding to the sample size
# Three options are available through the "direction" option, including
#  "both" for stepwise, "backward" for backward elimination, and 
#  "forward" for forward selection

# Backward elimination
# Start with the full model
g <- lm(Life.Exp ~ ., data=statedata)
step(g, direction = "backward") # backward based on AIC

# backward based on BIC
step(g, direction = "backward", k = log(sum(anova(g)[1])+1)) 

# Model selection based on adjusted R2, Cp, R2
# Needs library: leaps
library(leaps)

# Need at two options: x for predictors, and y  for the outcome
x <- model.matrix(g)[,-1]
y <- statedata$Life.Exp
# Use option "method = (Cp, adjr2, r2)" to choose selection criteria.
g1 <- leaps(x,y, method="Cp")
# Need package "faraway"
library(faraway)
Cpplot(g1)


