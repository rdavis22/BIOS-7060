# Read in the country-savings data set
savings  <- read.table("Lecture16_data_transformation.txt", header=T)

# Simple linear regression
g <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings)

# Box-Cox transformation, needing the package "MASS"
library(MASS)
boxcox(g, plotit=T) # default range for lambda: (-2, 2)

# Box-Cox transformation, with the range of lambda set to (0.5, 1.5)
boxcox(g, plotit=T, lambda = seq(0.5, 1.5, by=0.1) )