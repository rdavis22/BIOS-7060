# Read in the country-savings data set
savings  <- read.table("Lecture08_data_extra_sum_of_squares.txt", header=T)

# Summary statistics of the data
summary(savings)

# Multiple linear regression with order of pop15, pop75, dpi, ddpi
g1 = lm(sr ~ pop15 + pop75 + dpi + ddpi, data=savings)

# The anova table contains type I or sequential sum of squares
anova(g1)

# Multiple linear regression with order of dpi, ddpi, pop15, pop75
g2 = lm(sr ~ dpi + ddpi + pop15 + pop75, data=savings)

# The anova table contains type I or sequential sum of squares
anova(g2)

# "car" package is needed for the following analysis
library(car)

# ANOVA table contains partial or type II sum of squares
Anova(g1, type="II")
