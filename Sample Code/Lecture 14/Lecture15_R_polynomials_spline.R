# Read in the country-savings data set
savings  <- read.table("Lecture15_data_polynomials_spline.txt", header=T)

# simple linear regression: y - personal savings, x - ddpi
g1 <- lm(sr~ddpi, data=savings)
anova(g1)

# polynomial regression: y - personal savings, x - ddpi, ddpi^2, ddpi^3
g2 <- lm(sr~ddpi + I(ddpi^2) + I(ddpi^3), data=savings)
anova(g2)



