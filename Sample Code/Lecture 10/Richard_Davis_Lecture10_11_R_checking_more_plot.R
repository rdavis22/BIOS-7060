# Example codes for more model checking
#   Outliers -- internally and externally studentized residuals
#   Influential points -- Cook's D, DFBETAS, DFFITS
#   More plot -- partial regression plot, partial residual plot

# Read in the personal savings data set
savings  <- read.table("Lecture11_12_data_checking_more_plot.txt", header=T)

# Fit a regular regression model
g <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data=savings)
summary(g)

# Externally studentized residuals 
studres(g)

# Internally studentized residuals
stdres(g)

# Cook's D 
CookD = cooks.distance(g)
plot(CookD)

# DFBETAS
dfbeta(g)

# DFFITS
dffits(g)

par(mfrow=c(1,2))
# Partial regression plot, with pop15 as the example
d <- lm(sr ~ pop75 + dpi + ddpi, savings)$res
m <- lm(pop15 ~ pop75 + dpi + ddpi, savings)$res
plot(m, d, xlab="pop15 residuals", ylab="Saving residuals", 
	main="Partial Regression")
abline(0, g$coef['pop15'])

# Partial residual plot, with pop15 as the example
plot(savings$pop15, g$res + g$coef['pop15']*savings$pop15, 
	xlab="pop'n under 15", ylab="Saving(adjusted)", 
	main="Partial Residual")
abline(0,g$coef['pop15'])
