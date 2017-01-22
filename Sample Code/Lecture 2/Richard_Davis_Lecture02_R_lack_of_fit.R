if(!require(tidyverse))
  install.packages("tidyverse")
if(!require(alr3))
  install.packages("alr3")

# Read the data
corrosion <- read.table("Lecture02_data_lack_of_fit.txt", header=T)

# Regression loss on Fe
g <- lm(loss~Fe, data=corrosion)
summary(g)

##GGPlot: Plot the data with the regression line
p<-ggplot(corrosion, aes(x=corrosion$Fe, y=corrosion$loss))+
  geom_point(color="red")+
  labs(x="Iron content", y="weight loss")+
  geom_smooth(method="lm", se=FALSE)

# plot(corrosion$Fe,corrosion$loss,xlab="Iron content",ylab="Weight loss", 
# 	cex=1.5, pch=16, col=2, cex.axis=1.5, cex.lab=2)
# abline(g$coef, lwd=2, col=2)

# Regression loss on Fe with Fe being a factor type of variable
ga <- lm(loss~factor(Fe), data=corrosion)

# Comparison between two models in order to study pure errors
anova(g, ga)


# An alternative way of testing pure errors
# install the package "alr3", and use the function pureErrorAnova()
g2 <- lm(loss~Fe, data=corrosion)
pureErrorAnova(g2)
