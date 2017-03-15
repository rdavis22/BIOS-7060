####BIOS 7060: Homework 2####
###Load Libraries###
if(!require(tidyverse)){
  install.packages("tidyverse"); library(tidyverse)}
if(!require(car)){
  install.packages("car"); library(car)}
if(!require(nlme)){
  install.packages("nlme"); library(nlme)}
if(!require(MASS)){
  install.packages("MASS"); library(MASS)}

####Problem 1####
###Data input###
##set working directory to "Hw2" folder in "BIOS 7060" directory##
#path<-"/Tulane/MPH/BIOS\ 7060/Homework/Hw 2/"
#crime data
crime.data<-read.table("HW2_data1.txt", header=T)
#convert crime.data to a dplyr tibble
crime.tibble<-as_tibble(crime.data)

###Linear Regression###
#regression model Y=female indictment rate, X=fertility rate, labor force...
#...participation rate, post secondary degree rate, male theft conviction rate
crime.lm<-lm(findict~fertil+labor+postsec+mtheft, data=crime.tibble)

###Part a)###
##Bivariate Plots##
#findict vs. fertil
fertil.ggplot<-ggplot(data=crime.tibble, aes(x=fertil, y=findict))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(title="Female Indictable-Offense Conviction Rate per 100,000 vs.
       Fertility Rate per 1000")
#findict vs. labor
labor.ggplot<-ggplot(data=crime.tibble, aes(x=labor, y=findict))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(title="Female Indictable-Offense Conviction Rate per 100,000 vs.
       Labor Force Participation Rate per 1000")
#findict vs. postsec
postsec.ggplot<-ggplot(data=crime.tibble, aes(x=postsec, y=findict))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(title="Female Indictable-Offense Conviction Rate per 100,000 vs.
       Post Secondary Degree Rate per 1000")
#findict vs. mtheft
mtheft.ggplot<-ggplot(data=crime.tibble, aes(x=mtheft, y=findict))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(title="Female Indictable-Offense Conviction Rate per 100,000 vs.
       Male Theft Conviction Rate per 1000")

###Part b)###
##Partial Residual Plots
partres.plots<-crPlots(crime.lm)

###Part c)###
##Plot of Resisduals for the model and year##
#dataframe with "Year" predictor and residuals for the model
res.tibble<-tibble(crime.res=crime.lm$residuals, year=crime.data$year)
#residual vs. year plot
res.ggplot<-ggplot(data=res.tibble, aes(x=year, y=crime.res))+
  geom_point()+
  labs(title="Model with 4 Predictors Residuals vs. Year")

###Part d)###
##Outlier analysis##
#externally studentized residuals
extres.prb1<-studres(crime.lm)

#internally studentized residuals
intres.prb1<-stdres(crime.lm)

#outlier test for the residuals (check the 5 largest residuals)
outliers.prb1<-outlierTest(crime.lm, cutoff = 5)

##Influence Analysis##
# Cook's D 
CookD.prb1 = cooks.distance(crime.lm)
plot(CookD.prb1)
#influencePlot(crime.lm)

# DFBETAS
dfbet.prb1<-dfbeta(crime.lm)

# DFFITS
dffts.prb1<-dffits(crime.lm)