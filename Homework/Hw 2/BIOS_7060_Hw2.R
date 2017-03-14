####BIOS 7060: Homework 2####
###Load Libraries###
if(!require(tidyverse)){
  install.packages("tidyverse"); library(tidyverse)}
if(!require(car)){
  install.packages("car"); library(car)}
  if(!require(nlme)){
    install.packages("nlme"); library(nlme)}

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
partres.plots<-cr