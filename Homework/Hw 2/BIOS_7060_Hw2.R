####BIOS 7060: Homework 2####
###Load Libraries###
if(!require(tidyverse)){
  install.packages("tidyverse"); library(tidyverse)
}
if(!require(nlme)){
  install.packages("nlme"); library(nlme)
}

####Problem 1####
###Data input###
##set working directory to "Hw2" folder in "BIOS 7060" directory##
#crime data
crime.data<-read.table("HW2_data1.txt", header=T)
#convert crime.data to a dplyr tibble
crime.tibble<-as_tibble(crime.data)

###Linear Regression###
#regression model Y=female indictment rate, X=fertility rate, labor force...
#...participation rate, post secondary degree rate, male theft conviction rate
crime.lm<-lm(findict~fertil+labor+postsec+mtheft, data=crime.tibble)