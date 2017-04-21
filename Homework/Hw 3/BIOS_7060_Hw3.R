####BIOS 7060: Homework 3####
###Load Libraries###
if(!require(tidyverse)){
  install.packages("tidyverse"); library(tidyverse)}
if(!require(car)){
  install.packages("car"); library(car)}
if(!require(MASS)){
  install.packages("MASS"); library(MASS)}

####Problem 1####
###Data Input###
#path<-"/Tulane/MPH/BIOS\ 7060/Homework/Hw\ 3/"
prb3_1.data<-read.table(file="Hw3_1.txt", header=T)
#convert dataframe to more useable tibble forr from dplyr
prb3_1.tibble<-as_tibble(prb3_1.data)
#remove dataframe
rm(prb3_1.data)

###Linear regression model###
#model for the regression
attach(prb3_1.tibble)
prb3_1.mod<-hba1c~fbg
#regression analysis
prb3_1.lm<-lm(prb3_1.mod, data=prb3_1.tibble)
detach(prb3_1.tibble)

###Box-Transformation and rerun model###
#Box-Cox Transformation
prb3_1.bc<-boxCox(prb3_1.lm)
#transformation to apply to the hba1c observations
trans.prb3_1<-powerTransform(prb3_1.lm)$lambda

##new model
prb3_1new.lm<-lm(hba1c^trans.prb3_1~fbg, data = prb3_1.tibble)
#Q-Q plot of new model
plot(prb3_1new.lm, 2)
