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
#path<-"C:/Users/Rick/Documents/Tulane/MPH/BIOS\ 7060/Homework/Hw\ 3/"
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
#transformed hba1c response variable
prb3_1.tibble$hbtrans.prb3_1<-prb3_1.tibble$hba1c^trans.prb3_1

##new model
prb3_1new.lm<-lm(hbtrans.prb3_1~fbg, data = prb3_1.tibble)
#Q-Q plot of new model
plot(prb3_1new.lm, 2)

####Problem 3.2####
#remove "sex" categorical predictor and the non-transformed "hba1c" to perform...
#...stepwise model selection on all continuous predictors (incl. transformed hba1c)
attach(prb3_1.tibble)
prb3_2.tibble<-tibble(Age, bmi, fbg, fins, tg, tcho, hdl, ldl, hbtrans.prb3_1)
detach(prb3_1.tibble)

##Step wise regression with AIC and BIC as selection criterion
#initial full model
prb3_2.lm<-lm(hbtrans.prb3_1~., data=prb3_2.tibble)
#stepwise based on AIC
prb3_2.AIC<-step(prb3_2.lm, direction = "both")
#stepwise based on BIC
prb3_2.BIC<-step(prb3_2.lm, direction = "both", k=log(nrow(prb3_2.tibble)))

####Problem 3.3####
#new dataframe for problem 3.3
attach(prb3_1.tibble)
prb3_3.tibble<-tibble(sex, hbtrans.prb3_1, fbg, hba1c)
detach(prb3_1.tibble)

#regression in males vs. female
prb3_3male.lm<-lm(hbtrans.prb3_1[sex=="male"]~fbg[sex=="male"], data = prb3_3.tibble)
prb3_3female.lm<-lm(hbtrans.prb3_1[sex=="female"]~fbg[sex=="female"], data = prb3_3.tibble)

#regression in diabetics vs. non-diabetics
prb3_3dia.lm<-lm(hbtrans.prb3_1[hba1c>6.5]~fbg[hba1c>6.5], data = prb3_3.tibble)
prb3_3nondia.lm<-lm(hbtrans.prb3_1[hba1c<6.5]~fbg[hba1c<6.5], data = prb3_3.tibble)

##Plots for the regression lines##
#Overall relationship
prb3_3overall.ggplot<-ggplot(data = prb3_3.tibble)+
  geom_jitter(aes(x=fbg, y=hbtrans.prb3_1))+
  geom_smooth(aes(x=fbg, y=hbtrans.prb3_1), method = lm, se=F)+
  labs(x = "fbg", y = "Transformed hba1c", title="Transformed hba1c vs. fbg (Overall Trend)")
  
#Male vs. Female
prb3_3sex.ggplot<-ggplot()+
  geom_jitter(aes(x=fbg[sex=="male"], y=hbtrans.prb3_1[sex=="male"]), colour="blue")+
  geom_smooth(aes(x=fbg[sex=="male"], y=hbtrans.prb3_1[sex=="male"]), method = lm, se=T, colour="blue")+
  geom_jitter(aes(x=fbg[sex=="female"], y=hbtrans.prb3_1[sex=="female"]), colour="red")+
  geom_smooth(aes(x=fbg[sex=="female"], y=hbtrans.prb3_1[sex=="female"]), method = lm, se=T, colour="red")+
  labs(x = "fbg", y = "Transformed hba1c", title="Transformed hba1c vs. fbg in Males (blue) and Females (red)")

#
prb3_3sex.ggplot<-ggplot()+
  geom_jitter(aes(x=fbg[sex=="male"], y=hbtrans.prb3_1[sex=="male"]), colour="blue")+
  geom_smooth(aes(x=fbg[sex=="male"], y=hbtrans.prb3_1[sex=="male"]), method = lm, se=F, colour="blue")+
  geom_jitter(aes(x=fbg[sex=="female"], y=hbtrans.prb3_1[sex=="female"]), colour="red")+
  geom_smooth(aes(x=fbg[sex=="female"], y=hbtrans.prb3_1[sex=="female"]), method = lm, se=F, colour="red")+
  labs(x = "fbg", y = "Transformed hba1c", title="Transformed hba1c vs. fbg in Males (blue) and Females (red)")