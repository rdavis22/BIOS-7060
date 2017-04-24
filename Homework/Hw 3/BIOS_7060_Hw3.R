####BIOS 7060: Homework 3####
###Load Libraries###
if(!require(tidyverse)){
  install.packages("tidyverse"); library(tidyverse)}
if(!require(car)){
  install.packages("car"); library(car)}
if(!require(MASS)){
  install.packages("MASS"); library(MASS)}
if(!require(faraway)){
  install.packages("faraway"); library(faraway)}
if(!require(ridge)){
  install.packages("ridge"); library(ridge)}
if(!require(lmridge)){
  install.packages("lmridge"); library(lmridge)}
###Set the proper file path###
#path<-"C:/Users/Rick/Documents/Tulane/MPH/BIOS\ 7060/Homework/Hw\ 3/"
#***Make sure to set the path as the one with 'Hw3_1.txt' in it.

####Problem 3.1####
###Data Input###
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
#make males the reference level
prb3_3.tibble$sex<-relevel(prb3_3.tibble$sex, ref = "male")
detach(prb3_1.tibble)
#attach the new data frame to use ggplot2 without having to specify the dataframe...
#...in the 'ggplot' statement
attach(prb3_3.tibble)

###Plots for the regression lines (visualized before running analysis###
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

#Diabetics vs. non-diabetics
prb3_3dia.ggplot<-ggplot()+
  geom_jitter(aes(x=fbg[hba1c<6.5], y=hbtrans.prb3_1[hba1c<6.5]), colour="blue")+
  geom_smooth(aes(x=fbg[hba1c<6.5], y=hbtrans.prb3_1[hba1c<6.5]), method = lm, se=T, colour="blue")+
  geom_jitter(aes(x=fbg[hba1c>6.5], y=hbtrans.prb3_1[hba1c>6.5]), colour="red")+
  geom_smooth(aes(x=fbg[hba1c>6.5], y=hbtrans.prb3_1[hba1c>6.5]), method = lm, se=T, colour="red")+
  labs(x = "fbg", y = "Transformed hba1c", title="Transformed hba1c vs. fbg in Non-diabetics (blue) and Diabetics (red)")

#detach and then explicitly call variables in the 'prb3_3.tibble' dataframe to avoid...
#...confusion with dataframes from problems 1 and 2
detach(prb3_3.tibble)

###regression in males vs. female (dummy coded)###
prb3_3sex.lm<-lm(hbtrans.prb3_1~fbg+sex+fbg*sex, data = prb3_3.tibble)

###regression in diabetics(=1) vs. non-diabetics(=0)###
#create categorical status variable for "non-diabetics"(=0) vs "diabetics"(=1)
status<-c()
for (i in 1:length(prb3_3.tibble$hbtrans.prb3_1)){
   if (prb3_3.tibble$hba1c[i]<6.5){
     #non-diabetics will be coded as 0
     status[i]<-0
   }
   else if (prb3_3.tibble$hba1c[i]>6.5){
     #diabetics will be coded as 1
    status[i]<-1
   }
}
#turn diabetes 'status' variable into class 'factor'
status<-factor(status)
prb3_3dia.lm<-lm(hbtrans.prb3_1~fbg+status+fbg*status, data = prb3_3.tibble)


####Problem 3.4####
###Data Input###
prb3_4.data<-read.table(file="Hw3_2.txt", header=T)
#convert dataframe to more useable tibble forr from dplyr
prb3_4.tibble<-as_tibble(prb3_4.data)
#remove dataframe
rm(prb3_4.data)
attach(prb3_4.tibble)

###scaling and centering Data###
#intial regression model
g0<-lm(CHOL ~ ., data = prb3_4.tibble)
#new dataframe with ID, PILL, and PAIR removed
prb3_4_0.tibble<-tibble(AGE, HT, WT, CHOL, ALB, CALC, URIC, wtalb, acucont)
#remove the missing values
prb3_4_0.tibble<-na.omit(prb3_4_0.tibble)
#scaled new dataframe
prb3_4_0sc.tibble<-as_tibble(data.frame(scale(prb3_4_0.tibble)), na.rm=T)
#remove missing values
prb3_4_0sc.tibble<-na.omit(prb3_4_0sc.tibble)

##check the correlations (gives correlation matrix)##
#get correlation matrix (exclude "CHOL" response variable which is in 4th position)
cormat.prb3_4<-round(cor(prb3_4_0sc.tibble[,-4], use = "complete.obs"), 3)
#determinant of the correlation matrix is checked: close to 0=correlation; close to 1=no corr.
dtrm.prb3_4<-det(cormat.prb3_4)

###Assessing for Multicollinearity###
##Method 1: VIF (from faraway package)##
#get the regression equation
g.vif <- lm(CHOL ~ ., prb3_4_0.tibble)
#convert the dataframe (excluding the "CHOL" response variable) as a matrix
x <- as.matrix(prb3_4_0.tibble[, -4])
prb3_4.vif<-vif(x)

##Method 2: Eigenvalues##
#compute the eigenvalues and eigenvectors
e0<-eigen(t(x) %*% x)
#extract the eigenvalues and round to three decimal places
e<-round(e0$values, 3)

##Method 3: Condition Indices##
#get the condition indexes
CondInd.prb3_4<-sqrt(e[1]/e)
#Singular Value Decomposition
xsvd = svd(x)
v = xsvd$v
# The i-th column in the returned matrix represents the decomposition
#   of Var(b_i)
apply(v, 1, 
      function(x) {
        t = x^2/e
        return(round(t/sum(t), 3))
      })

####Problem 3.5####
###Data Input###
prb3_5.data<-read.table(file="Hw3_2.txt", header=T)
#convert dataframe to more useable tibble forr from dplyr
prb3_5.tibble<-as_tibble(prb3_5.data)
#remove dataframe
rm(prb3_5.data)

###regular regression###
#model for the regression
g.prb3_5<-lm(CHOL~AGE+WT+ALB+CALC+URIC+wtalb+acucont, data = prb3_5.tibble)

###ridge regression###
##Based on the 'MASS' package
gr.prb3_5<-lm.ridge(CHOL~AGE+WT+ALB+CALC+URIC+wtalb+acucont,
                     data = prb3_5.tibble, lambda = .13)
 matplot(gr.prb3_5$lambda, t(gr.prb3_5$coef), type="l", xlab=expression(lambda),
         ylab = expression(hat(beta)))
 abline(h=0,lwd=2)

##Based on the 'ridge' package
gr1.prb3_5<-linearRidge(CHOL~AGE+WT+ALB+CALC+URIC+wtalb+acucont,
                        data = prb3_5.tibble, lambda = "automatic")
##Based on the 'lmridge' package
#summary(lmridge(CHOL~AGE+WT+ALB+CALC+URIC+wtalb+acucont,
#data = prb3_5.tibble, K=0.13))