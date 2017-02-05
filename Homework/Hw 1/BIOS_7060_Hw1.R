####BIOS 7060: Hw 1####
##Load Packages and data##
if(!require(tidyverse))
  install.packages("tidyverse")
if(!require(alr3))
  install.packages("alr3")
if(!require(car))
  install.packages("car")
if(!require(MethComp))
  install.packages("MethComp")

#Diabetes Data
#hw1.data<-read.table(file=file.choose(), header=T, sep="")

####Problem 1####
###Part A###
#subset data to have just diabetes patients
hw1a.data<-subset(hw1.data, hba1c>6.5)
##1A-1) fit hba1c vs. fbg
hw1a.model<-lm(hba1c~fbg, data=hw1a.data)

##1A-2) pure error (l-o-f) and homogeneity
#*Pure Error: Lack-of-Fit
#choose 5 categories from which to subset the diabetes data
hw1a2.data<-subset(hw1a.data, fbg_round==5.2|fbg_round==7.2|fbg_round==9.2|
                     fbg_round==11.2|fbg_round==12.8)
#get the model to determine the pure Error
hw1a2.model<-lm(hba1c~fbg_round, data=hw1a2.data)

#pure error from the alr3 package
purErr<-pureErrorAnova(hw1a2.model)

#alternate method for pure error 
hw1a2_fac.model<-lm(hba1c~factor(fbg_round), data=hw1a2.data)
purErr_alt<-anova(hw1a2.model, hw1a2_fac.model)

#*Homogeneity of pure errors
#will return a logical vector
t01<-apply(outer(hw1a2.data$fbg_round, hw1a2.data$fbg_round, "=="), 1, sum)>1
#subset data based on t0
tmpD<-hw1a2.data[t01,]

#Levene's test: mean
lvne_mn.prb1<-leveneTest(tmpD$hba1c, tmpD$fbg_round, center = mean)
#Levene's test: median
lvne_md.prb1<-leveneTest(tmpD$hba1c, tmpD$fbg_round, center = median)
#Bartlest's test
brtlt.prb1<-bartlett.test(tmpD$hba1c, tmpD$fbg_round)