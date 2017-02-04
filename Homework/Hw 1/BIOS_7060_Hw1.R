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
attach(hw1.data)

####Problem 1####
##Part A##
#fit hba1c vs. fbg
hw1a.model<-lm(hba1c~fbg, data=hw1.data)