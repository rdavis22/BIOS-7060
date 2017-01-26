# The package "car" needs to be installed
if(!require(car))
  install.packages("car")

# the data from the textbook is used here (data 2.1)
#"Lecture03_data_homogeneity_of_PE.txt"
data2.1 <- read.table(file=file.choose(), header=T)


t01 = apply(outer(data2.1$X, data2.1$X, "=="), 1, sum)>1
tmpD = data2.1[t01,]

# three tests
# Levene's test wtih mean
levene_1<-leveneTest(tmpD$Y, tmpD$X, center=mean)

# Levene's test with median
levene_2<-leveneTest(tmpD$Y, tmpD$X, center=median)

# Bartlett's test
brtlt<-bartlett.test(tmpD$Y, tmpD$X)

