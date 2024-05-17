library(FNN)
library(ISLR2)
library(boot)

#2.1 A simulation study
##Bias_Variance_Trade_Off.R1 done
#3.1 Model evaluation using CV
##3.1.1 The Auto data set
data("Auto", package = "ISLR2")

?Auto
str(Auto)

set.seed(1)
train <- sample(392, 196)