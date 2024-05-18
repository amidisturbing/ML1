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

#2.3.5 Additional Graphical and Numerical Summaries
plot(Auto$cylinders, Auto$mpg)
attach(Auto)
plot(cylinders, mpg)
cylinders
#as.factor() function converts quantitative variables into qualitative variables
cylinders <- as.factor(cylinders)
#If the variable plotted on the x-axis is qualitative, 
#then boxplots will automatically be produced by the plot() function.
plot(cylinders, mpg)
plot(cylinders, mpg, col = "red")
plot(cylinders, mpg, col = "red", varwidth = T)
plot(cylinders, mpg, col = "red", varwidth = T)
plot(cylinders, mpg, col = "red", varwidth = T, xlab = "cylinders", ylab = "MPG")

#hist
hist(mpg)
hist(mpg, col = 2)
hist(mpg, col = 2, breaks = 15)
#scatterplot matrix
pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, data = Auto)

plot(horsepower, mpg)
identify(horsepower, mpg, name)

summary(Auto)
#clean up
detach(Auto)
search() # shows you the elements attached to the search path

#3.1.2 Quadratic regression model
# scatter plot
plot(mpg ~ horsepower, data = Auto)
# simple linear regression
lm_hp_lin <- lm(mpg ~ horsepower, data=Auto)
lm_hp_quadr <- lm(mpg ~ horsepower + I(horsepower^2), data=Auto)
# model summary
summary(lm_hp_quadr)
# Visualisation: for a simple linear model you can use abline
abline(lm_hp_lin, col = "red")
# for the quadratic model we build our own predictor function
f_q <- function(x){
  coefs <- coef(lm_hp_quadr)
  coefs[1] + coefs[2] * x + coefs[3] * x^2
}
curve(f_q, 40, 230, add = TRUE, col = "blue")

#Lab 5.3.1 The Validation Set Approach
library(ISLR2)
set.seed(1)
train <- sample(392, 196)

lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)

attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train]^2)

lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

set.seed(2)
train <- sample(392, 196)
lm.fit <- lm(mpg ~horsepower, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
lm.fit2 <- lm(mpg ~poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
lm.fit3 <- lm(mpg ~poly(horsepower, 3 ), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)
#clean up
detach(Auto)
search() # shows you the elements attached to the search path

#5.3.2 Leave-One-Out Cross-Validation (LOOCV)
#if we use glm() to fit a model without passing in the family argument, 
#then it performs linear regression, just like the lm() function.
glm.fit <- glm(mpg ~ horsepower, data = Auto)
coef(glm.fit)
#identical linear regression models:
lm.fit <- lm(mpg ~ horsepower, data = Auto)
coef(lm.fit)
#we will perform linear regression using the glm() function rather than 
#the lm() function because the former can be used together with cv.glm(). 
library(boot)
glm.fit <- glm(mpg ~horsepower, data = Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta

cv.error <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error

#Lab 5.3.3 k-Fold Cross-Validation
set.seed(17)
cv.error.10 <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
  }
cv.error.10

