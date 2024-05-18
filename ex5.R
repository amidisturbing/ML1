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

#3.2 Model selection using cross-validation
?cv.glm
#Which parameter set is required for leave-one-out CV?
#K, default = number of all observations in data => LOOCV
library(boot)
glm_01 <- glm(mpg ~ horsepower + I(horsepower^2), data = Auto) 
cv_error <- cv.glm(Auto, glmfit = glm_01)
cv_error$delta

cv_error <- rep (0, 6) # Why 6 as 2nd argument? 
#-> for storing the cv errors for all fitted models
cv_error[1] <- cv.glm(Auto, glm_01)$delta[1]
#year
glm_fit_02 <- glm(mpg ~ horsepower + I(horsepower^2) + year, data = Auto) 
cv_error[2] <- cv.glm(Auto, glm_fit_02)$delta[1]
#weight
glm_fit_03 <- glm(mpg ~ horsepower + I(horsepower^2) + weight, data = Auto) 
cv_error[3] <- cv.glm(Auto, glm_fit_03)$delta[1]
#acceleration
glm_fit_04 <- glm(mpg ~ horsepower + I(horsepower^2) + acceleration, data = Auto) 
cv_error[4] <- cv.glm(Auto, glm_fit_04)$delta[1]
#displacement
glm_fit_05 <- glm(mpg ~ horsepower + I(horsepower^2) + displacement, data = Auto) 
cv_error[5] <- cv.glm(Auto, glm_fit_05)$delta[1]
#cylinders
glm_fit_06 <- glm(mpg ~ horsepower + I(horsepower^2) + cylinders, data = Auto) 
cv_error[6] <- cv.glm(Auto, glm_fit_06)$delta[1]

plot(cv_error, type="b")

library(ISLR2) 
library(boot)
data("Auto")
mod_glm_loop <- mod_glm_01 <- "mpg ~ horsepower + I(horsepower^2)" 
new_vars_loop <- new_vars <- c("cylinders", "displacement", "weight","acceleration", "year") 
glm_01 <- glm(as.formula(mod_glm_01), data = Auto)
# Initialisation of loop variables
mse_loocv_best <- loocv_mse <- cv.glm(Auto, glmfit = glm_01)$delta[1] 
best_vars <- NULL
repeat({
  best_var <- NULL
  for(new_var in new_vars_loop){
    model_loop <- as.formula(paste(mod_glm_loop, "+", new_var)) 
    glm_fit_loop <- glm(model_loop, data = Auto)
    mse_loocv_loop <- cv.glm(Auto, glmfit = glm_fit_loop)$delta[1]
    # improvement?
    if(mse_loocv_loop < mse_loocv_best){ 
      mse_loocv_best <- mse_loocv_loop 
      best_var <- new_var
    } }
  # If no improvement, stop calculation
  if(is.null(best_var)) break
  # update model
  mod_glm_loop <- paste(mod_glm_loop, "+", best_var) # remove best_var from variable set
  new_vars_loop <- setdiff(new_vars_loop, best_var) # store current best loocv_mse
  loocv_mse <- c(loocv_mse, mse_loocv_best) # store current best new variable 
  best_vars <- c(best_vars, best_var)
  })
# How large is the improvement?
plot(loocv_mse)
# only the first two additional variables decrease the MSE to a large extent
# Best Modell:
mod_glm_best <- paste(mod_glm_01, " + ", paste(best_vars[1:2], collapse = "+"))
glm_best <- glm(as.formula(mod_glm_best), data = Auto)
summary(glm_best)
cv.glm(Auto, glm_best)$delta[1]

mod_glm_full <- paste(mod_glm_01, " + ", paste(best_vars, collapse = "+"))
glm_best_AIC <- glm(as.formula(mod_glm_full), data = Auto) |> step(trace = 0)
glm_best_BIC <- glm(as.formula(mod_glm_full), data = Auto) |> step(trace = 0, k = log(nrow(Auto))) 
summary(glm_best_AIC) 
summary(glm_best_BIC)

#4 Programming Cross-Validation
set.seed(1234567890)
x <- matrix(1:30, ncol = 1)
y_true <- cut(x, breaks = c(0, 10, 20, 30), labels = c(10, 15, 8)) |>
  as.character() |> as.integer() 
epsilon_train <- rnorm(length(x)) 
y_obs <- y_true + epsilon_train

plot(x[,1], y_true, type = "p", ylim = range(c(y_obs, y_true)), xlab = "x", ylab = "y")
lines(x[,1]-0.5, y_true, type = "s", lty = 2, col = "grey") 
points(x[,1], y_obs, lty = 2, col = "red", pch = 20) 
legend("topright", legend = c("true values", "observed data"), col = c("black", "red"), pch = c(1, 20))

# create new data by leaving one observation out
x_loo <- x[-1]
y_loo <- y_obs[-1]

# fit the linear regression model with the loo data
lm_loo <- lm(y_loo ~ x_loo)
# plot the resulting regression to existing plot
abline(lm_loo, col = "grey")
# find the sum of squared error using the missing point
(predict(lm_loo, newdata = data.frame(x_loo = x[1])) - y_obs[1])^2

# initialization of result vector
loo_sse <- NULL
# loop over each of the n observations
for(i in y_obs){
  #create new data by leaving one observation out
  x_loo <- x[-i]
  y_loo <- y_obs[-i]
  ## fit the linear regression model with the loo data
  lm_loo <- lm(y_loo ~ x_loo) #plot the resulting regression
  # plot the resulting regression to existing plot
  abline(lm_loo, col="grey")
  # store current sse to result vector using the missing point
  loo_sse <- c(loo_sse, (predict(lm_loo, newdata = data.frame(x_loo = x[i])) - y_obs[i])^2)
  }
# calculate mean of calculated sse
mse_loocv <- mean(loo_sse)
mse_loocv

# Initialisation of result vector
mse_loocv <- rep(NA, 10)
# outer loop changing the polynomial degree
for(k in 1:10){
  # inner loop over each of the n observations
  for(i in y_obs){
    #create new data by leaving one observation out
    x_loo <- x[-i]
    y_loo <- y_obs[-i]
    ## fit the linear regression model with the loo data
    lm_loo <- lm(y_loo ~ poly(x_loo, degree = k, raw = T))
    # store current sse to result vector using the missing point
    loo_sse <- c(loo_sse, (predict(lm_loo, newdata = data.frame(x_loo=x[i])) - y_obs[i])^2) }
  # store mse fur current polynomial degree in mse_loo
  mse_loocv[k] <- mean(loo_sse)
}
plot(mse_loocv, type="b")

lm_best_mse <- lm(y_obs ~ poly(x, degree = which.min(mse_loocv)))
plot(x, lm_best_mse$fitted.values, col="red", type = "l") 
points(x, y_obs)
lines(x, y_true)