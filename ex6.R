#2 Ridge and Lasso Regression
#2.1 The baseball Hitters data
library(ISLR2)
names(Hitters)
sum(is.na(Hitters))
dim(Hitters)
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

#scatterplot
pairs(Hitters)
hist(Hitters$Salary)

x <- model.matrix(Salary ~ ., Hitters)[,-1]
y <- Hitters$Salary

#alpha=0 then a ridge regression model is fit, and if alpha=1 then a lasso model is fit
library(glmnet)
grid <- 10^seq(10,-2, length = 100)
ridge.mod <- glmnet(x,y, alpha = 0, lamda = grid)

dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))

ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))

plot(grid, coef(ridge.mod)["AtBat",], log = "x", typl = "l", xlab = "lambda")
plot(grid, coef(ridge.mod)["Hits",], log = "x", typl = "l", xlab = "lambda")
plot(ridge.mod, xvar = "lambda")

#now I should predict sth. right?
predict(ridge.mod, s = 50, type ="coefficients")[1:20, ]

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

ridge.mod <- glmnet(x[train, ], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test, ])
mean((ridge.pred - y.test)^2)
# Note that if we had instead simply fit a model with just an intercept, 
#we would have predicted each test observation using the mean of the training observations. 
#In that case, we could compute the test set MSE like this:
mean((mean(y[train]) - y.test)^2)
#We could also get the same result by fitting a ridge regression model with a very large value of λ.
#Note that 1e10 means 1010.
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test, ])
mean((ridge.pred - y.test)^2)

ridge.pred <- predict(ridge.mod, s = 0, newx = x[test, ], exact = T, x = x[train, ], y = y[train])
mean((ridge.pred - y.test)^2)
lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, exact = T, type = "coefficients", x = x[train, ], y = y[train])[1:20, ]
#WEITER ON PAGE 278