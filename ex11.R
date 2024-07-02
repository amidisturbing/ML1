require(rpart)
require(rpart.plot)
library(ISLR2)

set.seed(1)

train <- sample(1:nrow(Boston), nrow(Boston)/2) # row numbers training data 
test <- (1:nrow(Boston))[-train]
tree.boston <- rpart(medv ~ ., Boston, subset = train, cp = 0.019)
#print(???)
rpart.plot(tree.boston)
pred.train <- predict(tree.boston, newdata = Boston[train, ]) 
mean((Boston$medv[train] - pred.train)^2)
pred.test <- predict(tree.boston, newdata = Boston[test, ]) 
mean((Boston$medv[test] - pred.test)^2)

#bagging
#n <- 100
bag.samp <- sample(train, size = length(train), replace = TRUE)
oob.samp <- train [- bag.samp]
n_in_bag <- length(unique(bag.samp))
n_in_bag
n_ooB <- nrow(Boston[train, ]) - n_in_bag
n_ooB
#4.Fit full tree
tree.bag <- rpart(medv ~ ., Boston, subset = bag.samp, cp = 0)
preds <- predict(tree.bag, newdata = Boston)
inbagMSE <- mean((Boston$medv - preds)[bag.samp]^2) 
inbagMSE
oobMSE <- mean((Boston$medv - preds)[oob.samp]^2)
oobMSE
test.MSE <- mean((Boston$medv-preds)[test]^2) 
test.MSE
