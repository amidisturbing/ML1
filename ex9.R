library(ISLR2)
?Boston
dim(Boston)
names(Boston)

par(mfrow=c(2,2))
plot(Boston$medv, Boston$crim)
plot(Boston$medv, Boston$zn)
plot(Boston$medv, Boston$indus)
plot(Boston$medv, Boston$age)

summary(Boston)

boxplot(Boston$medv)
sum(Boston$chas==1) # 5. 35 suburbs in this data set border the Charles river
par(mfrow=c(1,2))
boxplot(Boston$medv[Boston$chas==1], main = "medv border chas")
boxplot(Boston$medv)
median(Boston$ptratio)
Boston[which.min(Boston$medv),]
summary(Boston)
mean(Boston$crim)
sum(Boston$rm>7)
sum(Boston$rm>8)

#2.1.2 Regression Tree Model
library(rpart)
library(rpart.plot)

set.seed(1)
train <-sample(1:nrow(Boston), size =nrow(Boston)/2)
tree.boston <- rpart(medv~., data= Boston, subset = train)
print(tree.boston)
rpart.plot(tree.boston)
#What is the median house price for a flat located in a suburb with rm = 6 and lstat = 12?
#predict(tree.boston, newdata = Boston[train,])
set.seed(1)
tree.boston.full <- rpart(medv ~ . , data = Boston, subset = train, cp = 0) 
print(tree.boston.full)
rpart.plot(tree.boston.full)
# Table with cp values from cross validation
cptable <- tree.boston.full$cptable
plotcp(tree.boston.full)

minDeviance <- which.min(cptable[ , "xerror"])
dotted <- cptable[minDeviance, "xerror"] + cptable[minDeviance, "xstd"] 
abline(h = dotted, col = "red", lty = 2)
# which is the first row less than this value
cpPruning <- cptable[cptable[, "xerror"] < dotted,][1]
cpPruning
cptable
prune.boston <- prune(tree.boston, cp = 0.0072653855) 
prune.boston
rpart.plot(prune.boston)

# on training data
# full tree
pred.train.full<-predict(tree.boston.full, newdata = Boston[train,])
mean((Boston$medv[train]-pred.train.full)^2)
## default tree
pred.train.default<-predict(tree.boston, newdata = Boston[train,])
mean((Boston$medv[train]-pred.train.default)^2)
## pruned tree
pred.train.pruned<-predict(prune.boston, newdata = Boston[train,])
mean((Boston$medv[train]-pred.train.pruned)^2)

### on the test data
# full tree
pred.test.full <- predict(tree.boston.full, newdata = Boston[-train, ])
mean((Boston$medv[-train] - pred.test.full)^2)
# default tree
pred.test.default <- predict(tree.boston,newdata=Boston[-train,])
mean((Boston$medv[-train]-pred.test.default)^2)
# pruned tree
pred.test.pruned<-predict(prune.boston,newdata=Boston[-train,])
mean((Boston$medv[-train]-pred.test.pruned)^2)

boston.test <- Boston[-train, "medv"] 
plot(pred.test.pruned, boston.test) 
abline(c(0,1), col = "red", lty = 2)
