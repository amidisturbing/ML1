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
