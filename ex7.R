library(ISLR2)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket[,-9])

attach(Smarket)
plot(Volume)

glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag5 + Volume, data = Smarket, family = binomial)
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[, 4]

glm.probs <- predict(glm.fits, type = "response")
glm.probs[1:10]
contrasts(Direction)

#convert predicted probabilities into class labels
glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction)
mean(glm.pred == Direction)

train <- (Year < 2005)
Smarket.2005 <- Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]

glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 +Volume, data = Smarket, family = "binomial", subset = train)
glm.probs <- predict(glm.fits, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)

#fitting Lag1 and Lag2 which seem to have the highest predictive power in 
#the original logistic regression model
glm.fits <- glm(Direction ~ Lag1 + Lag2, data = Smarket, family = "binomial", subset = train)
glm.probs <- predict(glm.fits, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

predict(glm.fits, newdata = data.frame(Lag1 = c(1.2,1.5), Lag2 = c(1.1,-0.8)), type = "response")
