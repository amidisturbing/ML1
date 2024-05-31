#==============================================================================#
# R code template Classification exercise: Diabetes                            #
#==============================================================================#

# 00: packages -----------------------------------------------------------------
library(pROC)  # install package if necessary

# 01: load data ----------------------------------------------------------------
load("/Users/rafaelaneff/Documents/2024/BHT Berlin/ML1/EX/Datasets/Diabetes.Rda")  # ??? stands for "path/Diabetes.Rda"

# 02: explorative data analysis ------------------------------------------------
# Answer parts (a) to (e) in the worksheet
#a) How many observations are there?
dim(Diabetes) #-> 9629 
#b) obtain the frequency table for diabetes status.
table(Diabetes$YN)
#c) What is the mean and standard deviation of BMI and Age?
mean(Diabetes$BMI) #mean = 26.6586, sd = 7.38
sd(Diabetes$BMI)

mean(Diabetes$Age) #mean = 37.73538, sd = 21.79
sd(Diabetes$Age)
#d) Plot a histogram of BMI and Age, and a scatter plot of the two.
hist(Diabetes$BMI)
hist(Diabetes$Age)
plot(Diabetes$BMI, Diabetes$Age)
#e) Create a box plot of BMI against YN and BMI against YN.
boxplot(Diabetes$BMI, Diabetes$YN)
contrasts(Diabetes$YN)
boxplot(Diabetes$Age, Diabetes$YN)
# 03: modeling -----------------------------------------------------------------

# 03a: Train/Test Split ----
# Split the data into a train/test with 2000 observations in the test data set
set.seed(50)
n <- dim(Diabetes)[1]
testidx <- sample(n, 2000)
test <- Diabetes[testidx, ]
train <- Diabetes[-testidx, ]

table(train$YN)

# 03b: model training ----
# Fit a logistic regression model and examine the model summary.
glm.obj <- glm(YN ~ BMI, data = train, family = "binomial")
summary(glm.obj)  #it looks as if higher BMI means diabetes is more likely  
BMI.grid <- 10:82
glm.pred <- predict(glm.obj, 
                    newdata = data.frame(BMI = BMI.grid), 
                    type="response")
plot(BMI.grid, glm.pred, type = "l", xlab="BMI")

# Question: does the logistic regression Diabetes = 'Yes' or Diabetes = 'No' as
#           dependent variable? 
summary(glm.obj)
# 04: Assessment of Classification Quality -------------------------------------

# 04a: Classification matrix ----

# Define "High Risk of Diabetes" using a cut off of alpha=0.5 and construct
# the classification matrix
alpha <- 0.5 
fit1 <- fitted(glm.obj)
HiRisk <- fit1 > alpha
table(HiRisk)
tab <- table(train$YN, HiRisk, dnn = c("observed", "predicted"))
print(tab)

# The following 3 commands calculate the sensitivity, specificity and accuracy.
# Which is which?
(tab[1, 1] + tab[2, 2])/sum(tab)
tab[1, 1]/sum(tab[1, ])
tab[2, 2]/sum(tab[2, ])

# 04b: ROC curve and AUC ----
roc.obj1  <-  roc(train$YN, fit1)
plot(roc.obj1)   # R base graphics
ggroc(roc.obj1)  # ggplot graphics
auc(roc.obj1)


# roc produces a vector of thresholds (alpha), specificities and sensitivities.
# find the index of the threshold nearest to alpha=0.5
alpha <- 0.5 
idx <- which.min(abs(roc.obj1$thresholds-alpha))
idx

roc.obj1$thresholds[idx]
roc.obj1$sensitivities[idx]
roc.obj1$specificities[idx]

# do these values concur (fit) with your answers above?

# 04c: out-of-sample goodness-of-fit -----
# get the predicted probabilities for the test data
ptest <-  predict(glm.obj, newdata = test, type = "response")

# confusion matrix for the test data
table(test$YN, ptest > alpha, dnn = c("observed", "predicted"))
test.roc.obj1  <-  roc(test$YN, ptest)

# ROC curve and AUC
ggroc(list(train=roc.obj1, test=test.roc.obj1))
auc(test.roc.obj1)

# the training and test results are similar

# 05: Further modeling ---------------------------------------------------------

# 05a: Repeat the analysis done so far with variable 'Age'
glm.obj2 <- glm(YN ~ Age, data=train, family=binomial)
glm.obj2$coefficients  # it looks as if older means diabetes is more likely  
ptest2 <-  predict(glm.obj2, newdata=test, type="response")
test.roc.obj2  <-  roc(test$YN, ptest2)
ggroc(list("BMI"=test.roc.obj1, "Age"=test.roc.obj2))
auc(test.roc.obj2)

# 05b: repeat analyis with a model containing both BMI and Age 
glm.obj3 <- glm(YN ~ Age + BMI ,data = train,family=binomial)
glm.obj3$coefficients  # it looks as if BMI affects diabetes is more likely than Age, but both do.
ptest3 <-  predict(glm.obj3, newdata=test, type="response")
test.roc.obj3  <-  roc(test$YN, ptest3)
ggroc(list("BMI"=test.roc.obj1, "Age"=test.roc.obj2, "Age+BMI"=test.roc.obj3))
auc(test.roc.obj3)

alpha <- 0.5 
fit3 <- fitted(glm.obj3)
HiRisk3 <- fit3 > alpha
table(HiRisk3)
tab3 <- table(train$YN, HiRisk3, dnn = c("observed", "predicted"))
print(tab3)

# The following 3 commands calculate the sensitivity, specificity and accuracy.
# Which is which?
(tab3[1, 1] + tab3[2, 2])/sum(tab3)#accuracy
tab3[1, 1]/sum(tab3[1, ])#specificity
tab3[2, 2]/sum(tab3[2, ])#sensitivity

#change cut off probability to 0.10
alpha <- 0.19 
fit4 <- fitted(glm.obj3)
HiRisk4 <- fit4 > alpha
table(HiRisk4)
tab4 <- table(train$YN, HiRisk4, dnn = c("observed", "predicted"))
print(tab4)

(tab4[1, 1] + tab4[2, 2])/sum(tab4)
tab4[1, 1]/sum(tab4[1, ])
tab4[2, 2]/sum(tab4[2, ])


