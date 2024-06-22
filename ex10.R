library(rpart)
library(rpart.plot)

load("/Users/rafaelaneff/Documents/2024/BHT Berlin/ML1/EX/Datasets/Diabetes.Rda")
set.seed(50)
n <- dim(Diabetes)[1]
testidx <- sample(1:n, 2000)
test <- Diabetes[testidx, ]
train <-Diabetes[-testidx, ]

tree.diabetes <- rpart(YN ~ Age + BMI, data = train)
rpart.plot(tree.diabetes)
tree.diabetes.full <- rpart(YN ~ Age + BMI , data = train, cp = 0) 
rpart.plot(tree.diabetes.full)
cptable <- tree.diabetes.full$cptable
minDeviance <- which.min(cptable[ , "xerror"])
dotted <- cptable[minDeviance, "xerror"] + cptable[minDeviance, "xstd"] 
# which is the first row less than this value
cpPruning <- cptable[cptable[, "xerror"] < dotted,][1]
cpPruning
cptable #find cpvalue without rounding
##FRAGE! cpvalue kann nicht stimmen
prune.diabetes <- prune(tree.diabetes.full, cp = 0.0046) 
prune.diabetes
rpart.plot(prune.diabetes)
#2.2.c) 
#fit the tree using cp = 0.0046
tree.diabetes.fit <- rpart(YN ~ Age + BMI , data = train, cp = 0.0046)
rpart.plot(tree.diabetes.fit)
require(pROC)
predDiabetes <- predict(prune.diabetes, newdata=test, type = "prob")[,2]
# ROC analysis
roc.obj1 <- roc(test$YN, predDiabetes)
ggroc(roc.obj1)
auc(roc.obj1)
table(predDiabetes)
tab <- table(test$YN, predDiabetes, dnn = c("observed", "predicted"))
print(tab)

tab[1, 1]/sum(tab[1, ])#specificity
tab[2, 2]/sum(tab[2, ])#sensitivity

#Loss matrix
#first change at 1.4 = 4 end-nodes
lossmat <- matrix(c(0, 1.4, 1, 0), nrow = 2, ncol = 2) # x will be set later, see below
lossmat
rptree <- rpart(YN ~ BMI + Age, data = train, parms = list(loss = lossmat))
rpart.plot(rptree)
predRptree <- predict(rptree, newdata=test, type = "prob")[,2]
roc.obj2 <- roc(test$YN, predRptree)
ggroc(roc.obj2)
auc(roc.obj2)
table(predRptree)
tab <- table(test$YN, predRptree, dnn = c("observed", "predicted"))
print(tab)

#Written Exercise 
#Classification. tree
#Gini coefficient
0.4709302*(1-0.4709302)+0.1337209*(1-0.1337209)+0.3953488*(1-0.3953488)
(((242/(242+146))*0.59248)+(146/(146+242)*0.5846312))/6
((0.5040724*216/(216+172)+(172/(216+172))*0.6040427))/6
#entropy D
#Model 1

A11 <-  104
B11 <- 111
C11 <- 27
A12<-  81
B12 <- 23
C12 <- 42

##Node 1
((A11+B11+C11)/(((A11+B11+C11)+(A12+B12+C12)))*
(0.4297521*log(0.4297521)+0.4586777*log(0.4586777)+0.1115702*log(0.1115702))+
##Node 2
(A12+B12+C12)/(((A11+B11+C11)+(A12+B12+C12)))*
(0.5547945*log(0.5547945)+0.1575342*log(0.1575342)+0.2876712*log(0.2876712)))/(-6)
#Model 2
C21 <- 1
C22 <- 68
#Node 1
((A11+B11+C21)/(((A11+B11+C21)+(A12+B12+C22)))*
(0.00462963*log(0.00462963)+0.5138889*log(0.5138889)+0.4814815*log(0.4814815))+
#Node 2
(A12+B12+C22)/(((A11+B11+C21)+(A12+B12+C22)))*
(0.4709302*log(0.4709302)+0.1337209*log(0.1337209)+0.3953488*log(0.3953488)))/(-6)