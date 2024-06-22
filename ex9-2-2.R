library(ISLR2)

set.seed(1)
train <-sample(1:nrow(Carseats), size =nrow(Carseats)/2)
test <-Carseats[-train,]
tree.carseats.full <- rpart(Sales ~ . , data = Carseats, subset = train, cp = 0) 
print(tree.carseats.full)
rpart.plot(tree.carseats.full)

cptable <- tree.carseats.full$cptable
minDeviance <- which.min(cptable[ , "xerror"])
dotted <- cptable[minDeviance, "xerror"] + cptable[minDeviance, "xstd"] 
abline(h = dotted, col = "red", lty = 2)
# which is the first row less than this value
cpPruning <- cptable[cptable[, "xerror"] < dotted,][1]
cpPruning
cptable #find cpvalue without rounding
prune.carseats <- prune(tree.carseats.full, cp = 0.033427674) 
prune.carseats
rpart.plot(prune.carseats)

#first line

Carseats[1,]
