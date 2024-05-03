#change working directory to current dir
#wd = getwd()
#wd = "/Users/rafaelaneff/Documents/2024/BHT Berlin/ML1/EX"
#setwd(wd)
#wd
## Exercise 3.1.2
library(EMCluster) # Soft CLustering
library(mvtnorm) # multi-variate normal distributions
library(ISLR2) # contains the `NCI60` data set

## Exercise 3.2.1
# data generation
set.seed(1234567890)
x <- matrix(rnorm(50 * 2), ncol = 2)
x[1:25,1] <- x[1:25,1] + 2
x[1:25,2] <- x[1:25,2] - 2

km.out <- kmeans(x, 2, nstart= 20)

par(mfrow = c(1, 2))
plot(x, col = (km.out$cluster + 1), main = "K-Means Clustering Results with K = 2", xlab = "", ylab = "", pch = 20, cex = 2)

#performing k-Means with 3 Clusters on the very same data
#since we did create the data ourselves we do know the true number 
#of clusters being 3
set.seed(4)
km.out <- kmeans(x, 3, nstart = 20)
km.out$cluster

plot(x, col = (km.out$cluster +1 ), main = "K-Means Clustering Results with K = 3", xlab= "", ylab = "", pch = 20, cex = 2)

#comparing nstart = 1 and nstart = 20
set.seed(4)
x <- matrix(rnorm(50 * 2), ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4
km.out <- kmeans(x, 3, nstart = 1)
km.out$tot.withinss
km.out <- kmeans(x, 3, nstart = 20)
km.out$tot.withinss

#Hierarchical clustering
hc.complete <- hclust(dist(x), method = "complete")
hc.average <- hclust(dist(x), method = "average")
hc.single <- hclust(dist(x), method = "single")

cutree(hc.complete, 15)
cutree(hc.average, 15)
cutree(hc.single, 15)

#scale data before clustering
xsc <- scale(x)
plot(hclust(dist(xsc), method = "complete"), main = "Hierarchical Clustering with Scaled Features")

#correlation-based distances
x <- matrix(rnorm(30 * 3), ncol = 3)
dd <- as.dist(1 - cor(t(x)))
plot(hclust(dd, method = "complete"), main = "Complete Linkage with Correlation-Based Distance", xlab = "", sub = "")

#Excercise 3.4.1. Hierarchical Clustering
#a) Calculate the Euclidean distances between each pair of points rounded to 1 decimal
#place. You can use the R function dist() to do this.
datapoints <- rbind(c(5,1), c(8,-2), c(8,1), c(9,7), c(13,-2),c(17,3), c(18,5))
round(dist(datapoints), digits = 1)

#b) Use the distance matrix to find the first cluster join. Write down the two element numbers and the distance between them.
##??? TO DO: write down on paper

#c) Using the complete linkage rule repeat step (b) to obtain the hierarchical clusters. At
#each step write down the element numbers for each element in the new cluster and
#the distance used to determine the cluster.

#d) Draw the scatter plot of the points and the dendrogram of the clustering.

