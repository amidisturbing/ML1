#==============================================================================#
# EM Clustering by Hand                            #
# Machine Learning I - SoSe24                         #
#==============================================================================#
rm(list = ls(all.names = TRUE)) 



# 00: load packages -----------------------------------------------------------
library("mvtnorm")
library("EMCluster")

# 01: data generation ---------------------------------------------------------

# define the data which contains two clusters 
set.seed(13353) 

# first cluster
x11 <- rnorm(25, 0.3, 0.1)
x12 <- rnorm(25, 0.3, 0.1) - 0.4 * x11

# second cluster
x21 <- rnorm(25, 0.4, 0.1)
x22 <- rnorm(25, 0.4, 0.1) + 0.5 * x21

# cbind into a matrix
Xdata <- cbind(c(x11, x21), c(x12, x22))

# And we know which cluster they were in
truth <- rep(c(1, 2), c(25, 25))

# plot the data
plot(c(x11, x21), c(x12, x22), pch = truth, col = truth)
round(dist(Xdata), digits = 1)



# 02: Simple kmeans clustering with two clusters -------------------------------

km.out <- kmeans(scale(Xdata), centers = 2, nstart = 20)
plot(Xdata[, 1], Xdata[, 2], col = km.out$cluster, pch=truth)
table(km.out$cluster, truth)

# The triangles on the left side of the upper cluster are wrongly assigned

# 03: EM Clustering ------------------------------------------------------------

# initialise a matrix to contain the cluster allocation probabilities
probs <- matrix(NA, 50, 2)

# Initialise cluster centroids
EMmeans11 <- 0.2  # cluster 1 dimension 1
EMmeans21 <- 0.45 # cluster 2 dimension 1
EMmeans12 <- 0.3  # cluster 1 dimension 2
EMmeans22 <- 0.3  # cluster 2 dimension 2

# Plot the initial centroids 
plot(c(x11, x21), c(x12, x22), pch = 1)
points(EMmeans11, EMmeans12, pch = "1")
points(EMmeans21, EMmeans22, pch = "2")

# Initialise cluster covariances as a the global variance 
# times the identity matrix.
Sigma1 <- cov(Xdata)
Sigma2 <- cov(Xdata)

# 03a: Iteration starts from here ----------------------------------------------

# E step (estimation) -----
# calculate the cluster allocation probabilities

# density in clust 1
tprob1 <- dmvnorm(Xdata, mean = c(EMmeans11, EMmeans12), sigma = Sigma1) 
# density in clust 2
tprob2 <- dmvnorm(Xdata, mean = c(EMmeans21, EMmeans22), sigma = Sigma2) #density in clust 2
# density proportion 
probs[, 1] <- tprob1/(tprob1 + tprob2) 
probs[, 2] <- 1 - probs[, 1]


# Histogram of the probability the element is in clust 1
hist(probs[, 1], breaks=20)


# M step (MLE) -----
# update the weighted means in each group
EMmeans11 <- weighted.mean(Xdata[, 1], w = probs[, 1])
EMmeans21 <- weighted.mean(Xdata[, 1], w = probs[, 2])
EMmeans12 <- weighted.mean(Xdata[, 2], w = probs[, 1])
EMmeans22 <- weighted.mean(Xdata[, 2], w = probs[, 2])

# update the weighted coovariances in each group
Sigma1 <- cov.wt(Xdata, wt = probs[, 1])$cov
Sigma2 <- cov.wt(Xdata, wt = probs[, 2])$cov

# plot the updated centroids grey points are uncertain
# the darker the color the higher the probability for cluster ???
plot(c(x11, x21), c(x12, x22), pch=16, col=grey(probs[, 1]), main = "Iterion 1")
points(c(x11, x21), c(x12, x22), pch=1)
points(EMmeans11, EMmeans12, pch = "1", col = "red", cex = 1.5)
points(EMmeans21, EMmeans22, pch = "2", col = "red", cex = 1.5)

# more iterations by hand -----
# just repeat the EM steps

# looping: -----
# use an appropriate looping structure for 100 iterations

#???TODO: add for-loop? 
for(k in 1:100){
  # 03a: Iteration starts from here ----------------------------------------------
  
  # E step (estimation) -----
  # calculate the cluster allocation probabilities
  
  # density in clust 1
  tprob1 <- dmvnorm(Xdata, mean = c(EMmeans11, EMmeans12), sigma = Sigma1) 
  # density in clust 2
  tprob2 <- dmvnorm(Xdata, mean = c(EMmeans21, EMmeans22), sigma = Sigma2) #density in clust 2
  # density proportion 
  probs[, 1] <- tprob1/(tprob1 + tprob2) 
  probs[, 2] <- 1 - probs[, 1]
  
  
  # Histogram of the probability the element is in clust 1
  hist(probs[, 1], breaks=20)
  
  
  # M step (MLE) -----
  # update the weighted means in each group
  EMmeans11 <- weighted.mean(Xdata[, 1], w = probs[, 1])
  EMmeans21 <- weighted.mean(Xdata[, 1], w = probs[, 2])
  EMmeans12 <- weighted.mean(Xdata[, 2], w = probs[, 1])
  EMmeans22 <- weighted.mean(Xdata[, 2], w = probs[, 2])
  
  # update the weighted coovariances in each group
  Sigma1 <- cov.wt(Xdata, wt = probs[, 1])$cov
  Sigma2 <- cov.wt(Xdata, wt = probs[, 2])$cov
  
  # plot the updated centroids grey points are uncertain
  # the darker the color the higher the probability for cluster ???
  plot(c(x11, x21), c(x12, x22), pch=16, col=grey(probs[, 1]), main = paste("Iterion ",k))
  points(c(x11, x21), c(x12, x22), pch=1)
  points(EMmeans11, EMmeans12, pch = "1", col = "red", cex = 1.5)
  points(EMmeans21, EMmeans22, pch = "2", col = "red", cex = 1.5)
}
# 04: EMcluster -Package -------------------------------------------------------

# Now use the emcluster package to do the same
library(EMCluster)
emobj <- init.EM(Xdata, nclass = 2)
emclobj <- emcluster(Xdata, emobj, assign.class = TRUE)
emprobs <- round(e.step(Xdata, emobj = emclobj)$Gamma, 3)

hist(emprobs[,1], breaks=20)
hist(emprobs[,2], breaks=20)
plotem(emclobj, Xdata, lwd=2)


plot(c(x11,x21),c(x12,x22),
     pch=16, col=grey(emprobs[,1]))
points(c(x11,x21),c(x12,x22),pch=1)

# compare the hard clusters with the true cluster values.
table(???$class, truth)