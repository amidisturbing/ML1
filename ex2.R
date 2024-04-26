set.seed(135792468)
x <- matrix(rnorm(75*3), ncol = 3)
x[1:25, 1] <- x[1:25, 1] + 5
x[51:75, 2] <- x[51:75, 2] - 6
truth <- rep(1:3, each = 25)
pairs(x, col = truth)
#plot3d(x, col = truth, size=1, type="p")

km_out <- kmeans(x, centers = 3, nstart = 20)

#plot3d(x, col = km_out$cluster, size = 1, type = "s")
#plot3d(km_out$centers, add=TRUE, col = 1:3, type = "s")

#Exercise 2g)
y <- matrix(rnorm(75*10), ncol = 10)
y[1:25, 1] <- y[1:25, 1] + 5
y[51:75, 2] <- y[51:75, 2] - 6
truth_2 <- rep(1:10, each = 25)
pairs(y, col = truth_2)


km_out2 <- kmeans(x, centers = 3, nstart = 20)

table(km_out$cluster, km_out2$cluster)

#2.2 USA arrests data: PCA and clustering
help(USArrests)

#2.2.1
#a) descriptive stats
#1: Murder, 2:Assault, 3: Urban Pop, 4, Rape

#b) mean, sd
mean(USArrests$Murder)
sd(USArrests$Murder)
mean(USArrests$Assault)
sd(USArrests$Assault)
mean(USArrests$UrbanPop)
sd(USArrests$UrbanPop)
mean(USArrests$Rape)
sd(USArrests$Rape)

 #2.2.2
pca_out <- prcomp(USArrests)
#d
pca_out$scale #tells me if it IS scaled (sd)
pca_out$sdev #tells me what the sd is numerically per feature
#e
#screeplot()

biplot(pca_out, xlabs = state.abb)


#2.2.3 
#g)
km_out <- kmeans(pca_out$x, centers = 2, nstart = 20)
plot(pca_out$x[, 1:2], type="n")
text(pca_out$x[,1], pca_out$x[,2], labels=state.abb, col=km_out$cluster)
table(km_out$cluster)
#h)
wss_vec <- rep(NA,10)
for(k in 1:10){
  km_out <- kmeans(pca_out$x, centers=k, nstart=20)
  wss_vec[k] <- km_out$withinss[k]
}
plot(wss_vec, type = "b") 
#AW: k = 2
#i) 
pairs(pca_out$x, col=truth)


#j) START-------------------------------------
#g)
#h)
wss_vec <- rep(NA,10)
for(k in 1:10){
  km_out <- kmeans(USArrests, centers = k, nstart = 20)
  wss_vec[k] <- km_out$withinss[k]
}
plot(wss_vec, type = "b") 
#AW: k = 2
#i) 
pairs(USArrests, col=truth)
#plot(USArrests[, 1:2], type="n")
#text(USArrests[,1], pca_out$x[,2], labels=state.abb, col=km_out$cluster)
#j) END-------------------------------------