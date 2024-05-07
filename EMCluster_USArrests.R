arrestsScaled <- scale(USArrests)
pca_out <- prcomp(arrestsScaled)
km_out <- kmeans(pca_out$x, centers = 4, nstart = 20)
plot(pca_out$x[, 1:2], type="n")
text(pca_out$x[,1], pca_out$x[,2], labels=state.abb, col=km_out$cluster)

library(EMCluster)
em_obj<- init.EM(USArrests, nclass = 4)
em_cl_obj <- emcluster(USArrests, em_obj, assign.class = TRUE)
em_cl_probs <- e.step(USArrests, emobj = em_cl_obj)$Gamma

plot(pca_out$x[, 1:2], type="n")
text(pca_out$x[, 1:2], labels = state.abb, col= em_cl_obj$class)

addmargins(
  table(em_cl_obj$class, km_out$cluster, dnn = c("EM", "M-Means"))
)

round(em_cl_probs[km_out$cluster == 1, ], 2)
round(apply(em_cl_probs[km_out$cluster == 1, ], 2, mean), 2)

round(em_cl_probs[km_out$cluster == 2, ], 2)
round(apply(em_cl_probs[km_out$cluster == 2, ], 2, mean), 2)

round(em_cl_probs[km_out$cluster == 3, ], 2)
round(apply(em_cl_probs[km_out$cluster == 3, ], 2, mean), 2)

round(em_cl_probs[km_out$cluster == 4, ], 2)
round(apply(em_cl_probs[km_out$cluster == 4, ], 2, mean), 2)

