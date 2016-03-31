rm(list=ls())
options("scipen"=999, "digits"=4)

library(orddata)
prob <- list(
  c(60,5,5,5,5,5,15)/100,
  c(55,5,5,5,5,5,20)/100,
  c(55,5,5,5,5,10,15)/100,
  c(45,5,5,5,5,5,30)/100,
  c(40,15,5,5,5,15,15)/100,
  c(30,20,15,15,15,5)/100,
  c(25,20,20,15,15,5)/100,
  c(20,25,20,15,15,5)/100,
  c(15,25,25,15,15,5)/100,
  c(10,25,30,15,15,5)/100
)
prob

#uses matrix multiplication of factor loadings to create
#a correlation matrix with a given pattern
loadings<-matrix(c(
  .4,.6, 0,
  .4,.6, 0,
  .4,.6, 0,
  .4,.6, 0,
  .4,.6, 0,
  .4,.0,.6,
  .4,.0,.6,
  .4,.0,.6,
  .4,.0,.6,
  .4,.0,.6),
  10, 3, byrow=TRUE)
loadings

cor_matrix<-loadings %*% t(loadings)
diag(cor_matrix)<-1
cor_matrix

ord<-rmvord(n = 200, probs = prob, Cor = cor_matrix)
apply(ord,2,table)

# these are the commands needed to run the archetypal analysis
library(archetypes)
aa<-stepArchetypes(ord, k=1:10, nrep=5)
screeplot(aa)
rss(aa)
aa_3<-bestModel(aa[[3]])
round(t(parameters(aa_3)),3)
aa_3_profile<-coef(aa_3)
aa_3_cluster<-max.col(aa_3_profile)
table(aa_3_cluster)

#code for K-means
kcl_3<-kmeans(ord, 3, nstart=25, iter.max=100)
table(kcl_3$cluster)
t(kcl_3$centers)

#profiles for K-means and archetypes added to original data
# as supplementary points in order to map the results in a
# two-dimensional principal component space
aa_profile<-parameters(aa_3)
kcl_profile<-kcl_3$centers
ord2<-rbind(ord,aa_profile,kcl_profile)
row.names(ord2)<-NULL

#ease package to use with supplementary points
library(FactoMineR)
pca<-PCA(ord2, ind.sup=201:206, graph=FALSE)

#plots K-means
plot(pca$ind$coord[,1:2], type="n", xlim=c(-3.9,5.8), ylim=c(-3.9,5.8))
text(pca$ind$coord[,1:2], col=kcl_3$cluster, labels=kcl_3$cluster)
arrows(0, 0, 7*pca$var$coord[,1], 7*pca$var$coord[,2], col = "chocolate", angle = 15, length = 0.1)
#text(7*axes$loadings[,1], 7*axes$loadings[,2], labels=1:10)

#plots archetypes
plot(pca$ind$coord[,1:2], type="n", xlim=c(-3.9,5.8), ylim=c(-3.9,5.8))
text(pca$ind$coord[,1:2], col=aa_3_cluster, labels=aa_3_cluster)
arrows(0, 0, 7*pca$var$coord[,1], 7*pca$var$coord[,2], col = "chocolate", angle = 15, length = 0.1)
#text(7*axes$loadings[,1], 7*axes$loadings[,2], labels=1:10)

#plots K-means centroids and archetype profiles
plot(pca$ind$coord[,1:2], pch=".", cex=4, xlim=c(-3.9,5.8), ylim=c(-3.9,5.8))
text(pca$ind.sup$coord[,1:2], labels=c("A1","A2","A3","K1","K2","K3"), col=c(rep("blue",3),rep("red",3)),cex=1.5)