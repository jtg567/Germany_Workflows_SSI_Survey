rm(list=ls())
options("scipen"=999, "digits"=4)

# import, check classes (already manually find/replaced -- for NA in excel prior to this import)
d.in <- read.csv("C:\\Users\\Josh Gaunt\\Documents\\R\\Germany Workflows SSI Survey-Cleaned\\Germany Workflows SSI Survey-Cleaned - 20160203142449-SurveyExport cleaned.csv", stringsAsFactors = FALSE)

# drop fluff from task response names
t.names <- names(d.in[,1:27])
t.names <- gsub(".Did.you.do.you.do.the.following.on.the.Internet.in.the.last.week.", "", t.names, perl = TRUE)
t.names <- append(t.names, c('GOOD_EVERYDAY', 'NO_WEB_TOMORR', 'PRIVACY', 'NO_DO_WEB', 'START_YEAR', 
                             'BRWSR_MAIN', 'BRWSR_MAINo', 'BRWSR_OTHER',  'BRWSR_OTHERo', 'BRWSR_MOBILE_MAIN', 'BRWSR_MOBILE_MAINo', 'BRWSR_MOBILE_OTHER', 'BRWSR_MOBILE_OTHERo',
                             'GENDER', 'AGE', 'COMM', 'INCOME', 'EDU'))
names(d.in) <- t.names

# manually set datatypes (sigh)
for(i in  1:27) {d.in[,i] <- as.integer(d.in[,i])}
for(i in 28:45) {d.in[,i] <-     factor(d.in[,i])}

d.in <- na.omit(d.in)

# these are the commands needed to run the archetypal analysis
library(archetypes)
aa<-stepArchetypes(d.in[,2:27], k=1:10, nrep=5)
screeplot(aa)
rss(aa)
aa_3<-bestModel(aa[[3]])
round(t(parameters(aa_3)),3)
aa_3_profile<-coef(aa_3)
aa_3_cluster<-max.col(aa_3_profile)
table(aa_3_cluster)

#code for K-means
kcl_3<-kmeans(d.in[,2:27], 3, nstart=25, iter.max=100)
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