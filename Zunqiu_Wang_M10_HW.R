#1
#a
library(multtest)
data(golub)
df <- data.frame(golub[1042,])
colnames(df) <- c('CCND3 Cyclin D3')
go.fac <- factor(golub.cl,levels = 0:1, labels = c('ALL', 'AML'))
hc.single <- hclust(dist(df,method = 'euclidian'), method = 'single')
par(mfrow=c(1,1))
plot(hc.single, labels = go.fac)
grp.single <- cutree(hc.single, k=2)
table(go.fac, grp.single)

hc.ward <- hclust(dist(df,method = 'euclidian'), method = 'ward.D2')
par(mfrow=c(1,1))
plot(hc.ward, labels = go.fac)
grp.ward <- cutree(hc.ward, k=2)
table(go.fac, grp.ward)

#b
cl.2means <- kmeans(df, centers = 2, nstart = 10)
table(go.fac, cl.2means$cluster)

#d
initial <- cl.2means$centers
n <- dim(df)[1]
nboot <- 1000
boot.cl <- matrix(NA,nrow=nboot, ncol = 2)
for(i in 1:nboot) {
  dat.star <- df[sample(1:n, replace = TRUE),]
  cl <- kmeans(dat.star, centers=initial)
  boot.cl[i,] <- c(cl$centers[,1])
}
apply(boot.cl,2,mean)
quantile(boot.cl[,1], c(0.025,0.975))
quantile(boot.cl[,2], c(0.025,0.975))

#e
K <- 1:30
sse <- rep(NA, length(K))
for (k in K) {
  sse[k] <- kmeans(df, centers=k, nstart = 10)$tot.withinss
}
plot(K, sse, type='o', xaxt='n')
axis(1,at=K,las=2)

#2
rm(list=ls())
data(golub)
library(cluster)
sel1 <- grep('oncogene', golub.gnames[,2])
sel2 <- grep('antigen', golub.gnames[,2])
clusdata <- rbind(golub[sel1,], golub[sel2,])
g.name <- rep(c('oncogene', 'antigen'), c(length(sel1), length(sel2)))

#b
cl.2means <- kmeans(clusdata, center=2, nstart=10)
mean.table <- table(g.name, cl.2means$cluster)

cl.2medoids <- pam(dist(clusdata,method = 'eucl'), k=2)
medoid.table <- table(g.name, cl.2medoids$cluster)

#c
fisher.test(mean.table)

fisher.test(medoid.table)

#d
hc.single <- hclust(dist(clusdata, method = 'euclidian'), method = 'single')
plot(hc.single, labels = g.name, hang=-1)
rect.hclust(hc.single, k=2)

hc.complete <- hclust(dist(clusdata, method = 'euclidian'), method = 'complete')
plot(hc.complete, labels = g.name, hang=-1)
rect.hclust(hc.complete, k=2)

#3
install.packages('ISLR')
library(ISLR)
ncidata<-NCI60$data
ncilabs<-NCI60$labs

#a
K <- 1:30
sse <- rep(NA, length(K))
for (k in K) {
  sse[k] <- kmeans(ncidata, centers=k, nstart = 10)$tot.withinss
}
plot(K, sse, type='o', xaxt='n')
axis(1,at=K,las=2)

#b
cl.medoid <- pam(as.dist(1-cor(t(ncidata))),k=7)
table(ncilabs, cl.medoid$cluster)
























