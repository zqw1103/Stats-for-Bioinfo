#1
#a
library(ALL)
data(ALL)
expr.ALL <- exprs(ALL)
ALL.fac <- as.numeric(ALL$BT %in% c("B","B1","B2","B3","B4"))+1
par(mfrow=c(1,3))
for(i in 1:3) {
  hist(expr.ALL[i,],xlab=rownames(expr.ALL)[i], freq=F,nclass=15,main= paste0('gene ', rownames(expr.ALL)[i], ' expression'))
  lines(density(expr.ALL[i,]), col='blue') 
}

#b
five.gene <- expr.ALL[1:5,]
pairs(t(five.gene), col=ALL.fac)

#c
install.packages('scatterplot3d')
require(scatterplot3d)
X1=expr.ALL[grep('^39317_at', rownames(expr.ALL)),]
X2=expr.ALL[grep('^32649_at', rownames(expr.ALL)),]
X3=expr.ALL[grep('^481_at', rownames(expr.ALL)),]
scatterplot3d(X1,X2,X3,color=ALL.fac)

#d
df.three.genes <- data.frame(X1, X2, X3)
cl.2means <- kmeans(df.three.genes, centers=2, nstart = 10)
table(ALL.fac,cl.2means$cluster)
cl.3means <- kmeans(df.three.genes, centers=3, nstart = 10)
table(ALL.fac,cl.3means$cluster)

#e
PCA <- prcomp(expr.ALL, scale = TRUE)
summary(PCA)

#f
print(t(PCA$rotation[,1:2]),digits=3)
biplot(PCA, xlim=c(-0.04,0.04), ylim=c(-0.05,0.05), cex=0.5)

#g
o <- order(PCA$x[,2])
total.row <- nrow(expr.ALL)
rownames(expr.ALL)[o[1:3]]
rownames(expr.ALL)[o[(total.row-2):(total.row)]]

#2
#a
df.iris <- iris[,1:4]
scaled.iris <- scale(df.iris)
summary(scaled.iris)
apply(scaled.iris,2,sd)^2
#b
cor(df.iris)
cor(scaled.iris)

#c
summary(prcomp(df.iris,scale=FALSE))
summary(prcomp(scaled.iris,scale=FALSE))

#e
p <- ncol(scaled.iris)
n <- nrow(scaled.iris)
nboot <- 1000
sdevs <- array(dim=c(nboot,p))
for(i in 1:nboot) {
  dat.star <- scaled.iris[sample(1:n,replace=TRUE),]
  sdevs[i,] <- prcomp(dat.star)$sdev
}
print(names(quantile(sdevs[,1],c(0.05,0.95))))
as.numeric(quantile(sdevs[,2]^2/apply(sdevs^2,1,sum),c(0.05,0.95)))









