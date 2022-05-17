#1
#a
library(multtest)
data(golub)
gol.fac <- factor(golub.cl, levels=c(0,1), labels = c('ALL', 'AML'))
meanALL <- apply(golub[,gol.fac=='ALL'], 1, mean)

#b
meanAML <- apply(golub[,gol.fac=='AML'], 1, mean)

#c
sort.meanALL <- sort(meanALL, decreasing = TRUE)
idx.ALL <- match(sort.meanALL[1:3], meanALL)
golub.gnames[idx.ALL,2]

#d
sort.meanAML <- sort(meanAML, decreasing = TRUE)
idx.AML <- match(sort.meanAML[1:3], meanAML)
golub.gnames[idx.AML,2]

#2
rm(list = ls())
#a
data(golub)
gol.fac <- factor(golub.cl, levels=c(0,1), labels = c('ALL', 'AML'))
AML5 <- golub[1:5,gol.fac=='AML']
write.csv(AML5, "AML5.csv")

#b
ALL5 <- golub[1:5,gol.fac=='ALL']
write.table(ALL5, "ALL5.txt")

#c
p_101 <- golub[100:200, 1]
sd(p_101)

#d
sd_gene <- apply(golub, 1, sd)
sum(sd_gene > 1)

#e
plot(golub[101,], golub[102,], xlab=golub.gnames[101,2], ylab=golub.gnames[102,2])

#3
rm(list = ls())
#a
BiocManager::install("ALL")
library(ALL)
data(ALL)
str(ALL)
openVignette("ALL")
pData(ALL)
summary(pData((ALL)))

b1 <- exprs(ALL[,ALL$BT=="B1"])
hist(b1)

#b
mean_b1 <- apply(b1, 1, mean)
mean_b1

#c
mean.b1.sort <- sort(mean_b1, decreasing = TRUE)
idx.mean.b1 <- match(mean.b1.sort, mean_b1)
gene.id <- mean_b1[idx.mean.b1[1:3]]
gene.id

#4
rm(list = ls())
#a
data(trees)
typeof(trees)

#b
girth <- trees[, 1]
height <- trees[, 2]
volume <- trees[, 3]
plot(girth, height, col = "blue", pch = 3, ylab = "height or volume", ylim = c(5,100))
points(girth, volume, col = "red", pch =1)