#1
#a
library(multtest)
data("golub")
go.fac <- factor(golub.cl, 0:1, c('ALL', 'AML'))
pval <- apply(golub, 1, function(x) wilcox.test(x ~ go.fac, paried = F, alternative = 'greater')$p.value)
p.fdr <- p.adjust(pval, method = 'fdr')
sum(p.fdr < 0.05)

#b
idx <- order(pval, decreasing = FALSE)[1:3]
golub.gnames[idx,2]
idx2 <- order(abs(apply(golub[, go.fac=='ALL'], 1, mean) - apply(golub[, go.fac=='AML'], 1, mean)), decreasing = TRUE)[1:3]
golub.gnames[idx2, 2]

#2
go.fac <- factor(golub.cl, 0:1, c('ALL', 'AML'))
pval2 <- apply(golub[, go.fac=='AML'], 1, function(x) shapiro.test(x)$p.value)
p.fdr2 <- p.adjust(pval2, method = 'fdr')
sum(p.fdr2 < 0.05)

#3
go.fac <- factor(golub.cl, 0:1, c('ALL', 'AML'))
hoxa9 <- golub[grep('HOXA9 Homeo box A9', golub.gnames[,2]), go.fac=='ALL']
cd33 <- golub[grep('CD33', golub.gnames[,2]), go.fac=='ALL']
wilcox.test(hoxa9, cd33, paired = T)

#4
data(UCBAdmissions)
str(UCBAdmissions)
apply(UCBAdmissions, 3, function(x) fisher.test(x)$p.value)

#5
go.fac <- factor(golub.cl, 0:1, c('ALL', 'AML'))
data <- golub[grep('CD33', golub.gnames[,2]),]
T.obs <- var(data[go.fac == "ALL"])/var(data[go.fac == "AML"])
n <- length(data)
n.perm = 2000
T.perm <- c(NA, n.perm)
for (i in 1:n.perm) {
  data.perm <- sample(data, n, replace = F)
  T.perm[i] <- var(data.perm[go.fac == "ALL"])/var(data.perm[go.fac == "AML"])
}
mean(T.perm >= T.obs)





