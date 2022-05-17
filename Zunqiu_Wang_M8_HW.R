#1
#a 
library(ALL)
data(ALL)
ALL
ALL109 <- ALL[, ALL$BT %in% c('B', 'B1','B2', 'B3', 'B4')]
y <- exprs(ALL109)['109_at',]
summary(lm(y ~ ALL109$BT))

#b
summary(lm(y ~ ALL109$BT -1))

#c
pairwise.t.test(y, ALL109$BT)

#d
pairwise.t.test(y, ALL109$BT, p.adjust.method = 'fdr')

#e
library(lmtest)
shapiro.test(residuals(lm(y ~ ALL109$BT)))
bptest(lm(y ~ ALL109$BT), studentize = F)

#2
#a
ALLB <- ALL[, ALL$BT %in% c('B', 'B1','B2', 'B3', 'B4')]
y <- exprs(ALLB)
p.val <- apply(y,1, function(x) kruskal.test(x ~ ALLB$BT)$p.value)
p.fdr <- p.adjust(p.val, 'fdr')
sum(p.fdr < 0.05)

#b
names(sort(p.fdr)[1:5])

#3
#a
ALLBg <- ALL[,which(ALL$BT %in% c("B1","B2","B3","B4") & ALL$sex %in% c('M', 'F'))]
y <- exprs(ALLBg)['38555_at',]
Bcell <- ALLBg$BT
gender <- ALLBg$sex
anova(lm(y ~ Bcell*gender))

#b
shapiro.test(residuals(lm(y ~ Bcell*gender)))
bptest(lm(y ~ Bcell*gender), studentize = F)




