#1
#a
library(multtest)
data(golub)
GRO2 <- golub[grep('GRO2 GRO2 oncogene', golub.gnames[,2]),]
GRO3 <- golub[grep('GRO3 GRO3 oncogene', golub.gnames[,2]),]
cor(GRO2,GRO3)

#b
cor.test(GRO2, GRO3, conf.level = 0.9)

#c
nboot <- 2000
boot.cor <- matrix(0,nrow=nboot, ncol=1)
data <- cbind(GRO2,GRO3)
for(i in 1:nboot) {
  dat.star <- data[sample(1:nrow(data), replace = T),]
  boot.cor[i,] <- cor(dat.star[,1], dat.star[,2])
}
quantile(boot.cor[,1],c(0.05, 0.95))

#2
#a
rm(list=ls())

data(golub)
zyxin <- golub[grep('Zyxin', golub.gnames[,2]),]
cor.result <- apply(golub,1, function(x) cor(zyxin,x))
sum(cor.result < -0.5)

#b
golub.gnames[,2][order(cor.result)[1:5]]

#c
pval <- apply(golub,1, function(x) cor.test(zyxin,x, alternative = 'less')$p.value)
p.fdr <- p.adjust(pval, method = 'fdr')
sum(p.fdr<0.05)

#3
#a
rm(list=ls())

data(golub)
GRO2 <- golub[grep('GRO2 GRO2 oncogene', golub.gnames[,2]),]
GRO3 <- golub[grep('GRO3 GRO3 oncogene', golub.gnames[,2]),]

reg.fit <- lm(GRO3~GRO2)
summary(reg.fit)

#b
confint(reg.fit, level=0.9)

#c
predict(reg.fit, newdata = data.frame(GRO2=0), interval = 'prediction', level = 0.8)

#d
qqnorm(resid(reg.fit))
qqline(resid(reg.fit))
shapiro.test(resid(reg.fit))

#4
#a,b
data("stackloss")
lin.reg <- lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data = stackloss)
summary(lin.reg)

#c
predict(lin.reg, newdata = data.frame(Air.Flow = 60, Water.Temp = 20, Acid.Conc. = 90), interval = "confidence", level = 0.9)
predict(lin.reg, newdata = data.frame(Air.Flow = 60, Water.Temp = 20, Acid.Conc. = 90), interval = "prediction", level = 0.9)




