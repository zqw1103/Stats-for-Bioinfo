#1
library(multtest)
data(golub)
gol.fac <- factor(golub.cl, levels = c(0,1), labels = c('ALL', 'AML'))
h4j.ALL <- golub[2972, gol.fac == 'ALL']
h4j.AML <- golub[2972, gol.fac == 'AML']
aps.ALL <- golub[2989, gol.fac == 'ALL']
aps.AML <- golub[2989, gol.fac == 'AML']

#a
t.test(h4j.ALL, mu=-0.9, alternative = "greater")

#b
t.test(h4j.ALL, h4j.AML)

#c
t.test(h4j.ALL, aps.ALL, alternative = 'less')

#d
x <- length(h4j.ALL[h4j.ALL>-0.6])
n <- length(h4j.ALL)
binom.test(x,n,0.5, alternative = 'less')

#e
x.all <- length(h4j.ALL[h4j.ALL>-0.6])
n.all <- length(h4j.ALL)
x.aml <- length(h4j.AML[h4j.AML>-0.6])
n.aml <- length(h4j.AML)
prop.test(c(x.all,x.aml), c(n.all,n.aml), alternative = 'two.sided')

#2
pbinom(89, 2000, 0.05)

#3
x.sim <- matrix(rnorm(10000*20, mean = 3),ncol = 20)
tstat <- function(x) (mean(x)-3)/sd(x)*sqrt(length(x))
tstat.sim <- apply(x.sim, 1, tstat)
power.sim <- mean(tstat.sim > qt(0.3, 19) & tstat.sim < qt(0.4, 19))
power.sim + c(-1,0,1) * qnorm(1-0.1/2) * sqrt(power.sim * (1-power.sim)/10000)

#4
#a
gol.fac <- factor(golub.cl, levels = c(0,1), labels = c('ALL', 'AML'))
p.val <- apply(golub, 1, function(x) t.test(x ~ gol.fac)$p.value)
p.bon <- p.adjust(p=p.val, method = 'bonferroni')
p.fdr <- p.adjust(p=p.val, method = 'fdr')
sum(p.bon<0.05)
sum(p.fdr<0.05)

#b
sorted.p.val <- order(p.val)
golub.gnames[sorted.p.val[1:3],2]





