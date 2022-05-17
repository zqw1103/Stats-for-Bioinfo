#1
mean <- 20*0.7
std <- sqrt(20*0.7*0.3/100)
1- pnorm(15,14,0.2049)
#2
require(mvtnorm)

nsim=10000 
  
XmeanLess.sim<- rep(NA,nsim)

for (i in 1:nsim) {
  
  data.sim<-rmvnorm(50,c(9,10),matrix(c(3,2,2,5),2)) 
  
  mean.sim<-apply(data.sim,1,mean) 
  
  Xmean<-mean.sim[1]
  
  Ymean<-mean.sim[2]
  
  XmeanLess.sim[i] <- (Xmean+0.5<Ymean) 
  
}

mean(XmeanLess.sim) 

mean(XmeanLess.sim) + c(-1,1)*1.96*sqrt(var(XmeanLess.sim)/10000) 

#3
X1 <- rchisq(10000,10)
X2 <- rgamma(10000,1,2)
X3 <- rt(10000,3)
y <- sqrt(X1)*X2+4*(X3)^2
mean(y)

#4
rm(list = ls())
n <- 1000
an <- sqrt(2*log(n)) - 0.5*(log(log(n))+log(4*pi))*(2*log(n))^(-1/2) 
bn <- (2*log(n))^(-1/2)
sub_max <- c()
for (i in 1:1000) {
  sub_max[i] <- (max(rnorm(1000))-an)/bn
}
d <- density(sub_max)

plot(d, ylim=c(0,0.5))

f <- function(x) {
  exp(-x)*exp(-exp(-x))
}
curve(f, range(d$x), add=TRUE, col='blue')
curve(dnorm, add=TRUE, col='red')




