#1
obs <- c(1.636, 0.374, 0.534, 3.015, 0.932, 0.179)
nloglik <- function(lam) {
  -sum(log(dexp(obs,rate=lam)))
}
optimize(nloglik, c(0,6))$minimum

#2
#(b)
qt(0.1,52)
100.8+qt(0.1,52)*12.4/sqrt(53)

#3
#(a)
library(multtest)
data("golub")
grep('Zyxin',golub.gnames[,2])
gol.fac <- factor(golub.cl, levels = 0:1, labels = c('ALL','AML'))
data_ALL <- golub[2124, gol.fac=='ALL']     
data_AML <- golub[2124, gol.fac=='AML']  
nboot <- 1000
n_ALL <- length(data_ALL)
n_AML <- length(data_AML)
#Bootstrap 95% CI for ALL mean
xbar_ALL <- rep(NA, nboot)
for (i in 1:1000) {
  data.star <- data_ALL[sample(1:n_ALL, replace = TRUE)]
  xbar_ALL[i] <- mean(data.star)
}
quantile(xbar_ALL, c(0.025,0.975))

#Bootstrap 95% CI for AML mean
xbar_AML <- rep(NA, nboot)
for (i in 1:1000) {
  data.star <- data_AML[sample(1:n_AML, replace = TRUE)]
  xbar_AML[i] <- mean(data.star)
}
quantile(xbar_AML, c(0.025,0.975))

#Bootstrap 95% CI for ALL variance
var_ALL <- rep(NA, nboot)
for (i in 1:1000) {
  data.star <- data_ALL[sample(1:n_ALL, replace = TRUE)]
  var_ALL[i] <- var(data.star)
}
quantile(var_ALL, c(0.025,0.975))

#Bootstrap 95% CI for AML variance
var_AML <- rep(NA, nboot)
for (i in 1:1000) {
  data.star <- data_AML[sample(1:n_AML, replace = TRUE)]
  var_AML[i] <- var(data.star)
}
quantile(var_AML, c(0.025,0.975))

#b
#Parametric 95% CI for ALL mean
mean(data_ALL) + qt(c(0.025,0.975), df=n_ALL-1) * sd(data_ALL)/sqrt(n_ALL)
#Parametric 95% CI for AML mean
mean(data_AML) + qt(c(0.025,0.975), df=n_AML-1) * sd(data_AML)/sqrt(n_AML)
#Parametric 95% CI for ALL variance
(n_ALL-1)*var(data_ALL)/qchisq(c(0.975,0.025),df=n_ALL-1)
#Parametric 95% CI for AML variance
(n_AML-1)*var(data_AML)/qchisq(c(0.975,0.025),df=n_AML-1)

#c
#Bootstrap 95% CI for ALL median
med_ALL <- rep(NA, nboot)
for (i in 1:1000) {
  data.star <- data_ALL[sample(1:n_ALL, replace = TRUE)]
  med_ALL[i] <- median(data.star)
}
quantile(med_ALL, c(0.025,0.975))

#Bootstrap 95% CI for AML median
med_AML <- rep(NA, nboot)
for (i in 1:1000) {
  data.star <- data_AML[sample(1:n_AML, replace = TRUE)]
  med_AML[i] <- median(data.star)
}
quantile(med_AML, c(0.025,0.975))

#4
#a
MCsim <- function(nsim, lambda) {
  
  cov1<-rep(NA,nsim)
  cov2<-rep(NA,nsim) 
  
  for (i in 1:nsim) {
    
    x= rpois(50, lambda) 
    
    xbar=mean(x)
    
    Xsd=sd(x)
    
    CI1 <- xbar + qt(c(0.025,0.975),49) * sqrt(xbar/50)
    
    CI2 <- 49*Xsd^2/qchisq(c(0.95,0.05),49)
    
    cov1[i] <- (CI1[1] < lambda)&(lambda < CI1[2])
    
    cov2[i] <- (CI2[1] < lambda)&(lambda < CI2[2])
    
  }
  
  print(paste("When lambda=", lambda, ": coverage for first CI is", mean(cov1), ", coverage for second CI is", mean(cov2), "."))
  
}

#b
MCsim(1000,0.1)
MCsim(1000, 1)
MCsim(1000, 10)



