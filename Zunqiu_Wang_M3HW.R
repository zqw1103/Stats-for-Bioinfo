#2
f <- function(x) {
  ((2^x)*exp(-2)) / factorial(x)
}
f(1)

f(0) + f(1) + f(2) + f(3)

#4
rm(list=ls())
pbinom(2, 3, 0.25)

Y.range <- (0:3)
EY <- sprintf(sum(Y.range*dbinom(Y.range, 3, 0.25)), fmt = '%#.4f')
EY

VarY <- sum((Y.range-EY)^2*dbinom(Y.range, 3, 0.25))
VarY

#5
rm(list=ls())
pchisq(4, 3) - pchisq(1, 3)
integrate(function(x) dchisq(x, 3), lower = 1, upper = 4)$value

x <- rchisq(100000, 3)
mean((x>1) & (x<4))

#7
rm(list=ls())
#a
pnorm(1.6,1.6,0.4) - pnorm(1,1.6,0.4)

#b
x <- rnorm(500000,1.6,0.4)
mean((x>1) & (x<1.6))

#c
dbinom(2,5,(pnorm(1.6,1.6,0.4) - pnorm(1,1.6,0.4)))

#8
rm(list=ls())
#a
EX1 <- integrate(function(x) x*df(x,2,5),lower=0,upper=Inf)$value
EX1
V1 <- integrate(function(x)(x-EX1)^2*df(x,2,5), lower = 0, upper = Inf)$value
V1

EX2 <- integrate(function(x) x*df(x,10,5),lower=0,upper=Inf)$value
EX2
V2 <- integrate(function(x)(x-EX2)^2*df(x,10,5), lower = 0, upper = Inf)$value
V2




