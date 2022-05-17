#Problem 2
sq.sum <- function(x) {
  y <- c()
  for (i in 1:x) {
    y[i] <- i^2
    sum.sq <- sum(y)
  }
  return(sum.sq)
}

sq.sum(1000)

#Problem 3
X <- seq(1,20) *2
X

Y <- rep(0,20)
Y

for (k in 1:20) {
  if (k<12) {
    Y[k] <- cos(3*k)
  }else{
    Y[k] <- integrate(sqrt, lower=0, upper=k)$value
  }
}
unlist(Y)