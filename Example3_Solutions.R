# Given p-dim vector, want to calculate matrix with x, x^2, x^3 up to specified degree.
# Each each column is a particular power.
## INPUT
# x - vector
# dg - integer, maximal power
powers1 <- function(x, dg){
  pw <- matrix(x, nrow = length(x))
  prod <- x # current product
  for (i in 2:dg){
    prod <- prod * x
    pw <- cbind(pw, prod)
  }
  return(pw)
}

# Create a version of this function that only does matrix operations (matrix of powers)
powers2 <- function(x, dg){
  p = length(x)
  pw = matrix(x, nrow = p, ncol = dg)^matrix(1:dg, p, dg, byrow = T)
  return(pw)
}

# Create a version of this function that is in between: still for loop over dg, but avoids cbind
powers3 <- function(x, dg){
  pw <- matrix(x, nrow = length(x), ncol = dg)
  prod <- x # current product
  for (i in 2:dg){
    prod <- prod * x
    pw[, i] <- prod
  }
  return(pw)
}

# An example
x = runif(100000)
out1 = powers1(x, 8)
out2 = powers2(x, 8)
library(microbenchmark)
microbenchmark(
  powers1(x, 8),
  powers2(x, 8),
  powers3(x, 8),
  times = 10
)