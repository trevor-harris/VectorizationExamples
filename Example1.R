# Squaring elements of a given vector

square_for <- function(x){
  # [ToDo] Use the for loop
  p = length(x) # get length
  y = rep(NA, p) # allocate space for new vector
  for (i in 1:p){
    y[i] = x[i]^2 # square each individual element
  }
 return(y)
}

square_sapply <- function(x){
  # [ToDo] Use the sapply function
  y = sapply(x, function(z) z^2)
  return(y)
}

square_vec <- function(x){
  # [ToDo] Use power(^) function in vector form
  y = x^2
  return(y)
}

square_vec2 <- function(x){
  # [ToDo] Use multiplication(*) function in vector form
  y = x * x
  return(y)
}

# [ToDo] Create a vector x of size 100,000 of normal variables
x = rnorm(100000)


# [ToDo] Verify that all functions return the same output
y1 = square_for(x)
y2 = square_sapply(x)
y3 = square_vec(x)
y4 = square_vec2(x)
# Make sure the outputs agree
sum(abs(y1 - y2))
sum(abs(y2 - y3))
sum(abs(y3 - y4))


# [ToDo] Use microbenchmark package to compare three functions in terms of speed
library(microbenchmark)
microbenchmark(
  square_for(x),
  square_sapply(x),
  square_vec(x),
  square_vec2(x),
  times = 50
)