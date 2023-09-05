# Squaring elements of a given vector

square_for <- function(x){
  # [ToDo] Use the for loop
  y = x
  p = length(x)
  for (i in 1:p){
    # square each element individually
    y[i] = x[i]^2
  }
  return(y)
}

square_sapply <- function(x){
  # [ToDo] Use the sapply function
  sapply(x, function(z) z^2)
}

square_vec <- function(x){
  # [ToDo] Use power(^) function in vector form
  x^2
}

square_vec2 <- function(x){
  # [ToDo] Use multiplication(*) function in vector form
  x * x
}

# [ToDo] Create a vector x of size 100,000 of normal variables

x <- rnorm(100000)

# [ToDo] Verify that all functions return the same output
y1 = square_vec(x)
y2 = square_sapply(x)
sum(abs(y1 - y2))

# [ToDo] Use microbenchmark package to compare three functions in terms of speed
library(microbenchmark)
microbenchmark(
  square_for(x),
  square_sapply(x),
  square_vec(x),
  square_vec2(x),
  times = 10
)