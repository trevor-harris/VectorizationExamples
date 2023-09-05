
# Function that returns t-test statistic for each gene
## INPUT
# X - n x p data matrix
# group - class assignment for the t.test, either 1 or 2
computeT_for <- function(X, group){
  Tstats <- rep(0, p)
  for (j in 1:p){
    Tstats[j] <- t.test(X[, j] ~ group)$stat
  }
  return(Tstats)
}

# Function that returns t-test statistic for each gene, slight modification
## INPUT
# X - n x p data matrix
# group - class assignment for the t.test, either 1 or 2
computeT_for2 <- function(X, group){
  Tstats <- rep(0, p)
  for (j in 1:p){
    Tstats[j] <- t.test(X[group == 1, j], X[group == 2, j])$stat
  }
  return(Tstats)
}

# [ToDo] Function that returns t-test statistic for each gene, explicit calculations
computeT_for3 <- function(X, group){
  n1 = sum(group == 1)
  n2 = sum(group == 2)
  p = ncol(X)
  Tstats = rep(0, p)
  for (j in 1:p){
    m1 = mean(X[group == 1, j])
    m2 = mean(X[group == 2, j])
    var1 = var(X[group == 1, j])
    var2 = var(X[group == 2, j])
    Tstats[j] = (m1 - m2) / sqrt(var1/n1 + var2/n2)
  }
  return(Tstats)
}

# Function that returns t-test statistic for each gene, [ToDo] vectorize the code
computeT_vec <- function(X, group){
  # Get sample sizes
  n1 = sum(group == 1)
  n2 = sum(group == 2)
  p = ncol(X)
  
  # Get the group means
  m1 = colMeans(X[group == 1, , drop = FALSE])
  m2 = colMeans(X[group == 2, , drop = FALSE])
  
  # [ToDo] Fill in the variance calculations and the final statistic calculation
  # Var(X_j) = 1/(n-1)sum_{i=1}^n (X_{ji} - mean(X_j))^2
  # Group 1
  var1 = colSums((X[group == 1, ] - matrix(m1, n1, p, byrow = T))^2)/(n1 - 1)
  # Group 2
  var2 = colSums((X[group == 2, ] - matrix(m2, n2, p, byrow = T))^2)/(n2 - 1)  
    # Calculate Tstats
    Tstats = (m1 - m2) / sqrt(var1/n1 + var2/n2)
  return(Tstats)
}

######################################################################
# Generate some example data to perform t-test
p <- 1000
n <- 50
set.seed(03875)
X <- matrix(rnorm(n * p, mean = 10, sd = 3), n, p)
group <- rep(1:2, each = n/2)


# Do the microbenchmark comparisons
library(microbenchmark)
microbenchmark(
  computeT_for(X, group), 
  computeT_for2(X, group),
  computeT_for3(X, group),
  computeT_vec(X, group),
  times = 10
)


