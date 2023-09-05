
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

# Function that returns t-test statistic for each gene, explicit calculations
computeT_for3 <- function(X, group){
  n1 = sum(group == 1)
  n2 = sum(group == 2)
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
  n1 = sum(group == 1)
  n2 = sum(group == 2)
  m1 = colMeans(X[group == 1, , drop = F])
  m2 = colMeans(X[group == 2, , drop = F])
  
  # [ToDo] Fill in the variance calculations and the final statistic calculation
  var1 =
    var2 = 
    
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



# Calculate variance for each column of Z
var_calculation1 <- function(Z){
  fullcov <- var(Z) # calculates full p x p covariance
  return(diag(fullcov)) # takes only diagonal elements
}


# Calculate variance for each column of Z
# variance for jth column: sum((Z_j - m_j)^2)/(n-1)

var_calculation2 <- function(Z){
  m = colMeans(Z) # get means first
  n = nrow(Z)
  p = ncol(Z)
  center = Z - matrix(m, n, p, byrow = T) # centering
  variances = colSums(center^2) / (n - 1) # variance calculation
}

Z <- matrix(rnorm(30 * 1000), 30, 1000)
sum(abs(var_calculation1(Z) - var_calculation2(Z)))

# Use microbenchmark to compare timing of different functions
library(microbenchmark)
microbenchmark(
  var_calculation1(Z),
  var_calculation2(Z),
  apply(Z, 2, var),
  times = 10
)


Rprof()
invisible(computeT_for(X, group))
Rprof(NULL)
summaryRprof()



Tstats <- rep(0, p)
for (j in 1:p){
  Tstats[j] <- t.test(X[, j] ~ group)$stat
}


Tstats <- rep(0, p)
for (j in 1:p){
  Tstats[j] <- t.test(X[group == 1, j], X[group == 2, j])$stat
}


n1 = sum(group == 1)
n2 = sum(group == 2)
Tstats = rep(0, p)
for (j in 1:p){
  m1 = mean(X[group == 1, j])
  m2 = mean(X[group == 2, j])
  var1 = var(X[group == 1, j])
  var2 = var(X[group == 2, j])
  Tstats[j] = (m1 - m2) / sqrt(var1/n1 + var2/n2)
}