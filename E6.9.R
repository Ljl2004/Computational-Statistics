Ray1 <- function(n, sigma) {
  u <- runif(n)
  x <- numeric(n)
  for (i in 1:n) {
    x[i] =sigma * sqrt(-2 * log(u[i]))
  }
  return(x)
}

Ray2 <- function(n, sigma) {
  u <- runif(n/2)
  x1 <- x2 <- numeric(n/2)
  for (i in 1:(n/2)) {
    x1[i] <- sigma * sqrt(-2 * log(u[i]))
    x2[i]<- sigma * sqrt(-2 * log(1 - u[i]))
  }
  return(c(x1, x2))
}

m <- 10000
sigma <- 2

for (i in 1:n) {
  r1[i] <- mean(Ray1(1000, sigma))
  r2[i] <- mean(Ray2(1000, sigma))
}

var_reduction <- 100 * (var(r1) - var(r2)) / var(r1)

print(var_reduction)