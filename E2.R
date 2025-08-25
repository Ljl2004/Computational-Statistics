set.seed(123)
MC <- function(d) {
  r <- 0.5
  x = matrix(runif(d, -r, r),ncol=d)
  pi_estimate <- 0
  n = 0
  in_sphere = 0
  while (TRUE) {
    n = n + 1
    x = runif(d, -r, r) 
    in_sphere = in_sphere + as.numeric(sum(x^2) <= r^2) 
    fraction = in_sphere/n
    pi_estimate = (fraction*2^d*gamma(d/2+1))^(2/d)
    if ((pi_estimate*100000)%/%1 == 314159)  break
  }
  return(n)
}

dimensions = 2:10
ns = numeric(length(dimensions))
for(i in 1:length(dimensions)){
  ns[i] = MC(dimensions[i])
}
print(rbind(dimensions,ns))