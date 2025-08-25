MC = function(x){
  R = 10000
  u = runif(R,0,x)
  return(x*mean(exp(-u)/(1+u^2)))
}
Antithetic = function(x){
  R = 10000
  u = runif(R/2,0,x)
  v = 1-u
  u = c(u,v)
  return(x*mean(exp(-u)/(1+u^2)))
}
set.seed(123)
m = 1000
Theta1 = numeric(m)
Theta2 = numeric(m)
for(i in 1:1000){
  Theta1[i] = MC(1)
  Theta2[i] = Antithetic(1)
}
v1 = var(Theta1)
v2 = var(Theta2)
print((v1-v2)/v1)