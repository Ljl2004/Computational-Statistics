MC = function(x){
  u = runif(10000,0,x)
  return(x*mean(exp(u)))
}
Antithetic = function(x){
  u = runif(5000,0,x)
  v = 1-u
  u = c(u,v)
  return(x*mean(exp(u))) 
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