set.seed(123)
Uni = function(m){
  u = runif(m,0,0.5)
  g = exp(-u)
  theta_hat = 0.5*mean(g)
  return(theta_hat)
}
m = 10000
theta_hat = numeric(10000)
for(i in 1:10000){
  theta_hat[i] = Uni(m)
}
v_hat = var(theta_hat)

Exp = function(m){
  u = rexp(m)
  g = u<=0.5
  theta_star = mean(g)
  return(theta_star)
}
m = 10000
theta_star = numeric(10000)
for(i in 1:10000){
  theta_star[i] = Exp(m)
}
v_star = var(theta_star)

print(v_hat)
print(v_star)