set.seed(123)
x = seq(-5,5,length=11)
cdfs = numeric(length(x))
for(i in 1:length(x)){
  x1 = x[i]
  m = 10000
  if(x1>=0){
    u = runif(m,0,x1)
    cdf = 0.5 + x1*mean(exp(-u^2/2))/sqrt(2*pi)}
  else{
    x1 = -x1
    u = runif(m,0,x1)
    cdf = 0.5 - x1*mean(exp(-u^2/2))/sqrt(2*pi)
  }
  cdfs[i] = cdf
}
Phi = pnorm(x)
print(round(rbind(x,cdfs,Phi),3))

set.seed(123)
m = 10000
u = runif(m,0,5)
for(i in 1:length(x)){
  x2 = x[i]
  g = 0.5 + x2*exp(-u^2/2)/sqrt(2*pi)
}
cdf = mean(g)
var = mean((g-cdf)^2)/m
se = sqrt(var)
CI = c(cdf-1.96*se,cdf+1.96*se)
print(var)
print(CI)