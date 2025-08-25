#生成exp—gamma混合函数
Exp_Gamma_mixture <- function(r,beta,n){
  U1 = rgamma(n,r,beta)
  U2 = rexp(n,U1)
}

#生成目标函数
r = 4
beta = 2
results = Exp_Gamma_mixture(r,beta,1000)

#输出直方图以及曲线
hist(results,col='lightblue',freq = F,breaks=50,xlab='x',ylab = 'Density',
     main='Empirical Distribution vs. Theoretical Pareto Distribution')
xx = seq(min(results),max(results),length=1000)
yy = beta^r * r * (beta+xx)^(-r-1)
lines(xx,yy,col='red',lwd=2)