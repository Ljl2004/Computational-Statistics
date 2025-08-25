#利用Cholesky分解生成变量
rmvn.Cholesky <-
  function(n, mu, Sigma) {
    # generate n random vectors from MVN(mu, Sigma)
    # dimension is inferred from mu and Sigma
    d <- length(mu)
    Q <- chol(Sigma) # Choleski factorization of Sigma
    Z <- matrix(rnorm(n*d), nrow=n, ncol=d)
    X <- Z %*% Q + matrix(mu, n, d, byrow=TRUE)
    X
  }

#代入协方差矩阵和均值向量
Sigma = c(1,-.5,.5,-.5,1,-.5,.5,-.5,1)
Sigma = matrix(Sigma,nrow=3)
mu = c(0,1,2)
samples = rmvn.Cholesky(200,mu,Sigma)

#输出散点图
X1 = samples[,1]
X2 = samples[,2]
X3 = samples[,3]
pairs(samples)
apply(samples,MARGIN = 2,FUN = mean)
cor(X1,X2)
cor(X1,X3)
cor(X2,X3)