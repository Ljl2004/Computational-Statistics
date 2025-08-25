# 生成x序列
x <- seq(1, 10, by = 0.1)
# 定义函数
g <- x^2 * exp(-x^2 / 2) / sqrt(2 * pi)
f1 <- 2 / (x + 1)^2
f2 <- 2 * dnorm(x, mean = 1, sd = 1)
# 绘制g/f1 和 g/f2 的比值图像
plot(x, g / f1, type = 'l', col = 'blue')
lines(x, g / f2, col = 'red' )

# 初始化
set.seed(123)
m <- 10000
theta.hat <- variance <- numeric(2)
# 定义g函数
g <- function(x) {
  x^2 * exp(-x^2 / 2) / sqrt(2 * pi) * (x > 1)
}
# 使用f1进行估计
u <- runif(m)
x <- 2 / (1 - u) - 1  # 逆变换法
fg <- g(x) / (2 / (x + 1)^2)
theta.hat[1] <- mean(fg)
variance[1] <- var(fg)
# 使用f2进行估计
x <- numeric(m)
i <- 1
while (i <= m) {
  temp <- rnorm(1, mean = 1, sd = 1)
  if (temp > 1) {
    x[i] <- temp
    i <- i + 1
  }
}
fg <- g(x) / (2 * dnorm(x, mean = 1, sd = 1))
theta.hat[2] <- mean(fg)
variance[2] <- var(fg)
# 结果汇总
t <- rbind(theta.hat, variance)
colnames(t) <- c('f1', 'f2')
print(t)
