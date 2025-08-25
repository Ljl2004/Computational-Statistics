# 定义 Epanechnikov 分布的密度函数
epanechnikov_density <- function(x) {
  ifelse(abs(x) <= 1, (3/4) * (1 - x^2), 0)
}

# 模拟 Epanechnikov 分布的随机变元
generate_epanechnikov <- function() {
  repeat {
    u1 <- runif(1, -1, 1)
    u2 <- runif(1, -1, 1)
    u3 <- runif(1, -1, 1)
    
    if (abs(u3) >= abs(u2) && abs(u3) >= abs(u1)) {
      return(u2)
    } else {
      return(u3)
    }
  }
}
# 生成大量 Epanechnikov 分布的随机变元
set.seed(123)
n <- 100000
epanechnikov_sample <- replicate(n, generate_epanechnikov())

# 构造直方图
hist(epanechnikov_sample, breaks = 50, freq = FALSE, main = "Histogram of Epanechnikov Sample", 
     xlab = "Value", ylab = "Density", col = "lightblue")
density(epanechnikov_sample, add = TRUE, col = "red", lwd = 2)