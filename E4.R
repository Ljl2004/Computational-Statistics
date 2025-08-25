# 定义函数来计算概率
estimate_p <- function(n, method="normal") {
  if(method == "normal") {
    # 使用标准正态分布
    z <- rnorm(n)
    sum(z > 10)/n
  } else if(method == "change_of_measure") {
    # 使用改变度量的方法
    y <- rnorm(n, mean=10, sd=1)
    sum(y > 10)/n
  }
}

# 蒙特卡洛模拟
for(i in c(3, 4)) {
  print(paste("Sample size:", 10^i))
  
  for(j in c(1e3, 1e4, 1e5, 1e6)) {
    estimate <- replicate(100, estimate_p(j, method="normal"))
    print(paste("Normal distribution:", round(mean(estimate), 15)))
    
    estimate <- replicate(100, estimate_p(j, method="change_of_measure"))
    print(paste("Change of measure:", round(mean(estimate), 15)))
  }
}