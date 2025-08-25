generate_random_sample <- function(pmf, n) {
  # 计算累积分布函数（CDF）
  cdf <- cumsum(pmf)
  
  # 将CDF转换为[0, 1]之间的均匀随机数
  u <- runif(n, min = 0, max = 1)
  
  # 使用逆变换方法生成随机样本
  sample <- sapply(u, function(x) min(which(cdf >= x)))
  
  return(sample)
}

# 定义概率质量函数
pmf <- c(0.1, 0.2, 0.2, 0.2, 0.3)

# 生成随机样本
set.seed(123)
sample <- generate_random_sample(pmf, 1000)

# 构造相对频率表
relative_freq_table <- table(factor(sample, levels=1:length(pmf))) / length(sample)

# 输出相对频率表
print(relative_freq_table)

# 比较理论概率与经验概率
theoretical_probs <- pmf
empirical_probs <- as.numeric(relative_freq_table)

# 输出结果
print(data.frame(Theoretical = theoretical_probs, Empirical = empirical_probs))
