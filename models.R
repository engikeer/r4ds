library(tidyverse)

library(modelr)
options(na.action = na.warn)

# 模拟数据
ggplot(sim1, aes(x, y)) + 
    geom_point()

# 生成随机模型
models <- tibble(
    a1 = runif(250, -20, 40),
    a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) + 
    geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
    geom_point() 

# 计算预测值
model1 <- function(a, data) {
    a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1)

# 计算预测值与实际值之间的差距
measure_distance <- function(mod, data) {
    diff <- data$y - model1(mod, data)
    sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)

sim1_dist <- function(a1, a2) {
    measure_distance(c(a1, a2), sim1)
}

models <- models %>% 
    mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models

# 找出最好的10个模型
ggplot(sim1, aes(x, y)) + 
    geom_point(size = 2, colour = "grey30") + 
    geom_abline(
        aes(intercept = a1, slope = a2, colour = -dist), 
        data = filter(models, rank(dist) <= 10)
    )

# 可视化所有模型
ggplot(models, aes(a1, a2)) +
    geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
    geom_point(aes(colour = -dist))

# 使用网格找到最好的模型
grid <- expand.grid(
    a1 = seq(-5, 20, length = 25),
    a2 = seq(1, 3, length = 25)
) %>% 
    mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>% 
    ggplot(aes(a1, a2)) +
    geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
    geom_point(aes(colour = -dist)) 

ggplot(sim1, aes(x, y)) + 
    geom_point(size = 2, colour = "grey30") + 
    geom_abline(
        aes(intercept = a1, slope = a2, colour = -dist), 
        data = filter(grid, rank(dist) <= 10)
    )

# 使用牛顿-拉弗森得到最好的模型
best <- optim(c(0, 0), measure_distance, data = sim1)
best$par

ggplot(sim1, aes(x, y)) + 
    geom_point(size = 2, colour = "grey30") + 
    geom_abline(intercept = best$par[1], slope = best$par[2])

# 使用lm()得到最好的模型
sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

# 2. 可视化模型
# 计算数据网格
grid <- sim1 %>% 
    data_grid(x) 
grid

# 添加预测值
grid <- grid %>% 
    add_predictions(sim1_mod) 
grid

# 可视化预测值
ggplot(sim1, aes(x)) +
    geom_point(aes(y = y)) +
    geom_line(aes(y = pred), data = grid, colour = "red", size = 1)

# 添加残差
sim1 <- sim1 %>% 
    add_residuals(sim1_mod)
sim1

# 残差频率图
ggplot(sim1, aes(resid)) + 
    geom_freqpoly(binwidth = 0.5)

# 残差分布图
ggplot(sim1, aes(x, resid)) + 
    geom_ref_line(h = 0) +
    geom_point()
