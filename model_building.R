library(tidyverse)
library(modelr)
options(na.action = na.warn)

library(nycflights13)
library(lubridate)

# 品质差的钻石价格反而高
ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()

# 其原因在于品质差的钻石有着更大的克拉，而克拉数对价格有决定性的影响
ggplot(diamonds, aes(carat, price)) + 
    geom_hex(bins = 50)

# 对克拉和价格进行对数变换，使关系接近线性
diamonds2 <- diamonds %>% 
    filter(carat <= 2.5) %>% 
    mutate(lprice = log2(price), lcarat = log2(carat))

# 创建拟合模型
mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)

# 添加预测值
grid <- diamonds2 %>% 
    data_grid(carat = seq_range(carat, 20)) %>% 
    mutate(lcarat = log2(carat)) %>% 
    add_predictions(mod_diamond, "lprice") %>% 
    mutate(price = 2 ^ lprice)

# 生成拟合图形
ggplot(diamonds2, aes(carat, price)) + 
    geom_hex(bins = 50) + 
    geom_line(data = grid, colour = "red", size = 1)

# 包含更多变量的模型
mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)

# 对cut进行可视化
grid <- diamonds2 %>% 
    data_grid(cut, .model = mod_diamond2) %>% 
    add_predictions(mod_diamond2)

ggplot(grid, aes(cut, pred)) + 
    geom_point()