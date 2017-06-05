library(tidyverse)

# 可视化类别型变量
ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut))

# 手动计算类别型变量的计数
diamonds %>% 
    count(cut)


# 可视化连续型变量
ggplot(data = diamonds) +
    geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

# 手动计算连续型变量的计数
diamonds %>% 
    count(cut_width(carat, 0.5))

# 修改直方图的组间距
smaller <- diamonds %>% 
    filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
    geom_histogram(binwidth = 0.1)

# 在一个图形中绘制多个直方图
ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
    geom_freqpoly(binwidth = 0.1)

# 高度代表出现频率
ggplot(data = smaller, mapping = aes(x = carat)) +
    geom_histogram(binwidth = 0.01)

# 相似的集群暗示可能存在子分组
ggplot(data = faithful, mapping = aes(x = eruptions)) + 
    geom_histogram(binwidth = 0.25)

# 大数据集的直方图很难观察异常值
ggplot(diamonds) + 
    geom_histogram(mapping = aes(x = y), binwidth = 0.5)

# 通过对较小的值进行缩放，来观察图中的异常值
ggplot(diamonds) + 
    geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
    coord_cartesian(ylim = c(0, 50))

# 使用dplyr筛选出异常值
unusual <- diamonds %>% 
    filter(y < 3 | y > 20) %>% 
    select(price, x, y, z) %>%
    arrange(y)


