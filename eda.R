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

# 移除含有异常值的观测
diamonds2 <- diamonds %>% 
    filter(between(y, 3, 20))

# 将异常值替换为缺失值
diamonds2 <- diamonds %>% 
    mutate(y = ifelse(y < 3 | y > 20, NA, y))

# ggplot2会移除含有缺失值的行并给出提示
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
    geom_point()
#> Warning: Removed 9 rows containing missing values (geom_point).

# 可以通过is.na()创建一个新变量来比较含有缺失值的观测与其他观测有何不同
nycflights13::flights %>% 
    mutate(
        cancelled = is.na(dep_time),
        sched_hour = sched_dep_time %/% 100,
        sched_min = sched_dep_time %% 100,
        sched_dep_time = sched_hour + sched_min / 60
    ) %>% 
    ggplot(mapping = aes(sched_dep_time)) + 
    geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

# 类别值与连续值的关系
# 默认的geom_freqploy使用计数，仅样本量相当时才能看出组间差别
ggplot(data = diamonds, mapping = aes(x = price)) + 
    geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

# 查看样本数的差别
ggplot(diamonds) + 
    geom_bar(mapping = aes(x = cut))

# 可将y坐标改为密度来显示组间差别
ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
    geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

# 可以用箱线图探究分布的差别
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
    geom_boxplot()

# 使用reorder()对因子水平重排序
ggplot(data = mpg) +
    geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))

# 通过coord_flip()将图形旋转90度，以显示长变量名
ggplot(data = mpg) +
    geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
    coord_flip()

# 两个类别值的关系
ggplot(data = diamonds) +
    geom_count(mapping = aes(x = cut, y = color))

# 可以通过dplyr计算类别值，然后通过geom_tile()进行可视化
diamonds %>% 
    count(color, cut) %>%  
    ggplot(mapping = aes(x = color, y = cut)) +
    geom_tile(mapping = aes(fill = n))

# 如果样本量很大，可以使用方块图来可视化
smaller <- diamonds %>% 
    filter(carat < 3)

# 方块图
ggplot(data = smaller) +
    geom_bin2d(mapping = aes(x = carat, y = price))

# install.packages("hexbin")

# 六边形图
ggplot(data = smaller) +
    geom_hex(mapping = aes(x = carat, y = price))