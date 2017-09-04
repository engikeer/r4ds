library(modelr)
library(tidyverse)

library(gapminder)

# 对单个国家进行建模，步骤已介绍
nz <- filter(gapminder, country == "New Zealand")

# 探索数据分布趋势
nz %>%
    ggplot(aes(year, lifeExp)) + 
    geom_point() + 
    geom_smooth()

nz %>% 
    ggplot(aes(year, lifeExp)) + 
    geom_line() + 
    ggtitle("Full data = ")

nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>% 
    add_predictions(nz_mod) %>%
    ggplot(aes(year, pred)) + 
    geom_line() + 
    ggtitle("Linear trend + ")

nz %>% 
    add_residuals(nz_mod) %>% 
    ggplot(aes(year, resid)) + 
    geom_hline(yintercept = 0, colour = "white", size = 3) + 
    geom_line() + 
    ggtitle("Remaining pattern")

# 使用嵌套数据框应用到每个国家
# 先分组，后嵌套
by_country <- gapminder %>% 
    group_by(country, continent) %>% 
    nest()

by_country

# 构建建模函数
country_model <- function(df) {
    lm(lifeExp ~ year, data = df)
}

# 使用map()函数应用到每个data上，并将模型保存为一列
by_country <- by_country %>% 
    mutate(model = map(data, country_model))
by_country

# 对每个model-data对计算残差
by_country <- by_country %>% 
    mutate(
        resids = map2(data, model, add_residuals)
    )
by_country

# 将嵌套数据框恢复为普通数据框
resids <- unnest(by_country, resids)
resids

# 通过普通数据框，就可以对残差进行可视化
resids %>% 
    ggplot(aes(year, resid)) +
    geom_line(aes(group = country), alpha = 1 / 3) + 
    geom_smooth(se = FALSE)

# 通过大洲将图形切片
resids %>% 
    ggplot(aes(year, resid, group = country)) +
    geom_line(alpha = 1 / 3) + 
    facet_wrap(~continent)

# 使用broom::glance()添加质量指标
glance <- by_country %>% 
    mutate(glance = map(model, broom::glance)) %>% 
    unnest(glance, .drop = TRUE)

# 找出表现最差的模型
bad_fit <- filter(glance, r.squared < 0.25)

gapminder %>% 
    semi_join(bad_fit, by = "country") %>% 
    ggplot(aes(year, lifeExp, colour = country)) +
    geom_line()

# 创建list列
