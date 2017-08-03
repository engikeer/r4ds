library(tidyverse)
library(nycflights13)

# 示例数据1
df <- tibble(
    a = rnorm(10),
    b = rnorm(10),
    c = rnorm(10),
    d = rnorm(10)
)

# 计算每列的中位数
output <- vector("double", ncol(df))  # 1. 输出
for (i in seq_along(df)) {            # 2. 条件
    output[[i]] <- median(df[[i]])      # 3. 循环体
}
output
#> [1] -0.2458 -0.2873 -0.0567  0.1443

# 练习1，求中位数
mtcars_output <- vector("numeric", ncol(mtcars))

for (i in seq_along(mtcars)) {
    mtcars_output[[i]] <- median(mtcars[[i]])
}

mtcars_output

# 练习2，统计类型
flights_output <- vector("character", ncol(flights))
for (i in seq_along(flights)) {
    flights_output[[i]] <- typeof(flights[[i]])
}
flights_output

# 练习3， 计算唯一值的个数
iris_output <- vector("numeric", ncol(iris))
for (i in seq_along(iris)) {
    iris_output[[i]] <- length(unique(iris[[i]]))
}
iris_output

# 处理长度不确定的输出
means <- c(0, 1, 2)
out <- vector("list", length(means))
for (i in seq_along(means)) {
    n <- sample(100, 1)
    out[[i]] <- rnorm(n, means[[i]])
}
str(out)

str(unlist(out))

# 测试
x <- c(1, 3, 5)
# x <- c(a = 1, b = 3 , c = 5)
i <- 1
for (nm in names(x)) {
    print(i)
    # print(paste(nm, "：", x[[nm]], collapse = ""))
    cat(nm, ": ", x[[nm]], "\n", sep = "")
    i <- i + 1
}

# while循环
flip <- function() sample(c("T", "H"), 1)

flips <- 0
nheads <- 0

while (nheads < 3) {
    if (flip() == "H") {
        nheads <- nheads + 1
    } else {
        nheads <- 0
    }
    flips <- flips + 1
}
flips

# 函数式编程
df <- tibble(
    a = rnorm(10),
    b = rnorm(10),
    c = rnorm(10),
    d = rnorm(10)
)

col_summary <- function(df, fun) {
    out <- vector("double", length(df))
    for (i in seq_along(df)) {
        out[i] <- fun(df[[i]])
    }
    out
}
col_summary(df, median)
#> [1]  0.237 -0.218  0.254 -0.133
col_summary(df, mean)
#> [1]  0.2026 -0.2068  0.1275 -0.0917

# 使用map()函数
df %>% map_dbl(mean)
#>       a       b       c       d 
#>  0.2026 -0.2068  0.1275 -0.0917
df %>% map_dbl(median)
#>      a      b      c      d 
#>  0.237 -0.218  0.254 -0.133
df %>% map_dbl(sd)
#>     a     b     c     d 
#> 0.796 0.759 1.164 1.062

# 简便写法
models <- mtcars %>% 
    split(.$cyl) %>% 
    map(~lm(mpg ~ wt, data = .))

# 提取部分结果
models %>% 
    map(summary) %>% 
    map_dbl("r.squared")

# 通过位置提取结果
x <- list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
x %>% map_dbl(2)


