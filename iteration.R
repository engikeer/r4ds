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

# 使用safely()函数处理失败的结果
safe_log <- safely(log)
# 成功时
str(safe_log(10))
# 失败时
str(safe_log("a"))
# 与map()结合使用
x <- list(1, 10, "a")
y <- x %>% map(safely(log))
str(y)
y <- y %>% transpose()
str(y)

# 最常见的错误处理方式
# 找到错误对应的输入
is_ok <- y$error %>% map_lgl(is_null)
x[!is_ok]
# 显示成功执行的结果
y$result[is_ok] %>% flatten_dbl()

# possibly
x <- list(1, 10, "a")
x %>% map_dbl(possibly(log, NA_real_))

# quietly
x <- list(1, -1)
x %>% map(quietly(log)) %>% str()

# map2
mu <- list(5, 10, -3)
sigma <- list(1, 5, 10)
map2(mu, sigma, rnorm, n = 5) %>% str()

# pmap
n <- list(1, 3, 5)
args1 <- list(n, mu, sigma)
args1 %>%
    pmap(rnorm) %>% 
    str()

params <- tribble(
    ~mean, ~sd, ~n,
    5,     1,  1,
    10,     5,  3,
    -3,    10,  5
)
params %>% 
    pmap(rnorm)

# invoke_map
f <- c("runif", "rnorm", "rpois")
param <- list(
    list(min = -1, max = 1), 
    list(sd = 5), 
    list(lambda = 10)
)
invoke_map(f, param, n = 5) %>% str()

sim <- tribble(
    ~f,      ~params,
    "runif", list(min = -1, max = 1),
    "rnorm", list(sd = 5),
    "rpois", list(lambda = 10)
)
sim %>% 
    mutate(sim = invoke_map(f, params, n = 10))

# walk
x <- list(1, "a", 3)

x %>% 
    walk(print)

# pwalk
library(ggplot2)
plots <- mtcars %>% 
    split(.$cyl) %>% 
    map(~ggplot(., aes(mpg, wt)) + geom_point())
paths <- stringr::str_c(names(plots), ".pdf")

pwalk(list(paths, plots), ggsave, path = tempdir())

# Predicate functions
# keep and discard
iris %>% 
    keep(is.factor) %>% 
    str()

iris %>% 
    discard(is.factor) %>% 
    str()

# some and every
x <- list(1:5, letters, list(10))

x %>% 
    some(is_character)

x %>% 
    every(is_vector)

# detect 
x <- sample(10)

x %>% 
    detect(~ . > 5)

x %>% 
    detect_index(~ . > 5)

# head_while and tail while
x %>% 
    head_while(~ . > 5)

x %>% 
    tail_while(~ . > 5)

# reduce
dfs <- list(
    age = tibble(name = "John", age = 30),
    sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
    trt = tibble(name = "Mary", treatment = "A")
)

dfs %>% reduce(full_join)

# accumulate
x <- sample(10)
x %>% accumulate(`+`)
