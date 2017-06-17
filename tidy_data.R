library(tidyverse)

stocks <- tibble(
    year   = c(2015, 2015, 2016, 2016),
    half  = c(   1,    2,     1,    2),
    return = c(1.88, 0.59, 0.92, 0.17)
)

table3 %>% 
    separate(year, into = c("century", "year"), sep = 2 )

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
    separate(x, c("one", "two", "three"))

df <- data.frame(x = c("a", "a b", "a b c", NA))
