library(tidyverse)

# ?tidy data
scores <- tibble(
    ability = rep(c("Pure-Ch", "Semi-Ch", "Pure-En"), 4),
    student = c(rep("A", 3), rep("B", 3), rep("C", 3), rep("D", 3)),
    score = c(47, 22, 10, 31, 32, 11, 2, 21, 25, 1, 10, 20)
)

tb <- spread(scores, student, score)
