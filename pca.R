library(tidyverse)
library(psych)

Harman74.cor

fa.parallel(USJudgeRatings[,-1], fa="pc", n.iter=100,
            show.legend=FALSE, main="Scree plot with parallel analysis")

pc <- principal(USJudgeRatings[, -1], nfactors=1)
