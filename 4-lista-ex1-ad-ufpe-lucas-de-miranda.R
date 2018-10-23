load("./vote_growth_usa.RData")

read.dta("fair.dta")

install.packages("foreign")
library("foreign")

read.dta("fair.dta")
load("./fair.dta")

summary(bdstata)
summary(bd)

reg1 <- lm(data = bd, Vote ~ Growth)
summary(reg1)

library(ggplot2)

ggplot(data = reg1, mapping = aes(x = Growth, y = Vote)) +
  geom_point() +
  geom_abline(data = reg1, slope = 0.6249, intercept = 51.5082)

dataset1 <- intersect(bd, bdstata)

datasetmod <- dataset1[1 : 34,]

reg2 <- lm(data = datasetmod, Vote ~ Growth + GOODNEWS)
summary(reg2)

?intersect
library(dplyr)
