rm(list = ls())

library(quantmod)

getSymbols("^BVSP", from="2018-01-01")

ret <- diff(log(BVSP$BVSP.Close))

alpha <- 0.99
mu <- mean(ret, na.rm=TRUE)
sigma <- sd(ret, na.rm=TRUE)

VaR <- -(mu + qnorm(1-alpha) * sigma)
VaR
