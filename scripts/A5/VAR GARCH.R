rm(list=ls())

library(quantmod)
library(fGarch)

getSymbols("^BVSP", from="2018-01-01")

ret <- diff(log(BVSP$BVSP.Close))
summary(ret)
ret <- na.omit(ret)

g = garchFit(~ 1 + garch(1,1),
             data=ret,
             cond.dist = "norm",
             include.mean = FALSE,
             trace = FALSE) # parameter estimates

tt <- length(ret)

omega = g@fit$matcoef[1,1]
alpha = g@fit$matcoef[2,1]
beta = g@fit$matcoef[3,1]
sigma2 = omega + alpha * ret[tt]^2 + beta * g@h.t[tt]

p <- 0.05
value <- 1

# compute sigma2 for t+1
VaR9 = -sqrt(sigma2) * qnorm(p) * value
VaR9

previsao_g <- predict(g)

VaR9 = -previsao_g$standardDeviation[1] * qnorm(p) * value
VaR9
