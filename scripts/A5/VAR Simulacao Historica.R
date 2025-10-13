rm(list=ls())

library(quantmod)

getSymbols("^BVSP", from="2018-01-01")

ret <- diff(log(BVSP$BVSP.Close))

VaR <- quantile(ret, probs = 0.01, na.rm = TRUE)
VaR
