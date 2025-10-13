rm(list=ls())

library(quantmod)
# library(fGarch)
library(dplyr)
library(tidyr)
library(readr)
library(urca)

if(FALSE){
  getSymbols("VALE3.SA", from="2014-01-01", to = "2025-01-01")
  
  VALE3.SA <- VALE3.SA %>% 
    tsbox::ts_tbl() %>% 
    pivot_wider(id_cols = time, names_from = "id", values_from = "value")
  
  
  VALE3.SA %>% 
    arrange(desc(time)) %>% 
    write_csv(file = "./database/Lista5_ex2.csv")
}

VALE3 <- read_csv(file = "./database/Lista5_ex2.csv")


ret <- diff(log(VALE3$VALE3.SA.Close))
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

