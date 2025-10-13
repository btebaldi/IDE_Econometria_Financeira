rm(list=ls())

library(quantmod)
# library(fGarch)
library(dplyr)
library(tidyr)
library(readr)
library(urca)

if(FALSE){
getSymbols("PETR4.SA", from="2018-01-01", to = "2025-01-01")
  
  PETR4.SA <- PETR4.SA %>% 
    tsbox::ts_tbl() %>% 
    pivot_wider(id_cols = time, names_from = "id", values_from = "value")

  
  PETR4.SA %>% 
    arrange(desc(time)) %>% 
    write_csv(file = "./database/Lista5_ex1.csv")
}

PETRO <- read_csv(file = "./database/Lista5_ex1.csv")

ret <- diff(log(PETRO$PETR4.SA.Close))

alpha <- 0.99
mu <- mean(ret, na.rm=TRUE)
sigma <- sd(ret, na.rm=TRUE)

VaR_param <- -(mu + qnorm(1-alpha) * sigma)
VaR_param


VaR_hist <- quantile(ret, probs = 0.01, na.rm = TRUE)
VaR_hist

