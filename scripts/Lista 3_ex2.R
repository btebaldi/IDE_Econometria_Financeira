rm(list=ls())

library(quantmod)
library(fGarch)
library(dplyr)
library(tidyr)
library(readr)
library(urca)


archTest <- function(rtn, m=10){
  # Perform Lagrange Multiplier Test for ARCH effect of a time series
  # rtn: time series
  # m: selected AR order
  
  # calcula os residuos ao quadrado
  y=(rtn-mean(rtn))^2
  
  # Tamanho da serie 
  t=length(rtn)
  
  # Cria uma matrix com m defasagens
  atsq <- y[(m+1):t]
  
  # Matriz de defasagens
  x <- matrix(0,(t-m),m)
  
  for (i in 1:m){
    x[, i] = y[(m+1-i):(t-i)]
  }
  
  colnames(x) <- sprintf("Lag_%s", m:1)
  md <- lm(atsq~x)
  
  print(summary(md))
  
  return(md)
}


if(FALSE){
  getSymbols("TXN", from="1973-01-01", to = "2010-01-01")
  
  TXN <- TXN %>% 
    tsbox::ts_tbl() %>% 
    pivot_wider(id_cols = time, names_from = "id", values_from = "value")
  
  TXN %>% 
    arrange(desc(time)) %>% 
    write_csv(file = "./database/Lista3_ex2.csv")
}

TXN <- read_csv(file = "./database/Lista3_ex2.csv")

# oredena corretamente a base de dados
TXN <- TXN %>% arrange(time)


TXN$ret_log <- log(TXN$TXN.Close/dplyr::lag(TXN$TXN.Close))    

TXN <- TXN %>% na.omit()

# Teste para a "volatilidade realizada"
y <- TXN$ret_log - mean(TXN$ret_log)
Box.test(y^2,lag=12,type="Ljung")


archTest(y, 12) #% output edited.


