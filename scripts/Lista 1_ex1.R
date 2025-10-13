rm(list=ls())

library(quantmod)
library(fGarch)
library(dplyr)
library(tidyr)
library(readr)

if(FALSE){
  getSymbols("PETR4.SA", from="2022-01-01", to = "2025-01-01")
  
  PETR4 <- PETR4.SA %>% 
    tsbox::ts_tbl() %>% 
    pivot_wider(id_cols = time, names_from = "id", values_from = "value")
  
  PETR4 %>% 
    arrange(desc(time)) %>% 
    write_csv(file = "./database/Lista1_ex1.csv")
}

PETR4 <- read_csv(file = "./database/Lista1_ex1.csv")

# oredena corretamente a base de dados
PETR4 <- PETR4 %>% arrange(time)

ret_simples <- PETR4$PETR4.SA.Close/dplyr::lag(PETR4$PETR4.SA.Close) - 1

ret_log <- log(PETR4$PETR4.SA.Close/dplyr::lag(PETR4$PETR4.SA.Close))

summary(ret_simples)
summary(ret_log)


