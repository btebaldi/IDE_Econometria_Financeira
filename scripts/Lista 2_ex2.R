rm(list=ls())

library(quantmod)
# library(fGarch)
library(dplyr)
library(tidyr)
library(readr)
library(urca)

if(FALSE){
  getSymbols("ITUB4.SA", from="2022-01-01", to = "2025-01-01")
  
  ITUB4.SA <- ITUB4.SA %>% 
    tsbox::ts_tbl() %>% 
    pivot_wider(id_cols = time, names_from = "id", values_from = "value")
  
  ITUB4.SA %>% 
    arrange(desc(time)) %>% 
    write_csv(file = "./database/Lista2_ex2.csv")
}

ITUB4 <- read_csv(file = "./database/Lista2_ex2.csv")

# oredena corretamente a base de dados
ITUB4 <- ITUB4 %>% arrange(time)

ITUB4$ret_log <- log(ITUB4$ITUB4.SA.Close/dplyr::lag(ITUB4$ITUB4.SA.Close))

urca::ur.df(na.omit(ITUB4$ret_log)) %>% summary()

forecast::Acf(ITUB4$ret_log)
forecast::Pacf(ITUB4$ret_log)


mdl_0 <- forecast::auto.arima(ITUB4$ret_log)
mdl_1 <- forecast::Arima(ITUB4$ret_log, order = c(1,0,0)) 

predict(mdl_0, 10)
predict(mdl_1, 10)






