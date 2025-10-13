rm(list=ls())

library(quantmod)
# library(fGarch)
library(dplyr)
library(tidyr)
library(readr)
library(urca)

if(FALSE){
  getSymbols("^BVSP", from="2022-01-01", to = "2025-01-01")
  
  BVSP <- BVSP %>% 
    tsbox::ts_tbl() %>% 
    pivot_wider(id_cols = time, names_from = "id", values_from = "value")
  
  BVSP %>% 
    arrange(desc(time)) %>% 
    write_csv(file = "./database/Lista2_ex1.csv")
}

BVSP <- read_csv(file = "./database/Lista2_ex1.csv")

# oredena corretamente a base de dados
BVSP <- BVSP %>% arrange(time)

urca::ur.df(BVSP$BVSP.Close) %>% summary()
urca::ur.df(diff(BVSP$BVSP.Close)) %>% summary()

BVSP$ret_log <- log(BVSP$BVSP.Close/dplyr::lag(BVSP$BVSP.Close))

BVSP %>% 
  ggplot() + 
  geom_line(aes(x = time, y = BVSP.Close))

BVSP %>% 
  ggplot() + 
  geom_line(aes(x = time, y = ret_log))

