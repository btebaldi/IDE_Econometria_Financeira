rm(list=ls())

library(quantmod)
# library(fGarch)
library(dplyr)
library(tidyr)
library(readr)
library(urca)

if(FALSE){
  getSymbols("INTC", from="1973-01-01", to = "2010-01-01")
  
  INTC <- INTC %>% 
    tsbox::ts_tbl() %>% 
    pivot_wider(id_cols = time, names_from = "id", values_from = "value")
  
  INTC %>% 
    arrange(desc(time)) %>% 
    write_csv(file = "./database/Lista3_ex1.csv")
}

INTC <- read_csv(file = "./database/Lista3_ex1.csv")

# oredena corretamente a base de dados
INTC <- INTC %>% arrange(time)

INTC$ret_log <- log(INTC$INTC.Close/dplyr::lag(INTC$INTC.Close))

mARCH = garchFit(~ 1 + garch(1,0),
             data=na.omit(INTC$ret_log),
             cond.dist = "norm",
             trace = FALSE) # parameter estimates


mGARCH = garchFit(~ 1 + garch(1,1),
                 data=na.omit(INTC$ret_log),
                 cond.dist = "norm",
                 trace = FALSE) # parameter estimates



# Residuos 
resi.ARCH <- residuals(mARCH, standardize=TRUE)
Box.test(resi.ARCH^2,lag=20,type="Ljung")

hist(resi.ARCH, "FD")


resi.GARCH <- residuals(mGARCH, standardize=TRUE)
Box.test(resi.GARCH^2,lag=20,type="Ljung")
hist(resi.GARCH, "FD")


