rm(list=ls())

library(quantmod)
# library(fGarch)
library(dplyr)
library(tidyr)
library(readr)
library(urca)

if(FALSE){
  getSymbols("KO", from="1970-01-01", to = "2019-01-01")
  getSymbols("PEP", from="1970-01-01", to = "2019-01-01")
  
  KO <- KO %>% 
    tsbox::ts_tbl() %>% 
    pivot_wider(id_cols = time, names_from = "id", values_from = "value") %>% 
    dplyr::select(time, KO.Close)
  
  PEP <- PEP %>% 
    tsbox::ts_tbl() %>% 
    pivot_wider(id_cols = time, names_from = "id", values_from = "value") %>% 
    dplyr::select(time, PEP.Close)
  
  inner_join(KO, PEP, by = c("time"="time")) %>% 
    arrange(desc(time)) %>% 
    write_csv(file = "./database/Lista4_ex1.csv")
}

tbl <- read_csv(file = "./database/Lista4_ex1.csv")

# oredena corretamente a base de dados
tbl <- tbl %>% arrange(time)

tbl$ret_KO <- log(tbl$KO.Close/dplyr::lag(tbl$KO.Close))
tbl$ret_PEP <- log(tbl$PEP.Close/dplyr::lag(tbl$PEP.Close))

tbl <- tbl %>% 
  mutate(xp = ret_KO + ret_PEP,
         xm = ret_KO - ret_PEP) %>% 
  na.omit()


tbl %>% 
  ggplot() + 
  geom_point(aes(x = ret_KO  , y = ret_PEP)) + 
  labs(y = "Correlation", x = NULL, title = "Correlation CSCO vs. CAT")

normal_cor <- cor(tbl[, 4:5])


# Calculo dos modelos -----------------------------------------------------
m1 <- garchFit(~1+garch(1,1), data=tbl$xp, trace=FALSE)
summary(m1)

m2 <- garchFit(~1+garch(1,1), data=tbl$xm, trace=FALSE)
summary(m2)

mKO <- garchFit(~1+garch(1,1), data=tbl$ret_KO, trace=FALSE)
summary(mKO)

mPEP <- garchFit(~1+garch(1,1), data=tbl$ret_PEP, trace=FALSE)
summary(mPEP)

vxp <- fGarch::volatility(m1)
vxm <- fGarch::volatility(m2)
v_ko <- fGarch::volatility(mKO)
v_pep <- fGarch::volatility(mPEP)

tbl$Cov_t <- (vxp^2-vxm^2)/(4)
tbl$Cor_t <- tbl$Cov_t/(v_ko * v_pep)


tbl %>% 
  ggplot() + 
  geom_line(aes(x = time, y = Cor_t)) + 
  geom_hline(yintercept = normal_cor[1,2], colour = "red", size = 1) + 
labs()  

