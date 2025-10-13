rm(list=ls())

library(quantmod)
library(fGarch)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

if(FALSE){
  getSymbols("VALE3.SA", from="2019-01-01", to = "2025-01-01")
  
  VALE3 <- VALE3.SA %>% 
    tsbox::ts_tbl() %>% 
    pivot_wider(id_cols = time, names_from = "id", values_from = "value")
  
  VALE3 %>% 
    arrange(desc(time)) %>% 
    write_csv(file = "./database/Lista1_ex2.csv")
}

VALE3 <- read_csv(file = "./database/Lista1_ex2.csv")

# oredena corretamente a base de dados
VALE3 <- VALE3 %>% arrange(time)

VALE3$ret_log <- log(VALE3$VALE3.SA.Close/dplyr::lag(VALE3$VALE3.SA.Close))
VALE3$ret_log20 <- log(VALE3$VALE3.SA.Close/dplyr::lag(VALE3$VALE3.SA.Close, n = 20))

summary(VALE3)


VALE3 %>% 
  ggplot() +
  geom_histogram(aes(x = ret_log, y = after_stat(density)),
                 fill = "blue", 
                 alpha = 0.5,
                 colour = "black",
                 bins = 50) +
  stat_function(mapping = aes(x = ret_log),
                fun = dnorm,
                args = list(mean = mean(VALE3$ret_log, na.rm = TRUE),
                            sd = sd(VALE3$ret_log, na.rm = TRUE) ), 
                colour = "red", size = 1) +
  labs()



VALE3 %>% 
  ggplot() +
  geom_histogram(aes(x = ret_log20, y = after_stat(density)),
                 fill = "blue", 
                 alpha = 0.5,
                 colour = "black",
                 bins = 50) +
  stat_function(mapping = aes(x = ret_log20),
                fun = dnorm,
                args = list(mean = mean(VALE3$ret_log20, na.rm = TRUE),
                            sd = sd(VALE3$ret_log20, na.rm = TRUE) ), 
                colour = "red", size = 1) +
  labs()


