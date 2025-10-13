# Setup -------------------------------------------------------------------
rm(list = ls())

library(quantmod)
library(dplyr)
library(ggplot2)

# Calculo do retorno simples de cada ação ---------------------------------

getSymbols(c("^VIX"), auto.assign = TRUE, from="2020-01-02", to="2024-12-31")

VIX$VIX.Close %>% forecast::autoplot() + 
  labs(title = "VIX",
       subtitle = "Volatilidade implícida das opções do S&P500", y=NULL, x=NULL)

ggsave(filename = "./graficos/A3 - VIX.png", 
       width = 16, height = 9, dpi = 100, units = "in", scale = 0.5)


# Calculo do retorno simples de cada ação ---------------------------------

getSymbols(c("IBM"), auto.assign = TRUE, from="2020-01-02", to="2024-12-31")
IBM$IBM.Close %>% autoplot()

IBM$IBM.Close %>% diff() %>% autoplot()

IBM$IBM.r <- log(IBM$IBM.Close/lag(IBM$IBM.Close))

IBM$IBM.r %>% autoplot()

forecast::Acf(IBM$IBM.r)
forecast::Pacf(IBM$IBM.r)

forecast::Acf(IBM$IBM.r^2)
forecast::Pacf(IBM$IBM.r^2)


library(ruga)
