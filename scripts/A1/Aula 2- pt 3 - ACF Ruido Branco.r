# Setup -------------------------------------------------------------------
rm(list = ls())

library(quantmod)
library(dplyr)
library(ggplot2)


# Calculo do retorno simples de cada ação ---------------------------------

RB <- rnorm(n = 200)

forecast::Acf(RB) %>% forecast::autoplot() +
  # theme_bw() +
  labs(title = "Ruido Branco - ACF")

ggsave(filename = "./graficos/A2 - ACF Ruido Branco.png", 
       width = 16, height = 9, dpi = 100, units = "in")
