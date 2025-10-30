# Setup -------------------------------------------------------------------
rm(list = ls())

library(quantmod)
library(dplyr)
library(ggplot2)

getSymbols(c("IBM", "^SPX"), auto.assign = TRUE, from="2020-01-02", to="2024-12-31")


# Crio um objeto de forma de "tabela", com uma coluna de Data para cada tipo de
# ação
IBM.df <- as.data.frame(IBM)
IBM.df$Date <- as.Date(row.names(IBM.df))

SPX.df <- as.data.frame(SPX)
SPX.df$Date <- as.Date(row.names(SPX.df))

# Selecao de colunas ------------------------------------------------------
SPX.df <- SPX.df %>%
  dplyr::select(Date, SPX = SPX.Adjusted) %>%
  as_tibble()

IBM.df <- IBM.df %>%
  dplyr::select(Date, IBM = IBM.Adjusted) %>%
  as_tibble()



# Calculo do retorno simples de cada ação ---------------------------------

tbl <- inner_join(SPX.df, IBM.df, by = "Date")

forecast::Acf(tbl$R_ibm) %>% forecast::autoplot() +
  # theme_bw() +
  labs(title = "IBM - ACF",
       subtitle = "2010-01-02 to 2024-12-31")

ggsave(filename = "./graficos/A2 - ACF.png", 
       width = 16, height = 9, dpi = 100, units = "in")

