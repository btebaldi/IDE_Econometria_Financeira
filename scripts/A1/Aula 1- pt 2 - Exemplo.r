# Setup -------------------------------------------------------------------
rm(list = ls())

library(quantmod)
library(dplyr)

PETR <- getSymbols("PETR4.SA", auto.assign = FALSE, from="2022-01-02", to="2024-12-31")
VALE <- getSymbols("VALE3.SA", auto.assign = FALSE, from="2022-01-02", to="2024-12-31")
ITUB <- getSymbols("ITUB4.SA", auto.assign = FALSE, from="2022-01-02", to="2024-12-31")
MGLU <- getSymbols("MGLU3.SA", auto.assign = FALSE, from="2022-01-02", to="2024-12-31")


# Crio um objeto de forma de "tabela", com uma coluna de Data para cada tipo de
# ação
PETR.df <- as.data.frame(PETR)
PETR.df$Date <- as.Date(row.names(PETR.df))

VALE.df <- as.data.frame(VALE)
VALE.df$Date <- as.Date(row.names(VALE.df))

ITUB.df <- as.data.frame(ITUB)
ITUB.df$Date <- as.Date(row.names(ITUB.df))

MGLU.df <- as.data.frame(MGLU)
MGLU.df$Date <- as.Date(row.names(MGLU.df))



# Salvo as tabelas em excell ----------------------------------------------

writexl::write_xlsx(x = list("PETR" = PETR.df,
                             "VALE" = VALE.df,
                             "ITUB" = ITUB.df,
                             "MGLU" = MGLU.df),
                    path = "./scripts/A1/pt 2.xlsx")


# Selecao de colunas ------------------------------------------------------

MGLU.df <- MGLU.df %>% dplyr::select(Date, MGLU3 = MGLU3.SA.Close)
ITUB.df <- ITUB.df %>% dplyr::select(Date, ITUB4 = ITUB4.SA.Close)
PETR.df <- PETR.df %>% dplyr::select(Date, PETR4 = PETR4.SA.Close)
VALE.df <- VALE.df %>% dplyr::select(Date, VALE3 = VALE3.SA.Close)


# Junção do banco de dados ------------------------------------------------

full <- dplyr::inner_join(x = MGLU.df, y = ITUB.df, by = c("Date"="Date"))
full <- dplyr::inner_join(x = full, y = PETR.df, by = c("Date"="Date"))
full <- dplyr::inner_join(x = full, y = VALE.df, by = c("Date"="Date"))



# Calculo do retorno simples de cada ação ---------------------------------


full <- full %>% 
  mutate(R_petr = (PETR4 - lag(PETR4))/lag(PETR4),
         R_vale = (VALE3 - lag(VALE3))/lag(VALE3),
         R_itub = (ITUB4 - lag(ITUB4))/lag(ITUB4),
         R_mglu = (MGLU3 - lag(MGLU3))/lag(MGLU3) )


full <- full %>% 
  mutate(R_port = 0.25*R_petr + 0.25*R_vale + 0.25*R_itub + 0.25*R_mglu)


library(ggplot2)

ggplot(full) + 
  geom_line(aes(x = Date, y = R_port))
