# Setup -------------------------------------------------------------------
rm(list = ls())

library(quantmod)
library(dplyr)
library(ggplot2)

PETR <- getSymbols("PETR4.SA", auto.assign = FALSE, from="2010-01-02", to="2024-12-31")

# Crio um objeto de forma de "tabela", com uma coluna de Data para cada tipo de
# ação
PETR.df <- as.data.frame(PETR)
PETR.df$Date <- as.Date(row.names(PETR.df))

# Selecao de colunas ------------------------------------------------------
PETR.df <- PETR.df %>% dplyr::select(Date, PETR4 = PETR4.SA.Adjusted) %>% as_tibble()


# Calculo do retorno simples de cada ação ---------------------------------

PETR.df <- PETR.df %>% 
  mutate(R_petr = (PETR4 - lag(PETR4))/lag(PETR4))


ggplot(PETR.df) + 
  geom_line(aes(x = Date, y=R_petr), fill = "blue", alpha = 0.5, colour =  "black") +
  theme_bw() +
  labs(title = "Retorno de PETR4",
       subtitle = "2010-01-02 to 2024-12-31",
       y = "Retorno Simples", x = NULL)

ggsave(filename = "./graficos/A1 - Retorno de Petr4_cluster vol.png", 
       width = 16, height = 9, dpi = 100, units = "in")

