# Setup -------------------------------------------------------------------
rm(list = ls())

library(quantmod)
library(dplyr)
library(ggplot2)

Coca <- getSymbols("KO", auto.assign = FALSE, from="2020-01-02", to="2024-12-31")

# Crio um objeto de forma de "tabela", com uma coluna de Data para cada tipo de
# ação
Coca.df <- as.data.frame(Coca)
Coca.df$Date <- as.Date(row.names(Coca.df))

# Selecao de colunas ------------------------------------------------------
Coca.df <- Coca.df %>%
  dplyr::select(Date, Coca = KO.Adjusted) %>%
  as_tibble()


# Calculo do retorno simples de cada ação ---------------------------------

Coca.df <- Coca.df %>% 
  arrange(Date) %>%
  mutate(R = (Coca - lag(Coca))/lag(Coca))


g1 <- ggplot(Coca.df) + 
  geom_line(aes(x = Date, y=Coca), colour =  "black") +
  theme_bw() +
  labs(title = "Coca-Cola Company",
       subtitle = "2010-01-02 to 2024-12-31",
       y = "[USD]", x = NULL)
g1

g2 <- ggplot(Coca.df) + 
  geom_line(aes(x = Date, y=R), colour =  "black") +
  theme_bw() +
  labs(title = "Coca-Cola Company",
       subtitle = "2010-01-02 to 2024-12-31",
       y = "Retorno Simples", x = NULL)

g2

# ggsave(g1, filename = "./graficos/A2 - Nivel de Coca-Cola.png", 
#        width = 16, height = 9, dpi = 100, units = "in")
# 
# ggsave(g2, filename = "./graficos/A2 - Retorno de Coca-Cola.png", 
#        width = 16, height = 9, dpi = 100, units = "in")


library(urca)

urca::ur.kpss(na.omit(Coca.df$R)) |> summary()
urca::ur.df(na.omit(Coca.df$R)) |> summary()


