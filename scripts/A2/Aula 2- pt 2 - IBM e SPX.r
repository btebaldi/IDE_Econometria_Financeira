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

tbl <- tbl %>% 
  arrange(Date) %>%
  mutate(R_ibm = (IBM - lag(IBM))/lag(IBM),
         R_spx = (SPX - lag(SPX))/lag(SPX) )


g1 <- ggplot(tbl) + 
  geom_line(aes(x = Date, y=IBM/IBM[1], colour = "IBM")) +
  geom_line(aes(x = Date, y=SPX/SPX[1], colour = "SPX")) +
  theme_bw() +
  labs(title = "IBM / SPX",
       subtitle = "2010-01-02 to 2024-12-31",
       y = NULL, x = NULL, colour = NULL)
g1

g2 <- ggplot(tbl) + 
  geom_point(aes(x = R_ibm, y=R_spx)) +
  theme_bw() +
  labs(title = "Grafico de dispersão",
       subtitle = "2010-01-02 to 2024-12-31",
       y = "SPX", x = "IBM")

g2

ggsave(g1, filename = "./graficos/A2 - Nivel IBM e SPX.png", 
       width = 16, height = 9, dpi = 100, units = "in")

ggsave(g2, filename = "./graficos/A2 - Dispersao IBM e SPX.png", 
       width = 16, height = 9, dpi = 100, units = "in")




