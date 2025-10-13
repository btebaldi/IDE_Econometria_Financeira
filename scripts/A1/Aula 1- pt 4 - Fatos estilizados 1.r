# Setup -------------------------------------------------------------------
rm(list = ls())

library(quantmod)
library(dplyr)
library(ggplot2)

PETR <- getSymbols("PETR4.SA", auto.assign = FALSE, from="2021-01-02", to="2024-12-31")

# Crio um objeto de forma de "tabela", com uma coluna de Data para cada tipo de
# ação
PETR.df <- as.data.frame(PETR)
PETR.df$Date <- as.Date(row.names(PETR.df))

# Selecao de colunas ------------------------------------------------------
PETR.df <- PETR.df %>% dplyr::select(Date, PETR4 = PETR4.SA.Adjusted) %>% as_tibble()


# Calculo do retorno simples de cada ação ---------------------------------

PETR.df <- PETR.df %>% 
  mutate(R_petr = (PETR4 - lag(PETR4))/lag(PETR4))


mu <- mean(PETR.df$R_petr, na.rm = TRUE)
sigma <- sd(PETR.df$R_petr, na.rm = TRUE)


ggplot(na.omit(PETR.df)) + 
  # geom_density(aes(x = R_petr), fill = "blue", alpha = 0.5, colour =  "black") +
  geom_histogram(aes(x = R_petr, y=after_stat(density)), fill = "blue", alpha = 0.5, colour =  "black") +
  stat_function(mapping = aes(x = R_petr), fun = dnorm, args = list(mean = mu, sd = sigma),
                colour = "red", size = 1) + 
  theme_bw() +
  labs(title = "Retorno de PETR4",
       subtitle = "2021-01-02 to 2024-12-31",
       x = "Retorno Simples", y = "densidade de probabilidade")

ggsave(filename = "./graficos/A1 - Retorno de Petr4.png", 
       width = 16, height = 9, dpi = 100, units = "in")

