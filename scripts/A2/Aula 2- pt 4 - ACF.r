library(dplyr)
library(ggplot2)

# AR(1) -------------------------------------------------------------------
serieAR1 <- arima.sim(n = 1000, list(ar = c(0.8897)),
          sd = 1)

forecast::Acf(serieAR1) %>% 
  forecast::autoplot() +
  labs(title = "Exemplo AR1")

ggsave(filename = "./graficos/A2 - ACF AR1.png", 
       width = 15, height = 12, dpi = 100, units = "in")


forecast::Pacf(serieAR1) %>%
  forecast::autoplot() +
  labs(title = "Exemplo AR1")

ggsave(filename = "./graficos/A2 - PACF AR1.png", 
       width = 15, height = 12, dpi = 100, units = "in")



# AR(2) -------------------------------------------------------------------
serieAR2 <- arima.sim(n = 1000, list(ar = c(0.5, 0.35)),
                      sd = 1)

forecast::Acf(serieAR2) %>% 
  forecast::autoplot() +
  labs(title = "Exemplo AR2")

ggsave(filename = "./graficos/A2 - ACF AR2.png", 
       width = 15, height = 12, dpi = 100, units = "in")


forecast::Pacf(serieAR2) %>%
  forecast::autoplot() +
  labs(title = "Exemplo AR2")

ggsave(filename = "./graficos/A2 - PACF AR2.png", 
       width = 15, height = 12, dpi = 100, units = "in")


# MA(1) -------------------------------------------------------------------

serieMA1 <- arima.sim(n = 1000, list(ma = c(-0.7)),
                      sd = 1)

forecast::Acf(serieMA1) %>% 
  forecast::autoplot() +
  labs(title = "Exemplo MA1")

ggsave(filename = "./graficos/A2 - ACF MA1.png", 
       width = 15, height = 12, dpi = 100, units = "in")


forecast::Pacf(serieMA1) %>%
  forecast::autoplot() +
  labs(title = "Exemplo MA1")

ggsave(filename = "./graficos/A2 - PACF MA1.png", 
       width = 15, height = 12, dpi = 100, units = "in")


# MA(2) -------------------------------------------------------------------

serieMA2 <- arima.sim(n = 1000, list(ma = c(-0.4, -0.35)),
                      sd = 1)

forecast::Acf(serieMA2) %>% 
  forecast::autoplot() +
  labs(title = "Exemplo MA2")

ggsave(filename = "./graficos/A2 - ACF MA2.png", 
       width = 15, height = 12, dpi = 100, units = "in")


forecast::Pacf(serieMA2) %>%
  forecast::autoplot() +
  labs(title = "Exemplo MA2")

ggsave(filename = "./graficos/A2 - PACF MA2.png", 
       width = 15, height = 12, dpi = 100, units = "in")
