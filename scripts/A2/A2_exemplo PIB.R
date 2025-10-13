
# Setup -------------------------------------------------------------------

rm(list = ls())

library(readxl)
library(tidyverse)
library(forecast)

# Data load ---------------------------------------------------------------

# tbl <- read_excel("./database/GDP USA.xlsx", sheet = "Quarterly")
tbl <- read_delim("database/PIB Brasil.csv", 
                  delim = ";", escape_double = FALSE, 
                  locale = locale(decimal_mark = ","), 
                  trim_ws = TRUE)
tbl$Data <- lubridate::yq(tbl$Data)

gdp.ts <- ts(log(tbl$PIB), frequency = 4)

forecast::autoplot(gdp.ts)

# Verifica estacionariedade -----------------------------------------------

library(urca)

urca::ur.df(gdp.ts) %>% summary()
urca::ur.df(diff(gdp.ts)) %>% summary()


D.gdp.ts <- diff(gdp.ts)

gdp.ts %>% forecast::autoplot()
D.gdp.ts %>% forecast::autoplot()


# Selecao de ARMA ---------------------------------------------------------

forecast::Acf(D.gdp.ts)
forecast::Pacf(D.gdp.ts)

ARIMA_1 <- forecast::Arima(D.gdp.ts, seasonal = c(1,0,0))

ARIMA_1$residuals %>% forecast::Acf()
ARIMA_1$residuals %>% forecast::Pacf()

ARIMA_2 <- forecast::Arima(D.gdp.ts, order = c(4,0,0),seasonal = c(1,0,0))

ARIMA_2$residuals %>% forecast::Acf()
ARIMA_2$residuals %>% forecast::Pacf()

ARIMA_3 <- forecast::Arima(D.gdp.ts, order = c(0,0,4),seasonal = c(1,0,0))

ARIMA_3$residuals %>% forecast::Acf()
ARIMA_3$residuals %>% forecast::Pacf()



# Auto arima --------------------------------------------------------------

ARIMA_at <- forecast::auto.arima(D.gdp.ts)



forecast::auto.arima(D.gdp.ts, stepwise = FALSE, trace = TRUE,
                     approximation = FALSE)



BIC(ARIMA_at, ARIMA_1, ARIMA_2, ARIMA_3)
AIC(ARIMA_at, ARIMA_1, ARIMA_2, ARIMA_3)

