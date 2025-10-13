
# Setup -------------------------------------------------------------------
rm(list = ls())

library(quantmod)
library(dplyr)

PETRO <- getSymbols("PETR4.SA", auto.assign = FALSE)

getSymbols("AAPL",from="2005-01-02", to="2010-12-31")

# tsbox::ts_df(PETRO) %>% 
#   tidyr::pivot_wider(id_cols = c("time"),
#                      names_from = "id",
#                      values_from = "value")

PETRO.df <- as.data.frame(PETRO)
PETRO.df$Date <- as.Date(row.names(PETRO.df))

writexl::write_xlsx(PETRO.df, path = "C:/Users/bteba/Downloads/teste.xlsx")

colnames(PETRO.df) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Date")

PETRO.df %>%
  mutate(Rt = (Close - dplyr::lag(Close))/dplyr::lag(Close),
         rt = log(Close/dplyr::lag(Close)))


