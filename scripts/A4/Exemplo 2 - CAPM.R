# Setup -------------------------------------------------------------------
rm(list = ls())

library(fGarch)
library(dplyr)
library(ggplot2)
library(quantmod)

# TRUE = Salva os graficos
Save_Plots <- TRUE

#  Data inicial e data final da analise
dataIni <- as.Date("2001-01-01")
dataFim <- as.Date("2013-01-01")

# Data load ---------------------------------------------------------------

# Leitura dos dados de SP500 
getSymbols(c("^GSPC"), auto.assign = TRUE, from = dataIni, to = dataFim)

# calcula os retornos de SP500
sp500 <- diff(log(GSPC$GSPC.Adjusted))

# Leitura dos dados de CAT
getSymbols(c("CAT"), auto.assign = TRUE, from = dataIni, to = dataFim)

# calcula os retornos de CAT
cat <- diff(log(CAT$CAT.Adjusted))

# Conversão dos dados em dataframe
cat.df <- tsbox::ts_data.frame(cat) %>% rename(CAT = value)
sp500.df <- tsbox::ts_data.frame(sp500) %>% rename(SP500 = value)

# juncao dos dados
tbl <- inner_join(x = cat.df, y = sp500.df, by = c("time"="time")) %>%
  as_tibble() %>%
  na.omit() %>% 
  # rename(CAT = value.x, SP500 = value.y) %>% 
  mutate(xp = CAT + SP500,
         xm = CAT - SP500)


# Calculo dos modelos -----------------------------------------------------
m1 <- garchFit(~1+garch(1,1), data=tbl$xp, trace=FALSE)
summary(m1)

m2 <- garchFit(~1+garch(1,1), data=tbl$xm, trace=FALSE)
summary(m2)

m3 <- garchFit(~1+garch(1,1), data=tbl$SP500, trace=FALSE)
summary(m3)

vxp <- fGarch::volatility(m1)
vxm <- fGarch::volatility(m2)
vsp5 <- fGarch::volatility(m3)


# Calculo -----------------------------------------------------------------

beta <- (vxp^2-vxm^2)/(4*vsp5^2)

tbl$beta <- beta


# Beta CAPM classico ------------------------------------------------------

m4 <- lm(CAT ~ SP500, data = tbl)
summary(m4)

tbl$betaCAPM = m4$coefficients["SP500"]

plot(beta, xlab="year", ylab="beta", type="l")
abline(h=c(m4$coefficients["SP500"]))

tbl %>% 
ggplot() + 
  geom_line(aes(x = time, y = beta)) + 
  geom_line(aes(x = time, y = betaCAPM), colour="red") +
labs(y = "Beta", x = "Time")

if(Save_Plots){
  ggsave(filename = "./graficos/A4 - Beta variando_1.png",
         width = 10, height = 4, units = "in", dpi = 100, scale = 1)
}


tbl %>% 
  select(time, CAT, SP500, beta) %>% 
  tidyr::pivot_longer(cols = c("CAT", "SP500", "beta")) %>% 
  ggplot() + 
  geom_line(aes(x = time, y = value, group = name)) + 
  facet_wrap(~name, scales = "free", ncol = 1) +
  labs(y=NULL)

if(Save_Plots){
  ggsave(filename = "./graficos/A4 - Beta variando_2.png",
         width = 10, height = 4, units = "in", dpi = 100, scale = 1)
}

tbl$time[which.min(tbl$CAT)]
