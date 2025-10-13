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
dataFim <- as.Date("2010-12-31")

# Data load ---------------------------------------------------------------

# Leitura dos dados de SP500 
getSymbols(c("CSCO"), auto.assign = TRUE, from = dataIni, to = dataFim)

# calcula os retornos de SP500
csco <- diff(log(CSCO$CSCO.Adjusted))

# Leitura dos dados de CAT
getSymbols(c("CAT"), auto.assign = TRUE, from = dataIni, to = dataFim)

# calcula os retornos de CAT
cat <- diff(log(CAT$CAT.Adjusted))

# Conversão dos dados em dataframe
cat.df <- tsbox::ts_data.frame(cat) %>% rename(CAT = value)
csco.df <- tsbox::ts_data.frame(csco) %>% rename(CSCO = value)

# juncao dos dados
tbl <- inner_join(x = cat.df, y = csco.df, by = c("time"="time")) %>%
  as_tibble() %>%
  na.omit() %>% 
  # rename(CAT = value.x, SP500 = value.y) %>% 
  mutate(xp = CAT + CSCO,
         xm = CAT - CSCO)

# Limpeza de variaveis nao utilizadas mais
rm(list = c("cat", "csco", "CAT", "CSCO", "cat.df", "csco.df"))


# Calculo dos modelos -----------------------------------------------------
m1 <- garchFit(~1+garch(1,1), data=tbl$xp, trace=FALSE)
summary(m1)

m2 <- garchFit(~1+garch(1,1), data=tbl$xm, trace=FALSE)
summary(m2)

mCAT <- garchFit(~1+garch(1,1), data=tbl$CAT, trace=FALSE)
summary(mCAT)

mCSCO <- garchFit(~1+garch(1,1), data=tbl$CSCO, trace=FALSE)
summary(mCSCO)


vxp <- fGarch::volatility(m1)
vxm <- fGarch::volatility(m2)
v_cat <- fGarch::volatility(mCAT)
v_csco <- fGarch::volatility(mCSCO)

tbl$Cov_t <- (vxp^2-vxm^2)/(4)
tbl$Cor_t <- tbl$Cov_t/(v_cat * v_csco)

tbl %>% 
ggplot() + 
  geom_line(aes(x = time, y = Cor_t)) + 
  labs(y = "Correlation", x = NULL, title = "Correlation CSCO vs. CAT")


if(Save_Plots){
  ggsave(filename = "./graficos/A4 - Correlation_1.png",
         width = 10, height = 4, units = "in", dpi = 100, scale = 1)
}

tbl %>% 
  ggplot() + 
  geom_line(aes(x = time, y = Cov_t)) + 
  labs(y = "Covariance", x = NULL, title = "Covariance CSCO vs. CAT")


if(Save_Plots){
  ggsave(filename = "./graficos/A4 - Covariance_1.png",
         width = 10, height = 4, units = "in", dpi = 100, scale = 1)
}

# Calculo -----------------------------------------------------------------

"EWMAvol" <- function(rtn, theta=0.94){
  # Compute the exponentially weighted moving average covariance matrix.
  #
  if(!is.matrix(rtn)){
    rtn=as.matrix(rtn)
  }
  
  S=1e-6
  # if theta to low, use 0.9
  if(theta < S){
    theta=0.9
  }
  
  # if theta to high, use 0.98
  if(theta > (1-S)){
    theta = 0.98
  }
  
  # variance
  k <- dim(rtn)[2]
  t <- dim(rtn)[1]
  V <- NULL
  for (i in 1:k){
    x <- rtn[,i]
    Mean <- mean(x^2)
    e <- (1-theta)*c(Mean,x[-t]^2)
    v <- stats::filter(e, theta, "r", init=Mean)
    V <- cbind(V,v)
  }

  for (i in 1:(k-1)){
    for (j in (i+1):k){
      x <- rtn[,i]*rtn[,j]
      Mean <- mean(x)
      e <- (1-theta)*c(Mean,x[-t])
      v <- stats::filter(e,theta,"r",init=Mean)
      V <- cbind(V,v)
    }
  }
  
  colnames(V) <- c("Var_1", "Var_2", "Cov")
  V
}

# Selecao das duas series
rtn <- tbl[, 2:3] %>% data.matrix()

# Calculo da Covariancia usando EWMA
Correl.Ewma <- EWMAvol(rtn, theta=0.94)

tbl$CovEWMA_t <- Correl.Ewma[,3]
tbl$CorEWMA_t <- Correl.Ewma[,3]/(Correl.Ewma[,1]*Correl.Ewma[,2])^0.5

tbl %>% 
  ggplot() + 
  geom_line(aes(x = time, y = Cov_t, colour = "GARCH")) + 
  geom_line(aes(x = time, y = CovEWMA_t, colour = "EWMA")) + 
  labs(y = "Covariance", x = NULL, title = "Covariance CSCO vs. CAT",
       colour = NULL) +
  theme(legend.position = "bottom")


if(Save_Plots){
  ggsave(filename = "./graficos/A4 - Covariance_Garch vs EWMA.png",
         width = 10, height = 4, units = "in", dpi = 100, scale = 1)
}

tbl %>% 
  ggplot() + 
  geom_line(aes(x = time, y = Cor_t, colour = "GARCH")) + 
  geom_line(aes(x = time, y = CorEWMA_t, colour = "EWMA")) + 
  labs(y = "Correlation", x = NULL, title = "Correlation CSCO vs. CAT",
       colour = NULL) +
  theme(legend.position = "bottom")


if(Save_Plots){
  ggsave(filename = "./graficos/A4 - Correlation_GARCH vs EWMA.png",
         width = 10, height = 4, units = "in", dpi = 100, scale = 1)
}
