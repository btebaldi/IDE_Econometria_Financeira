
# Setup -------------------------------------------------------------------
rm(list = ls())

library(readr)

# User defined Functions --------------------------------------------------

archTest <- function(rtn, m=10){
  # Perform Lagrange Multiplier Test for ARCH effect of a time series
  # rtn: time series
  # m: selected AR order
  
  # calcula os residuos ao quadrado
  y=(rtn-mean(rtn))^2
  
  # Tamanho da serie 
  t=length(rtn)
  
  # Cria uma matrix com m defasagens
  atsq <- y[(m+1):t]
  
  # Matriz de defasagens
  x <- matrix(0,(t-m),m)
  
  for (i in 1:m){
    x[, i] = y[(m+1-i):(t-i)]
  }
  
  colnames(x) <- sprintf("Lag_%s", m:1)
  md <- lm(atsq~x)
  
  print(summary(md))

  return(md)
}


# Data Load ---------------------------------------------------------------

# carrega dado da Intel corporation
database = read_table("./database/m-intcsp7309.txt")
head(database)

# Calcula o log retorno
intc = log(database$intc+1)

# Cria um objeto de serie de tempo com frequencia
rtn = ts(intc, frequency=12, start=c(1973,1))

# time plot
plot(rtn, type="l", xlab="year", ylab="ln-rtn") 


# testing the mean of returns
t.test(intc)

# Calcula o teste Box–Pierce or Ljung–Box para examinar se os dados são
# independentes. (H0: os resíduos não são autocorrelacionados)
Box.test(intc, lag=12,type="Ljung")


# ACF plots
acf(intc,lag=24) 
acf(abs(intc),lag=24)

# Rejeito H0 para o absoluto dos residuos
Box.test(abs(intc),lag=12,type="Ljung")


# Teste para a "volatilidade realizada"
y=intc-mean(intc)
Box.test(y^2,lag=12,type="Ljung")


archTest(y, 12) #% output edited.
