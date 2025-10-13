rm(list = ls())

library(quantmod)

getSymbols("^BVSP", from="2018-01-01")

ret <- diff(log(BVSP$BVSP.Close))

alpha <- 0.99
mu <- mean(ret, na.rm=TRUE)
sigma <- sd(ret, na.rm=TRUE)

VaR_parametrico <- (mu + qnorm(1-alpha) * sigma)
VaR_parametrico

VaR_historico <- quantile(ret, probs = 0.01, na.rm = TRUE)
VaR_historico



# Expected Shortfall (ES) -------------------------------------------------

ES_historico <- mean(ret[ret < VaR_historico], na.rm=TRUE)



# funcao esperanca
Esperanca <- function(x, mu, sigma){
  return(x * dnorm(x, mean = mu, sd = sigma))
}

# integral
Integral_cond <- integrate(f = Esperanca,
                           lower = Inf,
                           upper = VaR_parametrico,
                           mu = mean(ret, na.rm = TRUE),
                           sigma = sd(ret, na.rm = TRUE))

# Calculo condicional
ES_parametrico <- Integral_cond$value/(1-alpha)




cat("\nVAR",sprintf("\nParametrico: %0.4f\n  Historico: %0.4f\n", VaR_parametrico, VaR_historico))
cat("\nES",sprintf("\nParametrico: %0.4f\n  Historico: %0.4f\n", ES_parametrico, ES_historico))

