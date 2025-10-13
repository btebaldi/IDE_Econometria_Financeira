# Setup -------------------------------------------------------------------
rm(list = ls())

library(quantmod)
library(dplyr)
library(ggplot2)

# Calculo do retorno simples de cada ação ---------------------------------

getSymbols(c("INTC"), auto.assign = TRUE, from="1973-01-02", to="2009-12-31")

da = read.table("C:/Users/bteba/Downloads/Historico/2025-09-17/ch4data/m-intcsp7309.txt",header=T)
head(da)


intc <- log(da$intc+1)
plot(intc, type = "l")

Acf(intc)
Pacf(intc)

Acf(intc^2)
Pacf(intc^2)


# Fit an ARCH(3) model
m1 <- garchFit(~1+garch(3,0),data=intc,trace=F) 
# Use subcommand "trace = F" to reduce the output.
summary(m1)


m2 <- garchFit(~1+garch(1,0),data=intc,trace=F) 
summary(m2)


Acf(residuals(m2, standardize=T)^2)
acf(residuals(m2))
plot(m2)

INTC$INTC.Close %>% forecast::autoplot()

INTC$INTC.Close %>% diff() %>% forecast::autoplot()

rtn <- log(INTC$INTC.Close/lag(INTC$INTC.Close))

rtn %>% forecast::autoplot()


plot(rtn, type="l",xlab="year",ylab="ln-rtn") #% time plot

# Avaliar se os retornos seguem uma distribuição t-student (a conclusão sera que
# nao seguem)
t.test(as.numeric(INTC$INTC.Close))

#  Avaliar se osresiduos são independentes (a conclusao é que nao são)
Box.test(INTC$INTC.Close, lag=12, type="Ljung")

# Se nao temos independencia dos residuos o que está aconcendo? Vamos avaliar o
# ACF para que possamos ter uma ideia da correlação dos ressiduos
na.omit(rtn) %>% acf()

# Vamos analissar o retorno em absoluto
acf(na.omit(abs(rtn)))

# ARCH --------------------------------------------------------------------

library(fGarch) #% Load package
# > da=read.table("m-intcsp7309.txt",header=T)
# > head(da)
# date intc sp
# 1 19730131 0.010050 -0.017111
# ....
# 6 19730629 0.133333 -0.006575
# > intc=log(da$intc+1)

# Fit an ARCH(3) model
m1 <- garchFit(~1+garch(3,0),data=intc,trace=F) 
# Use subcommand "trace = F" to reduce the output.
> summary(m1)
Title: GARCH Modelling
Mean and Variance Equation: data ∼ 1+garch(3,0) [data=intc]
Conditional Distribution: norm
Coefficient(s):
  Estimate Std. Error t value Pr(>|t|)
mu 0.012567 0.005515 2.279 0.0227 *
  omega 0.010421
da=read.table("m-intcsp7309.txt",header=T)
> head(da)
date intc sp
1 19730131 0.010050 -0.017111
...
> intc=log(da$intc+1)
> rtn=ts(intc,frequency=12,start=c(1973,1))
> plot(rtn,type=’l’,xlab=’year’,ylab=’ln-rtn’) % time plot
> t.test(intc) % testing the mean of returns
One Sample t-test
data: intc
t = 2.3788, df = 443, p-value = 0.01779
alternative hypothesis: true mean is not equal to 0
> Box.test(intc,lag=12,type=’Ljung’)
Box-Ljung test
data: intc
X-squared = 18.6761, df = 12, p-value = 0.09665
> par(mfcol=c(2,1))
> acf(intc,lag=24) % ACF plots
> acf(abs(intc),lag=24)
> Box.test(abs(intc),lag=12,type=’