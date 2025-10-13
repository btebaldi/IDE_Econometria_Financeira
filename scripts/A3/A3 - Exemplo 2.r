# Setup -------------------------------------------------------------------
rm(list = ls())

library(fGarch)


# Data load ---------------------------------------------------------------

da <- read.table("./database/m-intcsp7309.txt", header = TRUE)
head(da)

intc <- log(da$intc+1)


# Modelo ARCH -------------------------------------------------------------

m2 <- garchFit(~ 1 + garch(1,0), data=intc, trace=FALSE)
summary(m2)

# Residuos 
resi <- residuals(m2, standardize=TRUE)

# Model checking for the ARCH(1) model with Gaussian innovations shows that the
# model needs some refinement.
Box.test(resi^2,lag=20,type="Ljung")




# Modelo GARCH ------------------------------------------------------------

m4=garchFit(~1+garch(1,1),data=intc,trace=F)
summary(m4)


# % Obtain volatility
v1=volatility(m4)

# % Standardized residuals
resi=residuals(m4,standardize=T)

vol=ts(v1,frequency=12,start=c(1973,1))
res=ts(resi,frequency=12,start=c(1973,1))

# par(mfcol=c(2,1)) % Show volatility and residuals
plot(vol,xlab="year",ylab="volatility",type="l")
plot(res,xlab="year",ylab="st. resi",type="l")

# par(mfcol=c(2,2)) % Obtain ACF & PACF
acf(resi,lag=24)
pacf(resi,lag=24)
acf(resi^2,lag=24)
pacf(resi^2,lag=24)

# % Obtain plot of predictive intervals
# par(mfcol=c(1,1))
upp=0.0113+2*v1
low=0.0113-2*v1
tdx=c(1:444)/12+1973
plot(tdx,intc,xlab="year",ylab="series",type="l",ylim=c(-0.6,0.6))
lines(tdx,upp,lty=2,col="red")
lines(tdx,low,lty=2,col="red")
abline(h=c(0.0113))

plot(m4)
# Opcao 3
# Opcao 13


# % Student-t innovations
m5 <- garchFit(~1+garch(1,1), data=intc, trace=FALSE, cond.dist="std")
summary(m5)

v2=volatility(m5)

m6=garchFit(~1+garch(1,1),data=intc,trace=F,cond.dist="sstd")
summary(m6)

v3=volatility(m6)
par(mfcol=c(3,1))
plot(tdx,v1,xlab="year",ylab="volatility",type="l",ylim=c(0.06,0.3))
title(main="(a) Gaussian")
plot(tdx,v2,xlab="year",ylab="volatility",type="l",ylim=c(0.06,0.3))
title(main="(b) Student-t")
plot(tdx,v3,xlab="year",ylab="volatility",type="l",ylim=c(0.06,0.3))
title(main="(c) Skew Student-t")

par(mfcol=c(1,1))

plot(m6)
#  opcoes
# 10
# 11
# 13

