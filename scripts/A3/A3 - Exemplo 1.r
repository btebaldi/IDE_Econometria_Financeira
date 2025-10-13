
# Setup -------------------------------------------------------------------
rm(list = ls())

library(fGarch)


# Data load ---------------------------------------------------------------

da <- read.table("./database/m-intcsp7309.txt", header = TRUE)
head(da)

intc <- log(da$intc+1)


acf(intc^2)
pacf(intc^2)


# Fit an ARCH(3) model
m1 <- garchFit(~ 1 + garch(3,0), data=intc, trace=FALSE) 

# Use subcommand "trace = F" to reduce the output.
summary(m1)

m2 <- garchFit(~ 1 + garch(1,0), data=intc, trace=FALSE)
summary(m2)

# Residuos 
resi <- residuals(m2, standardize=TRUE)
tdx <- c(1:444)/12+1973

# par(mfcol=c(3,10)
plot(tdx,resi,xlab="year",ylab="stand-resi",type="l")
acf(resi,lag=20)
pacf(resi^2,lag=20)

# plot(m1)
# plot(m2)




