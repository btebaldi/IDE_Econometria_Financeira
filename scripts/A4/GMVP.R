library(fGarch)

"GMVP" <- function(rtn,start=500){
  # compute the weights and variance of global minimum variance portfolio.
  # The time period is from (start) to end of the data.
  #
  # uses cov(x,y) = [var(x+y)-var(x-y)]/4.
  
  if(!is.matrix(rtn)){
    rtn <- as.matrix(rtn)
  }
  
  TotalTime <- dim(rtn)[1]
  k <- dim(rtn)[2]
  wgt <- NULL
  mVar <- NULL
  VAR <- NULL
  ONE <- matrix(1,k,1)
  prtn <- NULL
  Det <- NULL
  
  Cov_Matrix <- array(NA, c(k, k, TotalTime));
  
  
  # for (t in start:T){
  # estimate variances and covariances at time "t".
  # COV <- matrix(0,k,k)
  i=1
  for (i in 1:k){
    
    # Calcula a volatilidade do ativo 1
    m1 <- garchFit(~1+garch(1,1),data=rtn[ ,i],trace=F)
    Cov_Matrix[i,i,] <- fGarch::volatility(m1)^2
    
    # Calcula a Covariancia do ativo i contra os ativos j
    if(i < k){
      for (j in (i+1):k){
        x <- rtn[ ,i]+rtn[ ,j]
        y <- rtn[ ,i]-rtn[ ,j]
        m2 <- garchFit(~1+garch(1,1),data=x,trace=F)
        m3 <- garchFit(~1+garch(1,1),data=y,trace=F)
        v2 <- fGarch::volatility(m2)
        v3 <- fGarch::volatility(m3)
        Cov_Matrix[j, i, ] <- (v2^2-v3^2)/4
        Cov_Matrix[i, j, ] <- Cov_Matrix[j,i,]
        
        # end of j-loop
        print(sprintf("Estimated Cov %d vs %d", i, j))
      }
      # end of (if-statement)
    }
    # end of i-loop
    print(sprintf("End of asset %d", i))
  }
  
  for(i in seq_len(dim(Cov_Matrix)[3])){
    
    COV <- Cov_Matrix[, , i]
    if(!(matrixcalc::is.positive.definite(COV))){
      print("MATRIX NAO POSITIVA DEFINIDA!!!!!!!!")
    }
    
    Det <- c(Det,det(COV))
    V <- solve(COV)
    VAR <- rbind(VAR,diag(COV))
    
    Psi <- V%*%ONE
    W <- sum(ONE*Psi)
    Psi <- Psi/W
    wgt <- cbind(wgt,Psi)
    mVar <- c(mVar,1/W)
    # if(i < TotalTime){
      prtn=c(prtn, as.matrix(rtn[i,]) %*% Psi)
    # }
  }
  
  # }
  
  GMVP <- list(Cov_Matrix = Cov_Matrix, 
               weights = wgt, 
               minVariance = mVar,
               variances = VAR, 
               returns = prtn,
               det = Det)
  
  return(GMVP)
}
