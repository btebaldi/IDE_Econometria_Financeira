
# Setup -------------------------------------------------------------------

rm(list=ls())

library(dplyr)

ols.mdl <- lm(mpg ~ wt + qsec + am,
              data = mtcars)

summary(ols.mdl)


# Previsao ----------------------------------------------------------------

y_hat <- predict(ols.mdl)

tbl <- mtcars %>% dplyr::select(mpg) %>% 
  dplyr::mutate(mpg_hat = y_hat,
                e = mpg-mpg_hat)

# MAE & MSE & RMSE --------------------------------------------------------

MAE <- mean(abs(tbl$e))

MSE <- mean(tbl$e^2)

RMSE <-  sqrt(MSE)


# MAPE --------------------------------------------------------------------

MAPE <- mean(abs(tbl$e/tbl$mpg))*100

# summary(ols.mdl)

R2 <- 1-sum(tbl$e^2)/sum((tbl$mpg-mean(tbl$mpg))^2)
R2_adj <- 1-(1-R2)*(31)/(ols.mdl$df.residual)


