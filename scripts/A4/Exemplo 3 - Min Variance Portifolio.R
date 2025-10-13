
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
getSymbols(c("BA", "CAT", "IBM", "MSFT", "PG"), auto.assign = TRUE, from = dataIni, to = dataFim)

# calcula os retornos de SP500
ba <- diff(log(BA$BA.Adjusted))
cat <- diff(log(CAT$CAT.Adjusted))
ibm <- diff(log(IBM$IBM.Adjusted))
msft <- diff(log(MSFT$MSFT.Adjusted))
pg <- diff(log(PG$PG.Adjusted))

rtn <- cbind(cat, ibm, pg) %>% na.omit()

# Cleanup
rm(list = setdiff(ls(), c("rtn", "Save_Plots")))


# Calculo do min var por bloco --------------------------------------------

tbl_1 <- rtn %>%
  as_tibble() %>% 
  mutate_all(.funs = function(x){return(NA)}) %>% 
  mutate(id = row_number(),
         block1 = if_else(id <= 756, 1,0),
         block2 = if_else(id > 756 & id <= 1512, 1,0),
         block3 = if_else(id > 1512, 1,0))


for(col in c("block1", "block2", "block3")){
  V1 <- cov(rtn[tbl_1$id[tbl_1[[col]] == 1],])
  V1inv <- solve(V1)
  One <- matrix(1,ncol(rtn),1)
  Wgt <- V1inv%*%One
  D <- sum(Wgt*One)
  Wgt <- Wgt/D
  
  for(j in seq_along(Wgt)){
    tbl_1[tbl_1[[col]] == 1, j] <- Wgt[j]
  }
}

# Calculo da min var com garch --------------------------------------------
source("./scripts/A4/GMVP.R")

# Executa o script que calcula a matriz de variancia e covariancia para cada dia
M2 <- GMVP(rtn)
str(M2)

#  transforma o resultado em uma tabela
final_table <- as_tibble(t(M2$weights))
colnames(final_table) <- colnames(rtn)

tbl <- dplyr::mutate(final_table,
                     MinVar = M2$minVariance, 
                     return = M2$returns,
                     det = M2$det)


#  Graficos ---------------------------------------------------------------


tbl %>% 
  ggplot() + 
  geom_boxplot(aes(x = det), alpha = 0.5) + 
  geom_vline(xintercept = 0, colour = "red")


tbl %>% 
  mutate(date = row_number()) %>% 
  ggplot() + 
  geom_line(aes(x = date, y = CAT.Adjusted)) +
  geom_line(aes(x = id, y = CAT.Adjusted, colour = "Block"), data = tbl_1, size = 1) +
  labs(x = NULL, y = "CAT weight", colour = NULL) +
  theme(legend.position = "bottom")

if(Save_Plots){
  ggsave(filename = "./graficos/A4 - Min Var_CAT Weight.png",
         width = 10, height = 4, units = "in", dpi = 100, scale = 1)
}

tbl %>% 
  mutate(date = row_number()) %>% 
  ggplot() + 
  geom_line(aes(x = date, y = IBM.Adjusted)) +
  geom_line(aes(x = id, y = IBM.Adjusted, colour = "Block"), data = tbl_1, size = 1) +
  labs(x = NULL, y = "IBM weight", colour = NULL) +
  theme(legend.position = "bottom")

if(Save_Plots){
  ggsave(filename = "./graficos/A4 - Min Var_IBM Weight.png",
         width = 10, height = 4, units = "in", dpi = 100, scale = 1)
}

tbl %>% 
  mutate(date = row_number()) %>% 
  ggplot() + 
  geom_line(aes(x = date, y = PG.Adjusted)) + 
  geom_line(aes(x = id, y = PG.Adjusted, colour = "Block"), data = tbl_1, size = 1) +
  labs(x = NULL, y = "PG weight", colour = NULL) +
  theme(legend.position = "bottom")

if(Save_Plots){
  ggsave(filename = "./graficos/A4 - Min Var_PG Weight.png",
         width = 10, height = 4, units = "in", dpi = 100, scale = 1)
}


final_table_var <- as_tibble(M2$variances)
colnames(final_table_var) <- colnames(rtn)
final_table_var %>% 
  mutate(date = row_number()) %>%
  ggplot() +
  geom_line(aes(x = date, y = CAT.Adjusted, colour = "CAT")) +
  geom_line(aes(x = date, y = IBM.Adjusted, colour = "IBM")) +
  geom_line(aes(x = date, y = PG.Adjusted, colour = "PG")) +
labs(x = NULL, y = "Variance", colour = NULL)

if(Save_Plots){
  ggsave(filename = "./graficos/A4 - Min Var_variances.png",
         width = 10, height = 4, units = "in", dpi = 100, scale = 1)
}

