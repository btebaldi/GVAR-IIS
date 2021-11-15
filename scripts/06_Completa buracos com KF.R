#' Author: Bruno Tebaldi Barbosa
#'
#' 2021-11-09
#'
#' Script que faz a preparacao dos dados (com buraco) para a regressao do GVAR 
#' 
#' 
# Setup -------------------------------------------------------------------

rm(list = ls())

library(readr)
library(dplyr)
library(tidyr)
library(dlm)


# User defined Functions --------------------------------------------------

source("../Statistics/KalmanFilter/lib/Kalman.r")

fn <- function(params){
  dlmModPoly(order= 1, dV= exp(params[1]) , dW = exp(params[2]))
}

# Data Load ---------------------------------------------------------------

db_com_buraco <- readRDS("./database/db_Ox_com_buraco.rds")

colnames(db_com_buraco)

# i = "R_1101_ETANOL_HIDRATADO"
for (i in colnames(db_com_buraco)) {
  
  # se for a coluna de data inicial ou data final
  if(i %in% c("DATA_INICIAL", "DATA_FINAL")){
    next()
  }
  
  # busca os dados da coluna
  data.2 <- db_com_buraco[[i]]
  
  # Fita o modelo de nivel local
  fit <- dlmMLE(data.2, rep(0,2), fn)
  
  # busca os parametros do modelo estimado
  mod <- fn(fit$par)
  
  # Filtra o modelo com os parrametros estimados
  filtered <- dlmFilter(data.2, mod)
  
  # Faz smooth
  smoothed <- dlmSmooth(filtered)
  
  # retira a primeira observacao
  mu <- dropFirst(smoothed$s)
  
  idx <- which(is.na(data.2))
  db_com_buraco[idx, i] <- mu[idx]
  
}


for (col in colnames(db_com_buraco)) {
  if( sum(is.na(db_com_buraco[[col]])) > 0 ){
    cat(sprintf("%s", col),  "\n")
  }
}

saveRDS(object = db_com_buraco, file = "./database/db_Ox_sem_buraco.rds")


readr::write_csv(x = db_com_buraco, file = "./export/database for ox/db_Ox_sem_buraco.csv")






