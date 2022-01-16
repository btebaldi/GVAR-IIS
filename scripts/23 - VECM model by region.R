#' Tese Capitulo 2 - Analise de precos de gasolina, alcool e Oleo Diesel
#' 
#' Script para geração dos Forecasts das series utilizando um VECM por regiao
#' 
#' Author: Bruno Tebaldi de Queiroz Barbosa
#' 
#' Data: 2022-01-06
#' 


rm(list=ls())
library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(tsDyn)


file.name <- "forecast_result.csv"
dir <- "Result 1"


export_file <- file.path("Ox", "mat_files", "Result_Matrix", dir, file.name)
main_path <- dirname(export_file)




# User defined functions --------------------------------------------------

mDiff <- function(x){
  ret <- x - dplyr::lag(x)
  return(ret)
}



# Load data ---------------------------------------------------------------

# Leitura do banco de dados para construção da tabela de forecasting
tbl <- readRDS(file = "./database/db_oil_forForecast.rds")


# Seleciona o periodo de forecasting
# tbl <- tbl %>% filter(year(DATA_FINAL) >= 2019)

# Adiciona datas faltantes. (Dados faltantes devido a lockdown da pandemia.)
tbl <- tbl %>% add_row(DATA_INICIAL = as.Date("2020-08-23"), DATA_FINAL = as.Date("2020-08-29"))
tbl <- tbl %>% add_row(DATA_INICIAL = as.Date("2020-08-30"), DATA_FINAL = as.Date("2020-09-05"))
tbl <- tbl %>% add_row(DATA_INICIAL = as.Date("2020-09-06"), DATA_FINAL = as.Date("2020-09-12"))
tbl <- tbl %>% add_row(DATA_INICIAL = as.Date("2020-09-13"), DATA_FINAL = as.Date("2020-09-19"))
tbl <- tbl %>% add_row(DATA_INICIAL = as.Date("2020-09-20"), DATA_FINAL = as.Date("2020-09-26"))
tbl <- tbl %>% add_row(DATA_INICIAL = as.Date("2020-09-27"), DATA_FINAL = as.Date("2020-10-03"))
tbl <- tbl %>% add_row(DATA_INICIAL = as.Date("2020-10-04"), DATA_FINAL = as.Date("2020-10-10"))
tbl <- tbl %>% add_row(DATA_INICIAL = as.Date("2020-10-11"), DATA_FINAL = as.Date("2020-10-17"))

#  ordena as datas
tbl <- tbl %>% arrange(DATA_INICIAL)

#  Colunas selecionadas para a regressao
colunas_Selecionadas <- paste("R",
                              rep(11, 3),
                              c("ETANOL_HIDRATADO", "OLEO_DIESEL", "GASOLINA_COMUM"),
                              sep = "_")


# Selecao de dados para o VECM
y <- tbl %>% 
  filter(year(DATA_FINAL) < 2019) %>%
  select(colunas_Selecionadas)
  
brent <- tbl %>% 
  filter(year(DATA_FINAL) < 2019) %>% 
  select("brent")


mdl <- VECM(data = y,
     lag = 8,
     r=1, 
     include = "const",
     beta = NULL,
     estim = "ML",
     LRinclude = "none",
     exogen = brent$brent)

# summary(mdl)

# Determina matrizes

# Determina coeficientes de longo prazo
mLagLR <- mdl$coefficients[,"ECT"] %*% t(mdl$model.specific$beta)


j=11
# determina coeficientes de Lag
mLag1 <- mdl$coefficients[, c(sprintf("R_%d_ETANOL_HIDRATADO -1", j), sprintf("R_%d_GASOLINA_COMUM -1", j), sprintf("R_%d_OLEO_DIESEL -1", j))]
mLag2 <- mdl$coefficients[, c(sprintf("R_%d_ETANOL_HIDRATADO -2", j), sprintf("R_%d_GASOLINA_COMUM -2", j), sprintf("R_%d_OLEO_DIESEL -2", j))]
mLag3 <- mdl$coefficients[, c(sprintf("R_%d_ETANOL_HIDRATADO -3", j), sprintf("R_%d_GASOLINA_COMUM -3", j), sprintf("R_%d_OLEO_DIESEL -3", j))]
mLag4 <- mdl$coefficients[, c(sprintf("R_%d_ETANOL_HIDRATADO -4", j), sprintf("R_%d_GASOLINA_COMUM -4", j), sprintf("R_%d_OLEO_DIESEL -4", j))]
mLag5 <- mdl$coefficients[, c(sprintf("R_%d_ETANOL_HIDRATADO -5", j), sprintf("R_%d_GASOLINA_COMUM -5", j), sprintf("R_%d_OLEO_DIESEL -5", j))]
mLag6 <- mdl$coefficients[, c(sprintf("R_%d_ETANOL_HIDRATADO -6", j), sprintf("R_%d_GASOLINA_COMUM -6", j), sprintf("R_%d_OLEO_DIESEL -6", j))]
mLag7 <- mdl$coefficients[, c(sprintf("R_%d_ETANOL_HIDRATADO -7", j), sprintf("R_%d_GASOLINA_COMUM -7", j), sprintf("R_%d_OLEO_DIESEL -7", j))]
mLag8 <- mdl$coefficients[, c(sprintf("R_%d_ETANOL_HIDRATADO -8", j), sprintf("R_%d_GASOLINA_COMUM -8", j), sprintf("R_%d_OLEO_DIESEL -8", j))]

# Determina coeficientes de constante 
mLagDm <- mdl$coefficients[,"Intercept"]

# Determina coeficientes de Exogenas
mExo <- mdl$coefficients[,"exo_1"]

DX.df <- tbl %>%
  select(colunas_Selecionadas) %>%
  mutate_if(.predicate = is.numeric, .funs = mDiff)

DX <- DX.df %>% data.matrix()
DX <- DX[-1,]

X.df <- tbl %>% select(colunas_Selecionadas)
X <- X.df %>% data.matrix()
dim(X)
X <- X[-1,]
dim(X)

Exo.df <- tbl %>% select("brent")
Exo <- Exo.df %>% data.matrix()

# Forecast
FSR_1 <- DX %*% t(mLag1)
FSR_2 <- DX %*% t(mLag2)
FSR_3 <- DX %*% t(mLag3)
FSR_4 <- DX %*% t(mLag4)
FSR_5 <- DX %*% t(mLag5)
FSR_6 <- DX %*% t(mLag6)
FSR_7 <- DX %*% t(mLag7)
FSR_8 <- DX %*% t(mLag8)


#  ajuste dos lags.
total_rows <- nrow(DX)

FSR_1 <- FSR_1[12:(total_rows-1),]
FSR_2 <- FSR_2[11:(total_rows-2),]
FSR_3 <- FSR_3[10:(total_rows-3),]
FSR_4 <- FSR_4[9:(total_rows-4),]
FSR_5 <- FSR_5[8:(total_rows-5),]
FSR_6 <- FSR_6[7:(total_rows-6),]
FSR_7 <- FSR_7[6:(total_rows-7),]
FSR_8 <- FSR_8[5:(total_rows-8),]

FSR = FSR_1 + FSR_2 + FSR_3 + FSR_4 + FSR_5 + FSR_6 + FSR_7 + FSR_8



# Forecast de long Run
FLR_1 = X %*% t(mLagLR)
FLR_1 <- FLR_1[12:(total_rows-1),]

# forecast constante e dummies
Dummies <- matrix(NA, nrow = 52, ncol = 1)
rownames(Dummies) <- c("CONST", "Seasonal", paste("Seasonal", 1:50, sep = ""))

Dummies[1,1] <- 1 # Constante
Dummies[2:52,1] <- 0-1/52 # Constante


Forecast.Dm <- matrix(NA, nrow = nrow(X), ncol = ncol(X))

season <- 5 # A sazonalidade da amostra começa no periodo 5
for(i in seq_len(nrow(Forecast.Dm))) {
  Dummies[2:52,1] <- 0-1/52 # Constante
  if(season < 52){
    Dummies[season+1,1] <- 1-1/52
  }
  
  Forecast.Dm[i, ] <- mLagDm %*% Dummies
  
  if(season == 52){
    season <- 1
  } else{
    season <- season + 1
  }
}

# Ajuste para os lags
Forecast.Dm <- Forecast.Dm[13:(total_rows),]

Forecast <- FSR + FLR_1 + Forecast.Dm

# Forecast <- FSR + Forecast.Dm
# Forecast <- FSR

results.tbl <- DX.df %>% filter(row_number() > 13)

colnames(Forecast) <- c("F_Brent",
                        paste("R",
                              sort(rep(1:110,3)),
                              c("ETANOL_HIDRATADO","OLEO_DIESEL","GASOLINA_COMUM"),
                              sep = "_"))
i <- 1
for(i in 1:110){
  col <- paste("R", i, c("ETANOL_HIDRATADO", "OLEO_DIESEL", "GASOLINA_COMUM"), sep = "_")
  results.tbl[, paste("Forecast", col, sep = "_") ] <- Forecast[, col]
  
}

results.tbl %>% select(starts_with("R_75_"), starts_with("Forecast_R_75_"))





