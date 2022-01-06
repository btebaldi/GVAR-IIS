#' Tese Capitulo 2 - Analise de precos de gasolina, alcool e Oleo Diesel
#' 
#' Script para geração dos Forecasts das series.
#' 
#' Author: Bruno Tebaldi de Queiroz Barbosa
#' 
#' Data: 2022-01-06
#' 



# setup -------------------------------------------------------------------

rm(list=ls())
library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)


file.name <- "forecast_result.csv"
dir <- "COM IIS - Modelo 17"


export_file <- file.path("..", "Ox Metrics GVAR","Ox Scripts", "mat_files", "Result_Matrix", dir, file.name)
main_path <- dirname(export_file)


# Load data ---------------------------------------------------------------


DX.df <- read_excel("Excel Export/DatabaseDesAdm_RA_vForecast_v3.xlsx", 
                    range = "A1:BR1108")

DX <- DX.df[,-1] %>% data.matrix()

X.df <- read_excel("Excel Export/DatabaseDesAdm_RA_vForecast_v3.xlsx", 
                   range = "A1115:BR2222")

X <- X.df[,-1] %>% data.matrix()




mLag1 <- readRDS(file.path(main_path, "mGy_inv_X_mGyL1.rds"))
mLag2 <- readRDS(file.path(main_path, "mGy_inv_X_mGyL2.rds"))
mLag3 <- readRDS(file.path(main_path, "mGy_inv_X_mGyL3.rds"))

mLag4 <- readRDS(file.path(main_path, "mGy_inv_X_mGyL4.rds"))
mLag5 <- readRDS(file.path(main_path, "mGy_inv_X_mGyL5.rds"))
mLag6 <- readRDS(file.path(main_path, "mGy_inv_X_mGyL6.rds"))

mLag7 <- readRDS(file.path(main_path, "mGy_inv_X_mGyL7.rds"))
mLag8 <- readRDS(file.path(main_path, "mGy_inv_X_mGyL8.rds"))
mLag9 <- readRDS(file.path(main_path, "mGy_inv_X_mGyL9.rds"))

mLag10 <- readRDS(file.path(main_path, "mGy_inv_X_mGyL10.rds"))
mLag11 <- readRDS(file.path(main_path, "mGy_inv_X_mGyL11.rds"))
mLag12 <- readRDS(file.path(main_path, "mGy_inv_X_mGyL12.rds"))
mLag13 <- readRDS(file.path(main_path, "mGy_inv_X_mGyL13.rds"))

# Carrega matriz de coeficiente de longo prazo
mLagLR <- readRDS(file.path(main_path, "mGy_inv_X_mL.rds"))

# Carrega matriz de coeficiente de constante e dummies sazonais
mLagDm <- readRDS(file.path(main_path, "mGy_inv_X_mC.rds"))
mLagDm <- mLagDm[, 1:12]
colnames(mLagDm) <- c("CONST", paste("M", 1:11))


#  Vetor de datas
datelist <- seq(from = as.Date("2015-01-01"),
                to = as.Date("2019-12-01"),
                by="month")


results.tbl <-  tibble(variavel=X.df$Variavel)

i=0
for(i in 0:35){
  n <- 25+i
  cat(sprintf("%s\n",datelist[n]))
  
  # resultado atual 
  results.tbl[, sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))] <- DX[,sprintf("D%dM%02d", year(datelist[n]), month(datelist[n]))]
  
  
  
  # forecast curto prazo
  Forecast.SR <- mLag1 %*% DX[, sprintf("D%dM%02d", year(datelist[n-1]), month(datelist[n-1]))] + 
    mLag2 %*% DX[,sprintf("D%dM%02d", year(datelist[n-2]), month(datelist[n-2]))] + 
    mLag3 %*% DX[,sprintf("D%dM%02d", year(datelist[n-3]), month(datelist[n-3]))] + 
    mLag4 %*% DX[,sprintf("D%dM%02d", year(datelist[n-4]), month(datelist[n-4]))] + 
    mLag5 %*% DX[,sprintf("D%dM%02d", year(datelist[n-5]), month(datelist[n-5]))] + 
    mLag6 %*% DX[,sprintf("D%dM%02d", year(datelist[n-6]), month(datelist[n-6]))] + 
    mLag7 %*% DX[,sprintf("D%dM%02d", year(datelist[n-7]), month(datelist[n-7]))] + 
    mLag8 %*% DX[,sprintf("D%dM%02d", year(datelist[n-8]), month(datelist[n-8]))] + 
    mLag9 %*% DX[,sprintf("D%dM%02d", year(datelist[n-9]), month(datelist[n-9]))] + 
    mLag10 %*% DX[,sprintf("D%dM%02d", year(datelist[n-10]), month(datelist[n-10]))] + 
    mLag11 %*% DX[,sprintf("D%dM%02d", year(datelist[n-11]), month(datelist[n-11]))] + 
    mLag12 %*% DX[,sprintf("D%dM%02d", year(datelist[n-12]), month(datelist[n-12]))] + 
    mLag13 %*% DX[,sprintf("D%dM%02d", year(datelist[n-13]), month(datelist[n-13]))]
  
  # forecast longo prazo
  Forecast.LR <- mLagLR %*% X[,sprintf("D%dM%02d", year(datelist[n-1]), month(datelist[n-1]))]
  
  # forecast constante e dummies
  Dummies <- matrix(NA, nrow = 12, ncol = 1)
  rownames(Dummies) <- c("CONST", paste("M", 1:11, sep = ""))
  
  Dummies[1,1] <- 1 # Constante
  Dummies[2:12,1] <- 0-1/12 # Constante
  
  if (!(i %in% c(11, 23, 35))){
    Dummies[month(datelist[n])+1,1] <- 1-1/12 # mes de previsao
    # cat(month(datelist[n]), "<\n")
  }
  
  Forecast.Dm <- mLagDm %*% Dummies
  Forecast <- Forecast.SR + Forecast.LR + Forecast.Dm
  
  results.tbl[, sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))] <- Forecast
  
  results.tbl[,sprintf("Error_%dM%02d", year(datelist[n]), month(datelist[n]))] <- results.tbl[[sprintf("Actual_%dM%02d", year(datelist[n]), month(datelist[n]))]] - results.tbl[[sprintf("Forecast_%dM%02d", year(datelist[n]), month(datelist[n]))]]
}



results.tbl$Regiao = str_split(results.tbl$variavel, "\\_", simplify = TRUE)[,1]
results.tbl$Tipo = str_split(results.tbl$variavel, "\\_", simplify = TRUE)[,2]

readr::write_excel_csv(results.tbl, file = export_file)
