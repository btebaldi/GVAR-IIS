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


file.name <- "Erro_GVARIIS.rds"
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

brent <- tbl %>% filter(year(DATA_INICIAL) < 2019) %>% select(brent)
D.brent <- mDiff(brent$brent)
sd(brent$brent)
sd(D.brent, na.rm = TRUE)
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
                              sort(rep(1:110, 3)),
                              rep(c("ETANOL_HIDRATADO", "OLEO_DIESEL", "GASOLINA_COMUM"), 4),
                              sep = "_")


DX.df <- tbl %>%
  select("brent", colunas_Selecionadas) %>%
  mutate_if(.predicate = is.numeric, .funs = mDiff)

DX <- DX.df %>% data.matrix()
dim(DX)
DX <- DX[-1,]
dim(DX)

X.df <- tbl %>% select("brent", colunas_Selecionadas)

X <- X.df %>% data.matrix()
dim(X)
X <- X[-1,]
dim(X)


# Leitura das matrizes de lag.
mLag1 <- readRDS(file.path(main_path, "mGy_inv_X_mGyL1.rds"))
mLag2 <- readRDS(file.path(main_path, "mGy_inv_X_mGyL2.rds"))
mLag3 <- readRDS(file.path(main_path, "mGy_inv_X_mGyL3.rds"))

mLag4 <- readRDS(file.path(main_path, "mGy_inv_X_mGyL4.rds"))
mLag5 <- readRDS(file.path(main_path, "mGy_inv_X_mGyL5.rds"))
mLag6 <- readRDS(file.path(main_path, "mGy_inv_X_mGyL6.rds"))

mLag7 <- readRDS(file.path(main_path, "mGy_inv_X_mGyL7.rds"))
mLag8 <- readRDS(file.path(main_path, "mGy_inv_X_mGyL8.rds"))

# Carrega matriz de coeficiente de longo prazo
mLagLR <- readRDS(file.path(main_path, "mGy_inv_X_mL.rds"))

# Carrega matriz de coeficiente de constante e dummies sazonais
mLagDm <- readRDS(file.path(main_path, "mGy_inv_X_mC.rds"))

#  seleciona as colunas: constante + dummies (51)
dim(mLagDm)
mLagDm <- mLagDm[, 1:52]

colnames(mLagDm) <-  c("CONST", "Seasonal", paste("Seasonal", 1:50, sep = ""))

# Forecast short run lag 1
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

FSR_1 <- FSR_1[8:(total_rows-1),]
FSR_2 <- FSR_2[7:(total_rows-2),]
FSR_3 <- FSR_3[6:(total_rows-3),]
FSR_4 <- FSR_4[5:(total_rows-4),]
FSR_5 <- FSR_5[4:(total_rows-5),]
FSR_6 <- FSR_6[3:(total_rows-6),]
FSR_7 <- FSR_7[2:(total_rows-7),]
FSR_8 <- FSR_8[1:(total_rows-8),]


FSR = FSR_1 + FSR_2 + FSR_3 + FSR_4 + FSR_5 + FSR_6 + FSR_7 + FSR_8


# Forecast de long Run
FLR_1 = X %*% t(mLagLR)
FLR_1 <- FLR_1[8:(total_rows-1),]

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
Forecast.Dm <- Forecast.Dm[9:(total_rows),]

Forecast <- FSR + FLR_1 + Forecast.Dm


results.tbl <- tbl %>%
  select("DATA_INICIAL", "brent", colunas_Selecionadas) %>%
  mutate_if(.predicate = is.numeric, .funs = mDiff) %>%
  filter(row_number() > 9)

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


library(ggplot2)


regiao <- c("São Paulo" = 75,
            "Rio de Janeiro" = 62,
            "Dist. Federal" = 109,
            "Belo Horizonte" = 46,
            "Salvador" = 39)
i <- 1
for(i  in seq_along(regiao)){

  g1 <- results.tbl %>% 
    select(date="DATA_INICIAL",
           actual=sprintf("R_%d_ETANOL_HIDRATADO", regiao[i]),
           forecast=sprintf("Forecast_R_%d_ETANOL_HIDRATADO", regiao[i])) %>% 
    ggplot() + 
    geom_line(aes(x=date, y = actual, colour= "Actual")) +
    geom_line(aes(x=date, y = forecast, colour= "Forecast")) +
    theme_bw() +
    theme(legend.position="bottom") +
    scale_colour_manual(values=c(Actual="#000000",Forecast="#FF0000"))+
    labs(title = sprintf("%s", names(regiao)[i]),
         subtitle = "Prediction of Etanol Hidratado",
         # subtitle = "Impulse Response Function - Shor run effects",
         colour = NULL,
         y=NULL,
         x=NULL,
         caption = "Elaborated by the author")
  
  ggsave(filename = sprintf("./Graficos/Forecast GVAR-IIS - Etanol - %s.png", names(regiao)[i]),
         plot = g1,
         units = "in",
         width = 8, height = 6,
         dpi = 100)
  
  g1 <- results.tbl %>% 
    select(date="DATA_INICIAL",
           actual=sprintf("R_%d_OLEO_DIESEL", regiao[i]),
           forecast=sprintf("Forecast_R_%d_OLEO_DIESEL", regiao[i])) %>% 
    ggplot() + 
    geom_line(aes(x=date, y = actual, colour= "Actual")) +
    geom_line(aes(x=date, y = forecast, colour= "Forecast")) +
    theme_bw() +
    theme(legend.position="bottom") +
    scale_colour_manual(values=c(Actual="#000000",Forecast="#FF0000"))+
    labs(title = sprintf("%s", names(regiao)[i]),
         subtitle = "Prediction of Oleo Diesel",
         # subtitle = "Impulse Response Function - Shor run effects",
         colour = NULL,
         y=NULL,
         x=NULL,
         caption = "Elaborated by the author")
  
  ggsave(filename = sprintf("./Graficos/Forecast GVAR-IIS - Diesel - %s.png", names(regiao)[i]),
         plot = g1,
         units = "in",
         width = 8, height = 6,
         dpi = 100)
  
  g1 <- results.tbl %>% 
    select(date="DATA_INICIAL",
           actual=sprintf("R_%d_GASOLINA_COMUM", regiao[i]),
           forecast=sprintf("Forecast_R_%d_GASOLINA_COMUM", regiao[i])) %>% 
    ggplot() + 
    geom_line(aes(x=date, y = actual, colour= "Actual")) +
    geom_line(aes(x=date, y = forecast, colour= "Forecast")) +
    theme_bw() +
    theme(legend.position="bottom") +
    scale_colour_manual(values=c(Actual="#000000",Forecast="#FF0000"))+
    labs(title = sprintf("%s", names(regiao)[i]),
         subtitle = "Prediction of Gasolina Comum",
         # subtitle = "Impulse Response Function - Shor run effects",
         colour = NULL,
         y=NULL,
         x=NULL,
         caption = "Elaborated by the author")
  
  ggsave(filename = sprintf("./Graficos/Forecast GVAR-IIS - Gasolina -%s.png", names(regiao)[i]),
         plot = g1,
         units = "in",
         width = 8, height = 6,
         dpi = 100)
  
}  



for(i in 1:110){
  for(serie in c("ETANOL_HIDRATADO","OLEO_DIESEL","GASOLINA_COMUM") ){
    coluna_actual <- sprintf("R_%d_%s",i, serie)
    coluna_forecast <- sprintf("Forecast_R_%d_%s",i, serie)
    coluna_erro <- sprintf("Erro_R_%d_%s",i, serie)
    
    results.tbl[coluna_erro] <- results.tbl[[coluna_actual]] - results.tbl[[coluna_forecast]]
  }
}


tbl.erro <- results.tbl %>% 
  select("DATA_INICIAL", starts_with("Erro")) %>% 
  pivot_longer(cols = -DATA_INICIAL, names_to = "Serie", values_to = "Erro_GVARIIS") %>% 
  mutate(SE_GVARIIS = Erro_GVARIIS^2)


saveRDS(object = tbl.erro, file = export_file)
