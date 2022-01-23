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


file.name <- "Erro_VECM_wCambio.rds"
dir <- "VECM w Cambio"


export_file <- file.path("Ox", "mat_files", "Result_Matrix", dir, file.name)
main_path <- dirname(export_file)




# User defined functions --------------------------------------------------

mDiff <- function(x){
  ret <- x - dplyr::lag(x)
  return(ret)
}



# Load data ---------------------------------------------------------------

# Leitura do banco de dados para construção da tabela de forecasting
tbl <- readRDS(file = "./database/db_oil_forForecast2.rds")


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



# Planilha de resultados --------------------------------------------------

results.tbl <-   tbl %>%
  mutate_if(.predicate = is.numeric, .funs = mDiff) %>% 
  filter(row_number() > 9)

i=1
for(i in 1:110){
  
  #  Colunas selecionadas para a regressao
  colunas_Selecionadas <- paste("R",
                                rep(i, 3),
                                c("ETANOL_HIDRATADO", "OLEO_DIESEL", "GASOLINA_COMUM"),
                                sep = "_")
  
  # Selecao de dados para o VECM
  y <- tbl %>% 
    filter(year(DATA_FINAL) < 2019) %>%
    select(colunas_Selecionadas)
  
  brent <- tbl %>% 
    filter(year(DATA_FINAL) < 2019) %>% 
    select("brent", "lnCambio")
  
  #  modelo VECM para a regiao atual
  mdl <- VECM(data = y,
              lag = 8,
              r=1, 
              include = "const",
              beta = NULL,
              estim = "ML",
              LRinclude = "none",
              exogen = data.matrix(brent))
  
  
  # Determina coeficientes de longo prazo
  mLagLR <- mdl$coefficients[,"ECT"] %*% t(mdl$model.specific$beta)
  
  
  # determina coeficientes de Lag
  mLag1 <- mdl$coefficients[, c(sprintf("R_%d_ETANOL_HIDRATADO -1", i), sprintf("R_%d_GASOLINA_COMUM -1", i), sprintf("R_%d_OLEO_DIESEL -1", i))]
  mLag2 <- mdl$coefficients[, c(sprintf("R_%d_ETANOL_HIDRATADO -2", i), sprintf("R_%d_GASOLINA_COMUM -2", i), sprintf("R_%d_OLEO_DIESEL -2", i))]
  mLag3 <- mdl$coefficients[, c(sprintf("R_%d_ETANOL_HIDRATADO -3", i), sprintf("R_%d_GASOLINA_COMUM -3", i), sprintf("R_%d_OLEO_DIESEL -3", i))]
  mLag4 <- mdl$coefficients[, c(sprintf("R_%d_ETANOL_HIDRATADO -4", i), sprintf("R_%d_GASOLINA_COMUM -4", i), sprintf("R_%d_OLEO_DIESEL -4", i))]
  mLag5 <- mdl$coefficients[, c(sprintf("R_%d_ETANOL_HIDRATADO -5", i), sprintf("R_%d_GASOLINA_COMUM -5", i), sprintf("R_%d_OLEO_DIESEL -5", i))]
  mLag6 <- mdl$coefficients[, c(sprintf("R_%d_ETANOL_HIDRATADO -6", i), sprintf("R_%d_GASOLINA_COMUM -6", i), sprintf("R_%d_OLEO_DIESEL -6", i))]
  mLag7 <- mdl$coefficients[, c(sprintf("R_%d_ETANOL_HIDRATADO -7", i), sprintf("R_%d_GASOLINA_COMUM -7", i), sprintf("R_%d_OLEO_DIESEL -7", i))]
  mLag8 <- mdl$coefficients[, c(sprintf("R_%d_ETANOL_HIDRATADO -8", i), sprintf("R_%d_GASOLINA_COMUM -8", i), sprintf("R_%d_OLEO_DIESEL -8", i))]
  
  # Determina coeficientes de constante 
  mLagDm <- mdl$coefficients[,"Intercept"]
  
  # Determina coeficientes de Exogenas
  mExo <- mdl$coefficients[,c("brent","lnCambio")]
  
  #  constroi o vetor de variaveis defasadas
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
  
  Exo.df <- tbl %>% select("brent", "lnCambio")
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
  Dummies <- matrix(NA, nrow = 1, ncol = 1)
  Dummies[1,1] <- 1 # Constante
  
  Forecast.Dm <- matrix(NA, nrow = nrow(X), ncol = ncol(X))
  
  for(j in seq_len(nrow(Forecast.Dm))) {
    Forecast.Dm[j, ] <- mLagDm %*% Dummies
  }
  
  # Ajuste para os lags
  Forecast.Dm <- Forecast.Dm[9:(total_rows),]
  
  
  #  calcula o forecast total
  Forecast <- FSR + FLR_1 + Forecast.Dm
  
  colnames(Forecast) <- paste("Forecast", "R",
                              i,
                              c("ETANOL_HIDRATADO","OLEO_DIESEL","GASOLINA_COMUM"),
                              sep = "_")
  
  #  adiciona os resultado no data.frame de resultados.
  results.tbl[, colnames(Forecast) ] <- Forecast[, colnames(Forecast)]  
  
} # Fim do for(i in 1:110)



library(ggplot2)

regiao <- c("Sao Paulo" = 75,
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
  
  ggsave(filename = sprintf("./Graficos/%s/Forecast VECM - Etanol - %s.png", dir,  names(regiao)[i]),
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
  
  ggsave(filename = sprintf("./Graficos/%s/Forecast VECM - Diesel - %s.png", dir, names(regiao)[i]),
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
  
  g1
  
  ggsave(filename = sprintf("./Graficos/%s/Forecast VECM - Gasolina -%s.png", dir, names(regiao)[i]),
         plot = g1,
         units = "in",
         width = 8, height = 6,
         dpi = 100)
  
}  






# Calculo dos erros -------------------------------------------------------

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
  pivot_longer(cols = -DATA_INICIAL, names_to = "Serie", values_to = "Erro_VECM") %>% 
  mutate(SE_VECM = Erro_VECM^2)


saveRDS(object = tbl.erro, file = export_file)










