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
library(ggplot2)
library(lubridate)
library(cowplot)


# file.name <- "forecast_result.csv"
dir <- "Result AL_GAS"


# export_file <- file.path("Ox", "mat_files", "Result_Matrix", dir, file.name)
# main_path <- dirname(export_file)
main_path <- file.path("Ox", "mat_files", "Result_Matrix", dir)

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



# Calculando os parametros do VAR -----------------------------------------

I  <- diag(nrow(mLagLR))
Ze <- I*0
L1 <- mLag1 + I + mLagLR
L2 <- mLag2 - mLag1
L3 <- mLag3 - mLag2
L4 <- mLag4 - mLag3
L5 <- mLag5 - mLag4
L6 <- mLag6 - mLag5
L7 <- mLag7 - mLag6
L8 <- mLag8 - mLag7
L9 <- - mLag8

a1 <- cbind(L1, L2, L3, L4, L5, L6, L7, L8, L9)
a2 <- cbind( I, Ze, Ze, Ze, Ze, Ze, Ze, Ze, Ze)
a3 <- cbind(Ze,  I, Ze, Ze, Ze, Ze, Ze, Ze, Ze)
a4 <- cbind(Ze, Ze,  I, Ze, Ze, Ze, Ze, Ze, Ze)
a5 <- cbind(Ze, Ze, Ze,  I, Ze, Ze, Ze, Ze, Ze)
a6 <- cbind(Ze, Ze, Ze, Ze,  I, Ze, Ze, Ze, Ze)
a7 <- cbind(Ze, Ze, Ze, Ze, Ze,  I, Ze, Ze, Ze)
a8 <- cbind(Ze, Ze, Ze, Ze, Ze, Ze,  I, Ze, Ze)
a9 <- cbind(Ze, Ze, Ze, Ze, Ze, Ze, Ze,  I, Ze)

row.names(a1) <- c("brent", "Cambio", paste("R", sort(rep(1:110, 2)), c("E", "G"), sep="_"))
row.names(a2) <- c("brent", "Cambio", paste("R", sort(rep(1:110, 2)), c("E", "G"), sep="_"))
row.names(a3) <- c("brent", "Cambio", paste("R", sort(rep(1:110, 2)), c("E", "G"), sep="_"))
row.names(a4) <- c("brent", "Cambio", paste("R", sort(rep(1:110, 2)), c("E", "G"), sep="_"))
row.names(a5) <- c("brent", "Cambio", paste("R", sort(rep(1:110, 2)), c("E", "G"), sep="_"))
row.names(a6) <- c("brent", "Cambio", paste("R", sort(rep(1:110, 2)), c("E", "G"), sep="_"))
row.names(a7) <- c("brent", "Cambio", paste("R", sort(rep(1:110, 2)), c("E", "G"), sep="_"))
row.names(a8) <- c("brent", "Cambio", paste("R", sort(rep(1:110, 2)), c("E", "G"), sep="_"))
row.names(a9) <- c("brent", "Cambio", paste("R", sort(rep(1:110, 2)), c("E", "G"), sep="_"))

mF <- rbind(a1, a2, a3, a4, a5, a6, a7, a8, a9)

which(row.names(a1) == "R_75_G") # Sao Paulo
which(row.names(a1) == "R_62_G") # RJ
which(row.names(a1) == "R_109_G") # DF
dim(L1)


mShock <- matrix(0, nrow = nrow(mF), ncol=1)
dim(mShock)
# mShock[1,1] <- 0.3465645
mShock[1,1] <- 1
dim(mShock)

n <- 24
# response <- matrix(NA, nrow = n, ncol = 331)
response <- matrix(NA, nrow = n, ncol = 222)

i <- 1
for(i in 1:n){
  if(i == 1){
    mShock.effect <- mF %*% mShock 
  } else {
    mShock.effect <- mF %*% mShock.effect
  }
  response[i, ] <- mShock.effect[1:222,1]
}


# colnames(response) <- c("brent", paste("R", sort(rep(1:110, 3)), c("E", "D","G"), sep="_"))
colnames(response) <- c("brent", "cambio", paste("R", sort(rep(1:110, 2)), c("E","G"), sep="_"))
response.df <- as_tibble(response)



mDiff <- function(x){
  ret <- x - dplyr::lag(x)
  ret[1] <- 0
  return(ret)
}

regiao <- c("Sao Paulo" = 75,
            "Rio de Janeiro" = 62,
            "Dist. Federal" = 109,
            "Belo Horizonte" = 46,
            "Salvador" = 39)

regiao2 <- c("SAO JOSE DO RIO PRETO" =  63,
             "RIBEIRAO PRETO" = 64,
             "ARACATUBA" = 65,
             "BOTUCATU" = 66,
             "ARARAQUARA" = 67,
             "ARARAS" = 68,
             "CAMPINAS" = 69,
             "MARILIA" = 71,
             "ADAMANTINA" = 70,
             "OURINHOS" = 72,
             "BRAGANCA PAULISTA" = 73,
             "CAMPOS DO JORDAO" = 74,
             "SAO PAULO" = 75,
             "ITANHAEM" = 76)

i=1
for(i in seq_along(regiao)){
  g1 <- response.df %>% 
    mutate_all(.funs = mDiff) %>%
    select(Etanol = sprintf("R_%d_E",regiao[i]), 
           # Diesel = sprintf("R_%d_D",regiao[i]), 
           Gasolina = sprintf("R_%d_G",regiao[i]))  %>% 
    mutate(Id = row_number()) %>% 
    ggplot() +
    geom_line(aes(x=Id, y=Etanol, colour="Hydrous ethanol"), linetype = "dotted",  size=1) +
    # geom_line(aes(x=Id, y=Diesel, colour="Diesel oil"), linetype = "dashed", size=1) +
    geom_line(aes(x=Id, y=Gasolina, colour="Regular gasoline"), linetype = "solid", size=1) +
    geom_hline(yintercept = 0) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(title = sprintf("%s", names(regiao)[i]),
         subtitle = "Impulse Response Function - Long Run",
         # subtitle = "Impulse Response Function - Shor run effects",
         colour = NULL,
         y=NULL,
         x=NULL,
         caption = "Elaborated by the author") +
    guides(color = guide_legend(override.aes = list(linetype = c(1, 1) ) ) )
  
  print(g1)
  
  # ggsave(filename = sprintf("./Graficos/IRF/IRF - Nivel - %s (%s).png",names(regiao)[i], dir),
  #        plot = g1,
  #        units = "in",
  #        width = 8, height = 6,
  #        dpi = 100)
}
# Diesel oil, regular gasoline, and hydrous ethanol.

saveRDS(object = response.df, file = sprintf("./database/db_IRF_response_LongRun (%s).rds", dir))

# Calculando os parametros do VECM -----------------------------------------

I  <- diag(nrow(mLagLR))
Ze <- I*0
L1 <- mLag1 
L2 <- mLag2 
L3 <- mLag3 
L4 <- mLag4 
L5 <- mLag5 
L6 <- mLag6 
L7 <- mLag7 
L8 <- mLag8 


a1 <- cbind(L1, L2, L3, L4, L5, L6, L7, L8)
a2 <- cbind( I, Ze, Ze, Ze, Ze, Ze, Ze, Ze)
a3 <- cbind(Ze,  I, Ze, Ze, Ze, Ze, Ze, Ze)
a4 <- cbind(Ze, Ze,  I, Ze, Ze, Ze, Ze, Ze)
a5 <- cbind(Ze, Ze, Ze,  I, Ze, Ze, Ze, Ze)
a6 <- cbind(Ze, Ze, Ze, Ze,  I, Ze, Ze, Ze)
a7 <- cbind(Ze, Ze, Ze, Ze, Ze,  I, Ze, Ze)
a8 <- cbind(Ze, Ze, Ze, Ze, Ze, Ze,  I, Ze)

mF <- rbind(a1, a2, a3, a4, a5, a6, a7, a8)

mShock <- matrix(0, nrow = nrow(mF), ncol=1)
# mShock[1,1] <- 0.03666161
mShock[152,1] <- 1

n <- 24
response <- matrix(NA, nrow = n, ncol = 222)

i <- 1
for(i in 1:n){
  if(i == 1){
    mShock.effect <- mF %*% mShock 
  } else {
    mShock.effect <- mF %*% mShock.effect
  }
  response[i, ] <- mShock.effect[1:222,1]
}


# colnames(response) <- c("brent", paste("R", sort(rep(1:110, 3)), c("E", "D","G"), sep="_"))
colnames(response) <- c("brent", "cambio", paste("R", sort(rep(1:110, 2)), c("E", "G"), sep="_"))
response.df <- as_tibble(response)

regiao <- regiao2
for(i  in seq_along(regiao)){
  
  g1 <- response.df %>% 
    select(Etanol = sprintf("R_%d_E",regiao[i]), 
           # Diesel = sprintf("R_%d_D",regiao[i]), 
           Gasolina = sprintf("R_%d_G",regiao[i]))  %>% 
    mutate(Id = row_number()) %>% 
    ggplot() +
    geom_line(aes(x=Id, y=cumsum(Etanol), colour="Hydrous ethanol"), linetype = "dotted",  size=1) +
    # geom_line(aes(x=Id, y=Diesel, colour="Diesel oil"), linetype = "dashed", size=1) +
    geom_line(aes(x=Id, y=cumsum(Gasolina), colour="Regular gasoline"), linetype = "solid", size=1) +
    geom_hline(yintercept = 0) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(title = sprintf("%s", names(regiao)[i]),
         subtitle = "Impulse Response Function",
         # subtitle = "Impulse Response Function - Shor run effects",
         colour = NULL,
         y=NULL,
         x=NULL,
         caption = "Elaborated by the author") +
    guides(color = guide_legend(override.aes = list(linetype = c(1, 1) ) ) )
  
  
  if(i %in% c(1,2,4)){
    assign(x = sprintf("g_%s", regiao[i]),
           value = g1)
  }
  
  print(g1)
  
  ggsave(filename = sprintf("./Graficos/IRF/IRF - Short Run - %s (%s).png",names(regiao)[i], dir),
         plot = g1,
         units = "in",
         width = 8, height = 6,
         dpi = 100)
}

saveRDS(object = response.df, file = sprintf("./database/db_IRF_response (%s).rds", dir))

g1 <- cowplot::plot_grid(g_75, g_46, g_62,
                         nrow=3, ncol = 1)


ggsave(filename = sprintf("./Graficos/IRF/IRF - Short Run - Combined SP-BH-RJ (%s).png", dir),
       plot = g1,
       units = "in",
       width = 8, height = 6*3,
       dpi = 100)
