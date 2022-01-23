#' 
#' Script para geração dos graficos das regioes.
#' 
#' Author: Bruno Tebaldi de Queiroz Barbosa
#' 
#' Data: 2022-01-06
#' 


# Load libraries ----------------------------------------------------------
rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)


# User defined Functions --------------------------------------------------

mDiff <- function(x){
  ret <- x - dplyr::lag(x)
  return(ret)
}


# Configs -----------------------------------------------------------------

grafico_path_mask <- "./Graficos/Regions"

# Read databases ----------------------------------------------------------

# Faz leitura do banco de dados
tbl <- readRDS(file = "./database/db_oil_forForecast2.rds")

# Constroi um vetor com as regioes que vão gerar graficos.
regions <- c("Sao Paulo" = 75,
             "Rio de Janeiro" = 62,
             "Dist. Federal" = 109,
             "Belo Horizonte" = 46,
             "Salvador" = 39,
             "Fortaleza" = 20,
             "Manaus" = 4)

# Nome das series a ser analisadas
series <- c("ETANOL_HIDRATADO", "GASOLINA_COMUM", "OLEO_DIESEL")

# Looping em cada regiao
i=1

for (i in seq_along(regions)) {
  
  
  # Constroi um vetor com o nome das coluas
  cols <- paste("R", regions[i], series, sep = "_")
  cols_wData <- c("DATA_FINAL", cols)
  
  # dicionario de colunas (apenas para deixar os graficos mais bonitos)
  cols_dic <- c("Etanol Hidratado", "Gasolina Comum", "Óleo Diesel")
  names(cols_dic) <- cols
  
  # labeller : Troca o nome das series.
  mlabbels <- as_labeller(cols_dic)
  
  # Cria o grafico da regiao atual - Grafico em nivel
  g1 <- tbl %>% 
    select(cols_wData) %>% 
    pivot_longer(cols = -DATA_FINAL) %>% 
    ggplot() +
    geom_line(aes(x=DATA_FINAL, y = value, colour = name)) +
    facet_grid(name~., labeller = mlabbels) + 
    labs(title = "Price level (in log)",
         subtitle = sprintf("Region %s", names(regions)[i]),
         y="Log-Price [R$]",
         x=NULL,
         caption = "Prices in Brazilian Reais (R$)\nSource: elaborated by the author") +
    theme(plot.caption = element_text(hjust = 0), legend.position = "none")
  
  # Salva o ultimo grafico criado
  grafico_filename <- sprintf(file.path(grafico_path_mask, sprintf("Regiao %d - %s - Preco Nivel.png", regions[i], names(regions)[i])))
  ggsave(plot = g1,
         filename = grafico_filename,
         units = "in",
         width = 8,
         height = 6,
         dpi = 100)
  
  # Cria o grafico da regiao atual - Grafico em diferença
  g2 <- tbl %>% 
    select(cols_wData) %>% 
    mutate_if(.predicate = is.numeric, .funs = mDiff) %>% 
    pivot_longer(cols = -DATA_FINAL) %>% 
    ggplot() +
    geom_line(aes(x=DATA_FINAL, y = value, colour = name)) +
    facet_grid(name~., labeller = mlabbels) + 
    labs(title = "Difference of the price level (in log)",
         subtitle = sprintf("Region %s", names(regions)[i]),
         y="Diff. Log-Price",
         x=NULL,
         caption = "Prices in Brazilian Reais (R$)\nSource: elaborated by the author") +
    theme(plot.caption = element_text(hjust = 0), legend.position = "none")
  
  grafico_filename <- sprintf(file.path(grafico_path_mask, sprintf("%d - %s - Preco Diferenca.png", regions[i], names(regions)[i])))
  ggsave(plot = g2,
         filename = grafico_filename,
         units = "in",
         width = 8,
         height = 6,
         dpi = 100)
}



