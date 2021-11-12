#' Author: Bruno Tebaldi Barbosa
#'
#' 2021-11-09
#'
#' Script que faz a preparacao dos dados (com buraco) para a regressao do GVAR 
#' 
#' 
# Setup -------------------------------------------------------------------

rm(list = ls())

library(dplyr)
library(tidyr)

# Data Load ---------------------------------------------------------------

Gas_agrupo <- readRDS("./database/Gasolina_Agrupamento.rds")

colnames(Gas_agrupo)

db_com_buraco <- Gas_agrupo %>% 
  filter(PRODUTO %in% c("ETANOL_HIDRATADO", "OLEO_DIESEL", "GASOLINA_COMUM")) %>% 
  pivot_wider(id_cols = c("DATA_INICIAL", "DATA_FINAL"),
              names_from = c("Agrupamento", "PRODUTO"),
              values_from = "PRECO_MEDIO_REVENDA",
              names_prefix = "R_")


saveRDS(object = db_com_buraco,
        file = "./database/db_com_buraco.rds")


