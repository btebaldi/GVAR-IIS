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
              names_from = c("OxCode", "PRODUTO"),
              values_from = "PRECO_MEDIO_REVENDA",
              names_prefix = "R_")


# completa datas faltantes
db_com_buraco <- db_com_buraco %>% add_row(DATA_INICIAL = as.Date("2005-08-14"), DATA_FINAL = as.Date("2005-08-20"))
db_com_buraco <- db_com_buraco %>% add_row(DATA_INICIAL = as.Date("2005-08-21"), DATA_FINAL = as.Date("2005-08-27"))
db_com_buraco <- db_com_buraco %>% add_row(DATA_INICIAL = as.Date("2009-08-16"), DATA_FINAL = as.Date("2009-08-22"))
db_com_buraco <- db_com_buraco %>% add_row(DATA_INICIAL = as.Date("2009-08-23"), DATA_FINAL = as.Date("2009-08-29"))
db_com_buraco <- db_com_buraco %>% add_row(DATA_INICIAL = as.Date("2015-08-16"), DATA_FINAL = as.Date("2015-08-22"))

db_com_buraco <- db_com_buraco %>% arrange(DATA_INICIAL)


saveRDS(object = db_com_buraco,
        file = "./database/db_Ox_com_buraco.rds")












