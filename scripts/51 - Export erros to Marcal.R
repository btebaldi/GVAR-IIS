#' Tese Capitulo 2 - Analise de precos de gasolina, alcool e Oleo Diesel
#' 
#' Script para exportacao dos erros para o Emerson Marcal
#' 
#' Author: Bruno Tebaldi de Queiroz Barbosa
#' 
#' Data: 2023-11-21
#' 

# setup -------------------------------------------------------------------

rm(list=ls())
# library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(forecast)
library(ggplot2)


# Data load ---------------------------------------------------------------

VECM_ERROS <- readRDS(file = file.path("Ox", "mat_files", "Result_Matrix", "VECM", "Erro_VECM.rds"))
VECM2_ERROS <- readRDS(file = file.path("Ox", "mat_files", "Result_Matrix", "VECM w Cambio", "Erro_VECM_wCambio.rds"))
colnames(VECM2_ERROS) <- c("DATA_INICIAL", "Serie", "Erro_VECM2", "SE_VECM2")
# GVARIIS_ERROS <- readRDS(file = file.path("Ox", "mat_files", "Result_Matrix", "Result 1", "Erro_GVARIIS.rds"))

lista_de_modelos <- c(1, 12:18, 21:28)
for (modelo_atual in lista_de_modelos) {
  assign(x = sprintf("GVARIIS_ERROS_M%d", modelo_atual),
         value = readRDS(file = file.path("Ox",
                                          "mat_files",
                                          "Result_Matrix",
                                          sprintf("Result %d", modelo_atual), 
                                          "Erro_GVARIIS.rds")) )
}


full_table <- dplyr::inner_join(VECM_ERROS, VECM2_ERROS, by = c("DATA_INICIAL", "Serie")) %>% 
  filter(year(DATA_INICIAL)>= 2019) %>% na.omit()

for (modelo_atual in lista_de_modelos) {
  tbl <- get(sprintf("GVARIIS_ERROS_M%d", modelo_atual))
  colnames(tbl) <- c("DATA_INICIAL",
                     "Serie",
                     sprintf("Erro_M_%d", modelo_atual),
                     sprintf("SE_M_%d", modelo_atual))
  
  full_table <- dplyr::inner_join(full_table, tbl, by = c("DATA_INICIAL", "Serie"))
}

full_table <- full_table %>% 
  na.omit()

rm(list = setdiff(ls(), c("full_table", "lista_de_modelos")))


full_table <- full_table %>% select(DATA_INICIAL, Serie, starts_with("Erro"))

# Data prepraration -------------------------------------------------------

full_table$Region <- as.numeric(NA)
full_table$Variavel <- as.character(NA)

lin <- 1
for (lin in seq_len(nrow(full_table))) {
  RegExpMatch <- stringr::str_match(full_table$Serie[lin], "Erro_R_(\\d+)_(.*)")
  full_table$Region[lin] <- as.numeric(RegExpMatch[2])
  full_table$Variavel[lin] <- RegExpMatch[3]
}

full_table %>% select(DATA_INICIAL, Region, Variavel, everything())

full_table_long <- full_table %>% pivot_longer(cols = starts_with("Erro"))
  
full_table_long$Model <- stringr::str_match(full_table_long$name, "Erro_(.*)")[,2]

my_levelsVar <- c("OLEO_DIESEL", "ETANOL_HIDRATADO", "GASOLINA_COMUM")
my_labelsVar <- c("D", "E", "G")


full_table_long$Variavel <- factor(full_table_long$Variavel,
                              levels = my_levelsVar,
                              labels = my_labelsVar)
tail(full_table_long)
full_table_long$Model <- stringr::str_replace_all(full_table_long$Model, pattern = "M_", replacement = "M")

full_table_long$ColName <- sprintf("%s_%s_R%d",
                                   full_table_long$Variavel,
                                   full_table_long$Model,
                                   full_table_long$Region)
unique(full_table_long$Model)

full_table_wide <- full_table_long %>%
  pivot_wider(id_cols = c("DATA_INICIAL"),
              names_from = "ColName",
              values_from = "value")


write.csv(full_table_wide, "Export_erros_to_Marcal.csv")
