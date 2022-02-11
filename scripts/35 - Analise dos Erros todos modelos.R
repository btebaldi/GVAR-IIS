#' Tese Capitulo 2 - Analise de precos de gasolina, alcool e Oleo Diesel
#' 
#' Script para analise dos erros de Forecast da series.
#' 
#' Author: Bruno Tebaldi de Queiroz Barbosa
#' 
#' Data: 2022-01-06
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

# Data prepraration -------------------------------------------------------


full_table$Region <- as.numeric(NA)
full_table$Variavel <- as.character(NA)

lin <- 1
for (lin in seq_len(nrow(full_table))) {
  RegExpMatch <- stringr::str_match(full_table$Serie[lin], "Erro_R_(\\d+)_(.*)")
  full_table$Region[lin] <- as.numeric(RegExpMatch[2])
  full_table$Variavel[lin] <- RegExpMatch[3]
}


my_levelsVar <- c("OLEO_DIESEL", "ETANOL_HIDRATADO", "GASOLINA_COMUM")
my_labelsVar <- c("Diesel Oil", "Hydrous Ethanol", "Regular Gasoline")


full_table$Variavel <- factor(full_table$Variavel,
                              levels = my_levelsVar,
                              labels = my_labelsVar)
tail(full_table)



# Calcula os melhores modelos

tbl <- full_table %>%
  select(starts_with("SE_"))

tbl <- apply(tbl, 2, mean)
tbl <- sort(tbl)

my_levels <- names(tbl)
my_labels <- stringr::str_match(names(tbl), "SE_(.*)")
my_labels <- my_labels[, 2]

my_labels <- stringr::str_replace(string = my_labels,
                                  pattern = "M_", 
                                  replacement = "Model ")

my_labels <- stringr::str_replace(string = my_labels,
                                  pattern = "VECM2", 
                                  replacement = "VECM w/ Exc")

# Boxplot graficos --------------------------------------------------------

# my_labels <- c("Model 18", "Model 16", "Model 28", "Model 26",
#                "Model 17", "Model 27", "Model 1", "Model 21",
#                "VECM w/ Exc", "VECM",
#                "Model 24", "Model 14", "Model 22", "Model 12",
#                "Model 25", "Model 15", "Model 23", "Model 13")
# 
# my_labels <- paste("SE_M_", c("18", "16", "28", "26",
#                "17", "27", "1", "21",
#                "VECM w/ Exc", "VECM",
#                "24", "14", "22", "12",
#                "25", "15", "23", "13"), 
#                sep = "" )

my_levels <- c("SE_M_18", "SE_M_16", "SE_M_28", "SE_M_26",
  "SE_M_17", "SE_M_27", "SE_M_1", "SE_M_21", 
  "SE_VECM2", "SE_VECM",
  "SE_M_24", "SE_M_14", "SE_M_22", "SE_M_12",
  "SE_M_25", "SE_M_15", "SE_M_23", "SE_M_13")

my_labels <- c("M8", "M6", "M16", "M14",
               "M7", "M15", "M1", "M9",
               "VECM2", "VECM", 
               "M12", "M4", "M10", "M2",
               "M13", "M5", "M11", "M3")


for (var in my_labelsVar) {
  
  g1 <- full_table %>% 
    select(DATA_INICIAL, Region, Variavel, starts_with("SE")) %>% 
    filter(Variavel == var) %>% 
    pivot_longer(cols = c("SE_VECM", "SE_VECM2", paste("SE_M_", lista_de_modelos, sep = "")),
                 names_to = "Modelo", values_to = "SE") %>% 
    mutate(Modelo = factor(Modelo,
                           levels = my_levels,
                           labels = my_labels)) %>%
    ggplot(mapping = aes(x = SE, y = Modelo)) +
    stat_boxplot(geom = "errorbar", width = 0.2) +
    geom_boxplot(outlier.shape = NA) +
    labs(title = sprintf("Squared Error - %s", var),
         subtitle = "Boxplot of the forecast suqared errors",
         y=NULL,
         x="Squared Errors",
         caption = "Elaborated by the author\nNote: Outliers are not ploted in the graph") +
    theme_bw() +
    scale_x_continuous(limits = c(0, 0.003))
  
  print(g1)
  
  ggsave(filename = sprintf("./Graficos/MSE/BoxPlot - MSE - %s - %s.png", "Total", var),
         plot = g1,
         units = "in",
         width = 8, height = 6,
         dpi = 100)
}


regions <- c("Sao Paulo" = 75,
             "Rio de Janeiro" = 62,
             "Dist. Federal" = 109,
             "Belo Horizonte" = 46,
             "Salvador" = 39,
             "Fortaleza" = 20,
             "Manaus" = 4)

i <- 1
for (i in seq_along(regions)) {
  
  for (var in my_labelsVar) {
    print(regions[i])
    print(var)
    full_table %>% 
      filter(Region == regions[i]) %>% 
      filter(Variavel == var) %>% 
      select(Region, starts_with("SE_")) %>%
      apply(MARGIN = 2, FUN = mean, na.rm=TRUE) %>% 
      print()
    
    g1 <- full_table %>% 
      filter(Region == regions[i]) %>% 
      select(DATA_INICIAL, Region, Variavel, starts_with("SE")) %>% 
      filter(Variavel == var) %>% 
      pivot_longer(cols = c("SE_VECM", "SE_VECM2", paste("SE_M_", lista_de_modelos, sep = "")),
                   names_to = "Modelo", values_to = "SE") %>% 
      mutate(Modelo = factor(Modelo,
                             levels = my_levels,
                             labels = my_labels)) %>%
      ggplot(mapping = aes(x = SE, y = Modelo)) +
      stat_boxplot(geom = "errorbar", width = 0.2) +
      geom_boxplot() +
      labs(title = sprintf("%s - Squared Error - %s", names(regions)[i], var),
           subtitle = "Boxplot of the forecast suqared errors",
           y=NULL,
           x="Squared Errors",
           caption = "Elaborated by the author") +
      theme_bw() 
    
    print(g1)
    
    ggsave(filename = sprintf("./Graficos/MSE/BoxPlot - MSE - %s - %s.png",  names(regions)[i], var),
           plot = g1,
           units = "in",
           width = 8, height = 6,
           dpi = 100)
  }
}



# MSE ---------------------------------------------------------------------


# my_labels <- c("Model 18", "Model 16", "Model 28", "Model 26", "Model 17", "Model 27", "Model 1","Model 21",
#                "VECM w/ Exc","VECM", "Model 24", "Model 14", "Model 22", "Model 12", "Model 25", "Model 15",
#                "Model 23", "Model 13")
# my_levels <- c("M_18", "M_16", "M_28", "M_26", "M_17", "M_27", "M_1","M_21",
#                "VECM2","VECM", "M_24", "M_14", "M_22", "M_12", "M_25", "M_15",
#                "M_23", "M_13")

my_levels <- c("SE_M_18", "SE_M_16", "SE_M_28", "SE_M_26",
               "SE_M_17", "SE_M_27", "SE_M_1", "SE_M_21", 
               "SE_VECM2", "SE_VECM",
               "SE_M_24", "SE_M_14", "SE_M_22", "SE_M_12",
               "SE_M_25", "SE_M_15", "SE_M_23", "SE_M_13")

my_labels <- c("M8", "M6", "M16", "M14",
               "M7", "M15", "M1", "M9",
               "VECM2", "VECM", 
               "M12", "M4", "M10", "M2",
               "M13", "M5", "M11", "M3")

MSE_Result <- tibble(Modelo = as.character(NA),
                     MSE_Total = as.numeric(NA),
                     
                     MSE_Ethanol = as.numeric(NA),
                     MSE_Diesel = as.numeric(NA),
                     MSE_Gasoline = as.numeric(NA),
                     
                     .rows = length(my_labels))


MSE_Result$Modelo <- my_labels

my_levelsVarColMap <- c("MSE_Total" = "Total",
                        "MSE_Diesel" = "Diesel Oil",
                        "MSE_Ethanol" = "Hydrous Ethanol",
                        "MSE_Gasoline" = "Regular Gasoline")
for (var in c("Total", my_labelsVar)) {
  
  if(var == "Total"){
    tbl <- full_table %>%
      select(starts_with("SE_")) 
  } else {
    tbl <- full_table %>%
      filter(Variavel == var) %>% 
      select(starts_with("SE_")) 
  }
  tbl <- apply(tbl , 2, mean)
  
  colidx <- which(my_levelsVarColMap == var)
  
  for (i in seq_len(nrow(MSE_Result))) {
    idx <- which(my_labels == MSE_Result$Modelo[i])
    # MSE_Result[i, names(my_levelsVarColMap)[colidx]] <- tbl[paste("SE_", my_levels[i], sep ="")]
    MSE_Result[i, names(my_levelsVarColMap)[colidx]] <- sqrt(tbl[my_levels[i]])
  }
  
}


print(xtable::xtable(MSE_Result, type = "latex", display = c("d", "s", "E", "E", "E", "E")),
      file = "./Tabelas/MSE_allmodels_latex.txt")
writexl::write_xlsx(x = MSE_Result, path = "./Tabelas/MSE_allmodels_latex.xlsx")

# Diebold Mariano ---------------------------------------------------------


# my_labels <- c("Model 18", "Model 16", "Model 28", "Model 26", "Model 17", "Model 27", "Model 1","Model 21",
#                "VECM w/ Exc","VECM", "Model 24", "Model 14", "Model 22", "Model 12", "Model 25", "Model 15",
#                "Model 23", "Model 13")
# my_levels <- c("M_18", "M_16", "M_28", "M_26", "M_17", "M_27", "M_1","M_21",
#                "VECM2","VECM", "M_24", "M_14", "M_22", "M_12", "M_25", "M_15",
#                "M_23", "M_13")

my_levels <- c("M_18",  "M_16", "M_28", "M_26",
               "M_17",  "M_27", "M_1", "M_21", 
               "VECM2", "VECM",
               "M_24",  "M_14", "M_22", "M_12",
               "M_25",  "M_15", "M_23", "M_13")

my_labels <- c("M8", "M6", "M16", "M14",
               "M7", "M15", "M1", "M9",
               "VECM2", "VECM", 
               "M12", "M4", "M10", "M2",
               "M13", "M5", "M11", "M3")


for(mVar in c("Total", "Etanol Hidratado", "Oleo Diesel", "Gasolina Comum") ){
  if(mVar == "Total"){
    tbl_aux <- full_table
  } else {
    tbl_aux <- full_table %>% 
      filter(Variavel == mVar)
  }
  
  DM_Table_Result <- tibble(Modelo =as.character(NA),
                            .rows = length(my_labels))
  
  DM_Table_Result$Modelo <- my_labels
  DM_Table_Result[,my_labels] <- NA
  
  
  for(M1 in DM_Table_Result$Modelo){
    for (M2 in DM_Table_Result$Modelo) {
      if(M1 == M2){
        next
      } else {
        
        M1_idx <- which(my_labels == M1)
        M1_name <- sprintf("Erro_%s", my_levels[M1_idx])
        
        M2_idx <- which(my_labels == M2)
        M2_name <- sprintf("Erro_%s", my_levels[M2_idx])
        
        mDm_test <- dm.test(e1 = full_table[[M1_name]],
                            e2 = full_table[[M2_name]],
                            # alternative = "two.sided" , #"two.sided", "less", "greater"
                            # h1: e1 > e2
                            alternative = "greater",
                            h = 1,
                            power = 1)

        DM_Table_Result[DM_Table_Result$Modelo == M1, M2] <- mDm_test$p.value
        
      }
    }
  }
  
  print(xtable::xtable(DM_Table_Result, type = "latex",
                       display = c("d", "s", rep("E", length(my_labels)))),
        file = sprintf("./Tabelas/diebold - %s - latex.txt", mVar))
  writexl::write_xlsx(x = DM_Table_Result, path = sprintf("./Tabelas/diebold - %s.xlsx", mVar))
  
}



