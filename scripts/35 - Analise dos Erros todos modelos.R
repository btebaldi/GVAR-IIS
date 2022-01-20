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
# library(readxl)
# library(readr)
# library(tidyr)
# library(dplyr)
library(stringr)
# library(lubridate)
library(forecast)


# Data load ---------------------------------------------------------------

VECM_ERROS <- readRDS(file = file.path("Ox", "mat_files", "Result_Matrix", "Result 1", "Erro_VECM.rds"))
GVARIIS_ERROS <- readRDS(file = file.path("Ox", "mat_files", "Result_Matrix", "Result 1", "Erro_GVARIIS.rds"))

lista_de_modelos <- c(12, 13, 14, 15, 16, 17)
for (modelo_atual in lista_de_modelos) {
  assign(x = sprintf("GVARIIS_ERROS_M%d", modelo_atual), 
         value = readRDS(file = file.path("Ox",
                                          "mat_files",
                                          "Result_Matrix",
                                          sprintf("Result %d", modelo_atual), 
                                          sprintf("Erro_Modelo_%d.rds", modelo_atual))) )
}


full_table <- dplyr::inner_join(VECM_ERROS, GVARIIS_ERROS, by = c("DATA_INICIAL", "Serie")) %>% 
  filter(year(DATA_INICIAL)> 2019) %>% na.omit()

for (modelo_atual in lista_de_modelos) {
  tbl <- get(sprintf("GVARIIS_ERROS_M%d", modelo_atual), )
  
  full_table <- dplyr::inner_join(full_table, tbl, by = c("DATA_INICIAL", "Serie"))
}

full_table <- full_table %>% filter(year(DATA_INICIAL)> 2019) %>% na.omit()


# Data prepraration -------------------------------------------------------


full_table$Region <- as.numeric(NA)
full_table$Variavel <- as.character(NA)

lin <- 1
for (lin in seq_len(nrow(full_table))) {
  RegExpMatch <- stringr::str_match(full_table$Serie[lin], "Erro_R_(\\d+)_(.*)")
  full_table$Region[lin] <- as.numeric(RegExpMatch[2])
  full_table$Variavel[lin] <- RegExpMatch[3]
}


full_table$Variavel <- factor(full_table$Variavel,
                              levels = c("OLEO_DIESEL", "ETANOL_HIDRATADO", "GASOLINA_COMUM"),
                              labels = c("Oleo Diesel", "Etanol Hidratado", "Gasolina Comum"))
tail(full_table)

# Boxplot graficos --------------------------------------------------------


regions <- c("São Paulo" = 75,
             "Rio de Janeiro" = 62,
             "Dist. Federal" = 109,
             "Belo Horizonte" = 46,
             "Salvador" = 39,
             "Fortaleza" = 20,
             "Manaus" = 4)

i <- 1
for (i in seq_along(regions)) {
  g1 <- full_table %>% 
    dplyr::filter(Region == regions[i]) %>%
    pivot_longer(cols = c("SE_GVARIIS", "SE_VECM", paste("SE_GVARIIS_M", 12:17, sep = "")), names_to = "Modelo", values_to = "SE") %>% 
    mutate(Modelo = factor(Modelo,
                           levels = c("SE_VECM", "SE_GVARIIS", paste("SE_GVARIIS_M", 12:17, sep = "")),
                           labels = c("VECM", "GVAR-IIS M1", paste("GVAR-IIS M", 2:7, sep = "")))) %>% 
    ggplot() +
    geom_boxplot(aes(x = SE, y = Variavel, fill = Modelo)) + 
    labs(title = sprintf("%s", names(regions)[i]),
         subtitle = "Boxplot of the forecast suqared errors",
         y=NULL,
         x="Squared Errors",
         caption = "Elaborated by the author")
  
  print(g1)
  
  ggsave(filename = sprintf("./Graficos/BoxPlot - MSE - %s.png",names(regions)[i]),
         plot = g1,
         units = "in",
         width = 8, height = 6,
         dpi = 100)
}



# MSE ---------------------------------------------------------------------

df_Result <- tibble(Descricao =as.character(NA),
                    GVAR = as.numeric(NA),
                    VECM = as.numeric(NA),
                    
                    GVAR_M12 = as.numeric(NA),
                    GVAR_M13 = as.numeric(NA),
                    GVAR_M14 = as.numeric(NA),
                    GVAR_M15 = as.numeric(NA),
                    GVAR_M16 = as.numeric(NA),
                    GVAR_M17 = as.numeric(NA),
                    
                    .rows = 4)

df_Result$Descricao[1] <- "Total"
df_Result$Descricao[2] <- "Etanol Hidratado"
df_Result$Descricao[3] <- "Oleo Diesel"
df_Result$Descricao[4] <- "Gasolina Comum"

df_Result$VECM[1] <- mean(full_table$SE_VECM)
df_Result$GVAR[1] <- mean(full_table$SE_GVARIIS)

df_Result$GVAR_M12[1] <- mean(full_table$SE_GVARIIS_M12)
df_Result$GVAR_M13[1] <- mean(full_table$SE_GVARIIS_M13)
df_Result$GVAR_M14[1] <- mean(full_table$SE_GVARIIS_M14)
df_Result$GVAR_M15[1] <- mean(full_table$SE_GVARIIS_M15)
df_Result$GVAR_M16[1] <- mean(full_table$SE_GVARIIS_M16)
df_Result$GVAR_M17[1] <- mean(full_table$SE_GVARIIS_M17)


tbl_aux <- full_table %>% 
  group_by(Variavel) %>% 
  summarise(VECM = mean(SE_VECM),
            GVAR = mean(SE_GVARIIS),
            GVAR_M12 = mean(SE_GVARIIS_M12),
            GVAR_M13 = mean(SE_GVARIIS_M13),
            GVAR_M14 = mean(SE_GVARIIS_M14),
            GVAR_M15 = mean(SE_GVARIIS_M15),
            GVAR_M16 = mean(SE_GVARIIS_M16),
            GVAR_M17 = mean(SE_GVARIIS_M17))


df_Result$GVAR[df_Result$Descricao == "Etanol Hidratado"] <- tbl_aux$GVAR[tbl_aux$Variavel == "Etanol Hidratado"]
df_Result$GVAR[df_Result$Descricao == "Oleo Diesel"] <- tbl_aux$GVAR[tbl_aux$Variavel == "Oleo Diesel"]
df_Result$GVAR[df_Result$Descricao == "Gasolina Comum"] <- tbl_aux$GVAR[tbl_aux$Variavel == "Gasolina Comum"]

df_Result$VECM[df_Result$Descricao == "Etanol Hidratado"] <- tbl_aux$VECM[tbl_aux$Variavel == "Etanol Hidratado"]
df_Result$VECM[df_Result$Descricao == "Oleo Diesel"] <- tbl_aux$VECM[tbl_aux$Variavel == "Oleo Diesel"]
df_Result$VECM[df_Result$Descricao == "Gasolina Comum"] <- tbl_aux$VECM[tbl_aux$Variavel == "Gasolina Comum"]


for (modelo_atual in lista_de_modelos) {
  df_Result[df_Result$Descricao == "Etanol Hidratado", sprintf("GVAR_M%d", modelo_atual)] <- tbl_aux[tbl_aux$Variavel == "Etanol Hidratado", sprintf("GVAR_M%d", modelo_atual)]
  df_Result[df_Result$Descricao == "Oleo Diesel",      sprintf("GVAR_M%d", modelo_atual)] <- tbl_aux[tbl_aux$Variavel == "Oleo Diesel",      sprintf("GVAR_M%d", modelo_atual)]
  df_Result[df_Result$Descricao == "Gasolina Comum",   sprintf("GVAR_M%d", modelo_atual)] <- tbl_aux[tbl_aux$Variavel == "Gasolina Comum",   sprintf("GVAR_M%d", modelo_atual)]
  
}


print(xtable::xtable(df_Result, type = "latex", display = c("d", "s", "E", "E", "E", "E", "E", "E", "E", "E")), file = "./MSE_allmodels_latex.txt")
writexl::write_xlsx(x = df_Result, path = "./MSE_allmodels_latex.xlsx")

# Diebold Mariano ---------------------------------------------------------

DM_Table_Result <- tibble(Modelo =as.character(NA),
                          GVARIIS = as.numeric(NA),
                          VECM = as.numeric(NA),
                          
                          GVARIIS_M12 = as.numeric(NA),
                          GVARIIS_M13 = as.numeric(NA),
                          GVARIIS_M14 = as.numeric(NA),
                          GVARIIS_M15 = as.numeric(NA),
                          GVARIIS_M16 = as.numeric(NA),
                          GVARIIS_M17 = as.numeric(NA),
                          
                          .rows = 8)


DM_Table_Result$Modelo[1] <- "GVARIIS"
DM_Table_Result$Modelo[2] <- "VECM"
DM_Table_Result$Modelo[3] <- "GVARIIS_M12"
DM_Table_Result$Modelo[4] <- "GVARIIS_M13"
DM_Table_Result$Modelo[5] <- "GVARIIS_M14"
DM_Table_Result$Modelo[6] <- "GVARIIS_M15"
DM_Table_Result$Modelo[7] <- "GVARIIS_M16"
DM_Table_Result$Modelo[8] <- "GVARIIS_M17"


for(M1 in DM_Table_Result$Modelo){
  for (M2 in DM_Table_Result$Modelo) {
    if(M1 == M2){
      next
    } else {
      mDm_test <- dm.test(e1 = full_table[[sprintf("Erro_%s", M1)]],
                          e2 = full_table[[sprintf("Erro_%s", M2)]],
                          # alternative = "two.sided" , #"two.sided", "less", "greater"
                          alternative = "greater",
                          h = 1,
                          power = 2)
      
      DM_Table_Result[DM_Table_Result$Modelo == M1, M2] <- mDm_test$p.value
      
    }
  }
}


print(xtable::xtable(DM_Table_Result, type = "latex", display = c("d", "s", "E", "E", "E", "E", "E", "E", "E", "E")), file = "./diebold_allmodels_latex.txt")
writexl::write_xlsx(x = DM_Table_Result, path = "./diebold_allmodels_latex.xlsx")

# Diebold mariano geral

# Diebold mariano By serie
for(mVar in c("Etanol Hidratado", "Oleo Diesel", "Gasolina Comum") ){
  tbl_aux <- full_table %>% 
    filter(Variavel == mVar)
  
  
  DM_Table_Result <- tibble(Modelo =as.character(NA),
                            GVARIIS = as.numeric(NA),
                            VECM = as.numeric(NA),
                            
                            GVARIIS_M12 = as.numeric(NA),
                            GVARIIS_M13 = as.numeric(NA),
                            GVARIIS_M14 = as.numeric(NA),
                            GVARIIS_M15 = as.numeric(NA),
                            GVARIIS_M16 = as.numeric(NA),
                            GVARIIS_M17 = as.numeric(NA),
                            
                            .rows = 8)
  
  
  DM_Table_Result$Modelo[1] <- "GVARIIS"
  DM_Table_Result$Modelo[2] <- "VECM"
  DM_Table_Result$Modelo[3] <- "GVARIIS_M12"
  DM_Table_Result$Modelo[4] <- "GVARIIS_M13"
  DM_Table_Result$Modelo[5] <- "GVARIIS_M14"
  DM_Table_Result$Modelo[6] <- "GVARIIS_M15"
  DM_Table_Result$Modelo[7] <- "GVARIIS_M16"
  DM_Table_Result$Modelo[8] <- "GVARIIS_M17"
  
  
  for(M1 in DM_Table_Result$Modelo){
    for (M2 in DM_Table_Result$Modelo) {
      if(M1 == M2){
        next
      } else {
        mDm_test <- dm.test(e1 = tbl_aux[[sprintf("Erro_%s", M1)]],
                            e2 = tbl_aux[[sprintf("Erro_%s", M2)]],
                            # alternative = "two.sided" , #"two.sided", "less", "greater"
                            alternative = "greater",
                            h = 1,
                            power = 2)
        
        DM_Table_Result[DM_Table_Result$Modelo == M1, M2] <- mDm_test$p.value
        
      }
    }
  }
  
  
  print(xtable::xtable(DM_Table_Result, type = "latex", display = c("d", "s", "E", "E", "E", "E", "E", "E", "E", "E")),
        file = sprintf("./diebold_allmodels_latex(%s).txt", mVar))
  writexl::write_xlsx(x = df_Result, path = sprintf("./diebold_allmodels_latex(%s).xlsx", mVar))
  
}



