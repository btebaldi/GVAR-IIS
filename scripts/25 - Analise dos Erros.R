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

full_table <- dplyr::inner_join(VECM_ERROS, GVARIIS_ERROS, by = c("DATA_INICIAL", "Serie")) %>% 
  filter(year(DATA_INICIAL)> 2019) %>% na.omit()



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
    pivot_longer(cols = c("SE_GVARIIS", "SE_VECM"), names_to = "Modelo", values_to = "SE") %>% 
    mutate(Modelo = factor(Modelo,
                           levels = c("SE_VECM", "SE_GVARIIS"),
                           labels = c("VECM", "GVAR-IIS"))) %>% 
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
                    Diebold = as.numeric(NA),
                    .rows = 4)

df_Result$Descricao[1] <- "Total"
df_Result$Descricao[2] <- "Etanol Hidratado"
df_Result$Descricao[3] <- "Oleo Diesel"
df_Result$Descricao[4] <- "Gasolina Comum"

df_Result$VECM[1] <- mean(full_table$SE_VECM)
df_Result$GVAR[1] <- mean(full_table$SE_GVARIIS)


tbl_aux <- full_table %>% 
  group_by(Variavel) %>% 
  summarise(VECM = mean(SE_VECM),
            GVAR = mean(SE_GVARIIS))

df_Result$GVAR[df_Result$Descricao == "Etanol Hidratado"] <- tbl_aux$GVAR[tbl_aux$Variavel == "Etanol Hidratado"]
df_Result$GVAR[df_Result$Descricao == "Oleo Diesel"] <- tbl_aux$GVAR[tbl_aux$Variavel == "Oleo Diesel"]
df_Result$GVAR[df_Result$Descricao == "Gasolina Comum"] <- tbl_aux$GVAR[tbl_aux$Variavel == "Gasolina Comum"]

df_Result$VECM[df_Result$Descricao == "Etanol Hidratado"] <- tbl_aux$VECM[tbl_aux$Variavel == "Etanol Hidratado"]
df_Result$VECM[df_Result$Descricao == "Oleo Diesel"] <- tbl_aux$VECM[tbl_aux$Variavel == "Oleo Diesel"]
df_Result$VECM[df_Result$Descricao == "Gasolina Comum"] <- tbl_aux$VECM[tbl_aux$Variavel == "Gasolina Comum"]

# Diebold Mariano ---------------------------------------------------------


# Diebold mariano geral
mDm_test <- dm.test(e1 = full_table$Erro_VECM,
        e2 = full_table$Erro_GVARIIS,
        # alternative = "two.sided" , #"two.sided", "less", "greater"
        alternative = "greater",
        h = 1,
        power = 2)

df_Result$Diebold[1] <- mDm_test$p.value

# Diebold mariano By serie
for(mVar in c("Etanol Hidratado", "Oleo Diesel", "Gasolina Comum") ){
  tbl_aux <- full_table %>% 
    filter(Variavel == mVar)
  
  mDm_test <- dm.test(e1 = tbl_aux$Erro_VECM,
                      e2 = tbl_aux$Erro_GVARIIS,
                      # alternative = "two.sided" , #"two.sided", "less", "greater"
                      alternative = "greater",
                      h = 1,
                      power = 2)
  
  print(mDm_test)
  
  df_Result$Diebold[df_Result$Descricao == mVar] <- mDm_test$p.value
  
}


print(xtable::xtable(df_Result, type = "latex", display = c("d", "s", "E", "E", "E")), file = "./diebold_latex.txt")


