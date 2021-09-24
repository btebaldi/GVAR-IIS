#' AUTOR: Bruno Tebaldi de Q Barbosa
#' 
#' Data: 2021-09-24
#' 
#' Construção do banco de dados de gasolina para a tese


# Setup -------------------------------------------------------------------
rm(list=ls())


library(readxl)
library(dplyr)
library(readr)

# User defined Function ---------------------------------------------------

RenameColumns <- function(x){
  
  namesMap <- c("DATA INICIAL"="DATA_INICIAL",
                "DATA FINAL" = "DATA_FINAL",
                "REGIÃO"="REGIAO",
                "MUNICÍPIO" = "MUNICIPIO",
                "NÚMERO DE POSTOS PESQUISADOS"="NUM_PESQUISADOS",
                "UNIDADE DE MEDIDA"="UNI_MEDIDA",
                "PREÇO MÉDIO REVENDA"="PRECO_MEDIO_REVENDA",
                "DESVIO PADRÃO REVENDA"="DESVIO_PADRAO_REVENDA",
                "PREÇO MÍNIMO REVENDA"="PRECO_MINIMO_REVENDA",
                "PREÇO MÁXIMO REVENDA"="PRECO_MAXIMO_REVENDA",
                "MARGEM MÉDIA REVENDA"="MARGEM_MEDIA_REVENDA",
                "COEF DE VARIAÇÃO REVENDA" = "COEF_DE_VARIACAO_REVENDA",
                "PREÇO MÉDIO DISTRIBUIÇÃO"="PRECO_MEDIO_DISTRIBUICAO",
                "DESVIO PADRÃO DISTRIBUIÇÃO"="DESVIO_PADRAO_DISTRIBUICAO",
                "PREÇO MÍNIMO DISTRIBUIÇÃO"="PRECO_MINIMO_DISTRIBUICAO",
                "PREÇO MÁXIMO DISTRIBUIÇÃO"="PRECO_MAXIMO_DISTRIBUICAO",
                "COEF DE VARIAÇÃO DISTRIBUIÇÃO"="COEF_DE_VARIACAO_DISTRIBUICAO")
  
  
  for(i in 1:length(namesMap)){
    x[x == names(namesMap)[i]] = namesMap[i]
  }
  return(x)
}



# Carrega banco de dados --------------------------------------------------

# Sao 6 bancos de dados ao todo
# Gas 1: 2004-2012
# Gas 2: 2013-2017
# Gas 3: 2018
# Gas 4: 2019
# Gas 5: 2020
# Gas 6: 2021

for(i in 1:6){
  # carrega o banco de dados
  tbl <- read_excel(sprintf("C:/Users/bruno.tebaldi/OneDrive - NOVA FUTURA CTVM LTDA/Gas %d.xlsx", i), 
                    na = "-")
  
  # Ajusta o nome das colunas
  colnames(tbl) <- RenameColumns(colnames(tbl))
  
  # junta todos os bancos de dados
  if(i==1){
    full_tbl <- tbl
  } else {
    full_tbl <- full_tbl %>% dplyr::bind_rows(tbl)
  }
}


rm(list = setdiff(ls(), "full_tbl"))


# Padronizacao da unidade de medida ---------------------------------------

levels = unique(full_tbl$UNI_MEDIDA)
labels = c("BRL_L", "BRL_13KG", "BRL_M3", "BRL_13KG", "BRL_M3")

full_tbl$UNI_MEDIDA <- factor(full_tbl$UNI_MEDIDA, labels = labels, levels = levels)


# Padronizacao do produto -------------------------------------------------

levels = unique(full_tbl$PRODUTO)
labels = c("ETANOL_HIDRATADO",
           "OLEO_DIESEL",
           "GASOLINA_COMUM",
           "GLP",
           "GNV",
           "OLEO_DIESEL_S10",
           "GASOLINA_ADITIVADA",
           "OLEO_DIESEL",
           "OLEO_DIESEL_S10")

full_tbl$PRODUTO <- factor(full_tbl$PRODUTO, labels = labels, levels = levels)


# Padronizacao da regiao --------------------------------------------------

levels = unique(full_tbl$REGIAO)
labels = c("NORTE", "NORDESTE", "SUDESTE", "CENTRO OESTE", "SUL")

full_tbl$REGIAO <- factor(full_tbl$REGIAO, labels = labels, levels = levels)

# Padronizacao do Estado --------------------------------------------------

levels = unique(full_tbl$ESTADO)
labels = levels

full_tbl$ESTADO <- factor(full_tbl$ESTADO, labels = labels, levels = levels)


# Padronizacao do Municipio -----------------------------------------------

# levels = unique(full_tbl$MUNICIPIO)
# 
# write.csv(levels, file = "joe.csv")
# 
# full_tbl %>% filter(MUNICIPIO %in% c("ASSIS", "ASSIS CHATEAUBRIAND"))
# 
# 
# sort(levels)
# labels = levels
# 
# full_tbl$MUNICIPIO <- factor(full_tbl$MUNICIPIO, labels = labels, levels = levels)


# Padronizacao de datas ---------------------------------------------------

full_tbl$DATA_INICIAL <- as.Date(full_tbl$DATA_INICIAL)
full_tbl$DATA_FINAL <- as.Date(full_tbl$DATA_FINAL)

# Salva o banco de dados --------------------------------------------------

saveRDS(full_tbl, "./database/Gasolina2.rds")
