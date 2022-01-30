#' Tese Capitulo 2 - Analise de precos de gasolina, alcool e Oleo Diesel
#'
#' Script para geração dos dados de IRF para serem utilizados pelo Excel e gerar
#' um mapa com o video dos choques.
#'
#' Author: Bruno Tebaldi de Queiroz Barbosa
#'
#' Data: 2022-01-29
#' 


# Setup -------------------------------------------------------------------

rm(list=ls())

library(readxl)



# Data load ---------------------------------------------------------------

Cadastro_de_municipios <- read_excel("database/Cadastro de municipios.xlsx", 
                                     sheet = "Map_Meso-Ox")


model <- "Result 28"
IRF <- readRDS(file = sprintf("./database/db_IRF_response (%s).rds", model))


# Regioes faltantes -------------------------------------------------------

missingRegions <- Cadastro_de_municipios$ID_Meso[Cadastro_de_municipios$Ox == -1]


# Construcao da base de dados ---------------------------------------------

# Inicializa a tabela de dados
tbl <- tibble(Id = 1:24)

lin=1
for(lin in seq_len(nrow(Cadastro_de_municipios))){
  
  for(serie in c("E", "D", "G")){
    
    col_destino <- sprintf("R_%d_%s", Cadastro_de_municipios$ID_Meso[lin], serie)
    col_origem <- sprintf("R_%d_%s", Cadastro_de_municipios$Ox[lin], serie)
    
    
    if(Cadastro_de_municipios$ID_Meso[lin] %in% missingRegions){
      tbl[, col_destino] <- NA
    } else {
      
      
      tbl[, col_destino] <- IRF[, col_origem]
    }
    
  }
  
}



# Calcula regioes faltantes -----------------------------------------------

# 1301 1302 5102 5103 5201 2403 2502 2801 2203
# Regiao - 1301

tbl[, "R_1301_E"] = (9/99) * tbl[, "R_1201_E"] +
  (9/99) * tbl[, "R_1304_E"] +
  (45/99) * tbl[, "R_1303_E"] +
  (36/99) * tbl[, "R_1402_E"]

tbl[, "R_1301_D"] = (9/99) * tbl[, "R_1201_D"] +
  (9/99) * tbl[, "R_1304_D"] +
  (45/99) * tbl[, "R_1303_D"] +
  (36/99) * tbl[, "R_1402_D"]

tbl[, "R_1301_G"] = (9/99) * tbl[, "R_1201_G"] +
  (9/99) * tbl[, "R_1304_G"] +
  (45/99) * tbl[, "R_1303_G"] +
  (36/99) * tbl[, "R_1402_G"]


# Regiao - 1302
tbl[, "R_1302_E"] = (27/99) * tbl[, "R_1201_E"] +
  (27/99) * tbl[, "R_1304_E"] +
  (36/99) * tbl[, "R_1303_E"] + 
  (9/99) * tbl[, "R_1402_E"]

tbl[, "R_1302_D"] = (27/99) * tbl[, "R_1201_D"] +
  (27/99) * tbl[, "R_1304_D"] +
  (36/99) * tbl[, "R_1303_D"] + 
  (9/99) * tbl[, "R_1402_D"]

tbl[, "R_1302_G"] = (27/99) * tbl[, "R_1201_G"] +
  (27/99) * tbl[, "R_1304_G"] +
  (36/99) * tbl[, "R_1303_G"] + 
  (9/99) * tbl[, "R_1402_G"]



# Regiao - 5102 
tbl[, "R_5102_E"] = 0.25 * tbl[, "R_1701_E"] +
  (625/3000) * tbl[, "R_1506_E"] +
  (625/3000) * tbl[, "R_5101_E"] +
  (625/3000) * tbl[, "R_5105_E"] +
  (125/3000) * tbl[, "R_5202_E"] +
  (125/3000) * tbl[, "R_5203_E"] +
  (125/3000) * tbl[, "R_5205_E"] 

tbl[, "R_5102_D"] = 0.25 * tbl[, "R_1701_D"] +
  (625/3000) * tbl[, "R_1506_D"] +
  (625/3000) * tbl[, "R_5101_D"] +
  (625/3000) * tbl[, "R_5105_D"] +
  (125/3000) * tbl[, "R_5202_D"] +
  (125/3000) * tbl[, "R_5203_D"] +
  (125/3000) * tbl[, "R_5205_D"] 

tbl[, "R_5102_G"] = 0.25 * tbl[, "R_1701_G"] +
  (625/3000) * tbl[, "R_1506_G"] +
  (625/3000) * tbl[, "R_5101_G"] +
  (625/3000) * tbl[, "R_5105_G"] +
  (125/3000) * tbl[, "R_5202_G"] +
  (125/3000) * tbl[, "R_5203_G"] +
  (125/3000) * tbl[, "R_5205_G"] 


# Regiao - 5201
tbl[, "R_5201_E"] = 0.25 * tbl[, "R_1701_E"] +
  (125/3000) * tbl[, "R_1506_E"] +
  (125/3000) * tbl[, "R_5101_E"] +
  (125/3000) * tbl[, "R_5105_E"] +
  (625/3000) * tbl[, "R_5202_E"] +
  (625/3000) * tbl[, "R_5203_E"] +
  (625/3000) * tbl[, "R_5205_E"] 

tbl[, "R_5201_D"] = 0.25 * tbl[, "R_1701_D"] +
  (125/3000) * tbl[, "R_1506_D"] +
  (125/3000) * tbl[, "R_5101_D"] +
  (125/3000) * tbl[, "R_5105_D"] +
  (625/3000) * tbl[, "R_5202_D"] +
  (625/3000) * tbl[, "R_5203_D"] +
  (625/3000) * tbl[, "R_5205_D"] 

tbl[, "R_5201_G"] = 0.25 * tbl[, "R_1701_G"] +
  (125/3000) * tbl[, "R_1506_G"] +
  (125/3000) * tbl[, "R_5101_G"] +
  (125/3000) * tbl[, "R_5105_G"] +
  (625/3000) * tbl[, "R_5202_G"] +
  (625/3000) * tbl[, "R_5203_G"] +
  (625/3000) * tbl[, "R_5205_G"] 



# Regiao - 5103
tbl[, "R_5103_E"] = 0.5 * tbl[, "R_5101_E"] +  0.5 * tbl[, "R_5104_E"]
tbl[, "R_5103_D"] = 0.5 * tbl[, "R_5101_D"] +  0.5 * tbl[, "R_5104_D"]
tbl[, "R_5103_G"] = 0.5 * tbl[, "R_5101_G"] +  0.5 * tbl[, "R_5104_G"]

# Regiao - 2403
tbl[, "R_2403_E"] = (1/3)*tbl[, "R_2503_E"] +
  (1/3)*tbl[, "R_2404_E"] +
  (1/3)*tbl[, "R_2402_E"]

tbl[, "R_2403_D"] = (1/3)*tbl[, "R_2503_D"] +
  (1/3)*tbl[, "R_2404_D"] +
  (1/3)*tbl[, "R_2402_D"]

tbl[, "R_2403_G"] = (1/3)*tbl[, "R_2503_G"] +
  (1/3)*tbl[, "R_2404_G"] +
  (1/3)*tbl[, "R_2402_G"]


# Regiao - 2502
tbl[, "R_2502_E"] = (0.2)*tbl[, "R_2601_E"] +
  (0.2)*tbl[, "R_2603_E"] +
  (0.2)*tbl[, "R_2503_E"] +
  (0.2)*tbl[, "R_2402_E"] +
  (0.2)*tbl[, "R_2501_E"]

tbl[, "R_2502_D"] = (0.2)*tbl[, "R_2601_D"] +
  (0.2)*tbl[, "R_2603_D"] +
  (0.2)*tbl[, "R_2503_D"] +
  (0.2)*tbl[, "R_2402_D"] +
  (0.2)*tbl[, "R_2501_D"]


tbl[, "R_2502_G"] = (0.2)*tbl[, "R_2601_G"] +
  (0.2)*tbl[, "R_2603_G"] +
  (0.2)*tbl[, "R_2503_G"] +
  (0.2)*tbl[, "R_2402_G"] +
  (0.2)*tbl[, "R_2501_G"]


# Regiao - 2203
tbl[, "R_2203_E"] = (0.2)*tbl[, "R_2204_E"] +
  (0.2)*tbl[, "R_2902_E"] +
  (0.2)*tbl[, "R_2901_E"] +
  (0.2)*tbl[, "R_2105_E"] +
  (0.2)*tbl[, "R_2104_E"] 

tbl[, "R_2203_D"] = (0.2)*tbl[, "R_2204_D"] +
  (0.2)*tbl[, "R_2902_D"] +
  (0.2)*tbl[, "R_2901_D"] +
  (0.2)*tbl[, "R_2105_D"] +
  (0.2)*tbl[, "R_2104_D"] 

tbl[, "R_2203_G"] = (0.2)*tbl[, "R_2204_G"] +
  (0.2)*tbl[, "R_2902_G"] +
  (0.2)*tbl[, "R_2901_G"] +
  (0.2)*tbl[, "R_2105_G"] +
  (0.2)*tbl[, "R_2104_G"] 


# Regiao - 2801
tbl[, "R_2801_E"] = (1/3)*tbl[, "R_2904_E"] + (1/3)*tbl[, "R_2701_E"] + (1/3)*tbl[, "R_2802_E"]

tbl[, "R_2801_D"] = (1/3)*tbl[, "R_2904_D"] + (1/3)*tbl[, "R_2701_D"] + (1/3)*tbl[, "R_2802_D"] 

tbl[, "R_2801_G"] = (1/3)*tbl[, "R_2904_G"] + (1/3)*tbl[, "R_2701_G"] + (1/3)*tbl[, "R_2802_G"] 




# Transforma dados em longer para Excel -----------------------------------

tbl_longer <- tbl %>% pivot_longer(cols = -Id,
                                   names_to = "Serie",
                                   values_to = "Valor" )



tbl_longer[ , c("Skip", "Region",  "Fuel")] <- stringr::str_split(string = tbl_longer$Serie, 
                                                                  pattern = "_", simplify = TRUE)

tbl_longer$Fuel <- factor(tbl_longer$Fuel,
                          labels = c("Ethanol", "Gasoline", "Diesel"),
                          levels = c("E", "G", "D"))

tbl_longer <- tbl_longer %>% pivot_wider(id_cols = c("Id", "Region"),
                                         names_from = "Fuel",
                                         values_from = Valor)

tbl_longer$Region <- as.numeric(tbl_longer$Region)


tbl_longer <- tbl_longer %>% mutate(Date = Sys.Date() + tbl_longer$Id,
                                    Ethanol_p = if_else(Ethanol >= 0, Ethanol, 0),
                                    Ethanol_n = if_else(Ethanol < 0, -Ethanol, 0),
                                    Diesel_p = if_else(Diesel >= 0, Diesel, 0),
                                    Diesel_n = if_else(Diesel < 0, -Diesel, 0),
                                    Gasoline_p = if_else(Gasoline >= 0, Gasoline, 0),
                                    Gasoline_n = if_else(Gasoline < 0, -Gasoline, 0))


writexl::write_xlsx(x = tbl_longer,
                    path = sprintf("./Excel Videos/Shoques_v1 (%s).xlsx", model))
