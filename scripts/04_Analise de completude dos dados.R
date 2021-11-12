#' Author: Bruno Tebaldi Barbosa
#'
#' 2021-10-24
#'
#' Script que faz a analsie de quais os municipios mais completos no reporting
#' dos preços

# Setup -------------------------------------------------------------------

rm(list=ls())

library(dplyr)
library(readxl)
library(tidyr)

# Configuracao de labels and levels
level_n_labels_path <- file.path("./database/levels_and_labels.xlsx")

# Data final utilizada na analise dos dados
final_date <- as.Date("2019-01-01")


# Carrega banco de dados  -------------------------------------------------

# Carrega banco de dados de historico de gasolina
Gas <- readRDS("./database/Gasolina.rds")

Gas <- Gas %>% filter(DATA_FINAL < final_date)

# Carrega o banco de dados dos municipios
Cadastro_de_municipios <- read_excel("database/Cadastro de municipios.xlsx", 
                                     sheet = "Mapeamento")

# Carrega levels and labels
LL_Estados <- read_excel(level_n_labels_path, sheet = "Estados")
LL_Regiao <- read_excel(level_n_labels_path, sheet = "Regiao")

Cadastro_de_municipios$REGIAO <- factor(Cadastro_de_municipios$REGIAO, labels = LL_Regiao$labels, levels = LL_Regiao$levels)
Cadastro_de_municipios$ESTADO <- factor(Cadastro_de_municipios$ESTADO, labels = LL_Estados$labels, levels = LL_Estados$levels)


Gas <- Gas %>% inner_join(Cadastro_de_municipios, by = c("REGIAO", "ESTADO", "MUNICIPIO"))


# Analise de municipios com dados de consumo energetico -------------------

lst_data_inicial <- unique(Gas$DATA_INICIAL)
total_N <- length(lst_data_inicial)


Gas_by_municipio <- Gas %>%
  group_by(ESTADO, MUNICIPIO, ID_Municipio, PRODUTO) %>%
  summarise(n=n(), .groups = "drop") %>% 
  pivot_wider(names_from = PRODUTO, values_from = n) %>% 
  dplyr::select(ESTADO, MUNICIPIO, ID_Municipio, ETANOL_HIDRATADO, OLEO_DIESEL, GASOLINA_COMUM)

Gas_by_municipio %>%
  mutate(Min = pmin(ETANOL_HIDRATADO, OLEO_DIESEL, GASOLINA_COMUM),
         check = Min /total_N) %>% 
  arrange(desc(Min)) -> Gas_by_municipio_resumo
  
readr::write_excel_csv(x = Gas_by_municipio_resumo, file = "./export/04-Gas_by_municipio.csv")

# Analise de mesoregioes com dados de consumo energetico ------------------
rm(list=setdiff(ls(), c("final_date")))

Gas_meso <- readRDS("./database/Gasolina_meso.rds")

Gas_meso <- Gas_meso %>% filter(DATA_FINAL < final_date)

total_N <- length(unique(Gas_meso$DATA_INICIAL))

Gas_by_meso <- Gas_meso %>%
  group_by(ID_Meso, PRODUTO) %>%
  summarise(n=n(), .groups = "drop") %>% 
  pivot_wider(names_from = PRODUTO, values_from = n) %>% 
  dplyr::select(ID_Meso, ETANOL_HIDRATADO, OLEO_DIESEL, GASOLINA_COMUM)

Gas_by_meso %>%
  mutate(Min = pmin(ETANOL_HIDRATADO, OLEO_DIESEL, GASOLINA_COMUM),
         check = Min /total_N) %>% 
  arrange(desc(Min)) -> Gas_by_meso_resumo

readr::write_excel_csv(x = Gas_by_meso_resumo, file = "./export/04-Gas_by_meso_resumo.csv")

# Analise do Agrupamento propostp com dados de consumo energetico ------------------
rm(list=setdiff(ls(), c("final_date")))

Gas_agrupo <- readRDS("./database/Gasolina_Agrupamento.rds")

Gas_agrupo <- Gas_agrupo %>% filter(DATA_FINAL < final_date)

total_N <- length(unique(Gas_agrupo$DATA_INICIAL))

Gas_by_agrupo <- Gas_agrupo %>%
  group_by(Agrupamento, PRODUTO) %>%
  summarise(n=n(), .groups = "drop") %>% 
  pivot_wider(names_from = PRODUTO, values_from = n) %>% 
  dplyr::select(Agrupamento, ETANOL_HIDRATADO, OLEO_DIESEL, GASOLINA_COMUM)

Gas_by_agrupo %>%
  mutate(Min = pmin(ETANOL_HIDRATADO, OLEO_DIESEL, GASOLINA_COMUM),
         check = Min /total_N) %>% 
  arrange(desc(Min)) -> Gas_by_agrupo_resumo

readr::write_excel_csv(x = Gas_by_agrupo_resumo, file = "./export/04-Gas_by_Agrupamento_resumo.csv")




colnames(Gas_agrupo)
Gas_agrupo %>% select(DATA_INICIAL, Agrupamento, PRODUTO, PRECO_MEDIO_REVENDA) %>% 
  pivot_wider(id_cols = c("Agrupamento", "PRODUTO"),
              names_from = DATA_INICIAL, 
              values_from = PRECO_MEDIO_REVENDA,
              names_prefix = "D") %>% 
  filter(Agrupamento == 2601) %>% 
  filter(PRODUTO %in% c("ETANOL_HIDRATADO", "OLEO_DIESEL", "GASOLINA_COMUM")) -> b

plot(is.na(t(data.matrix(b[2,c(-1, -2)]))))





