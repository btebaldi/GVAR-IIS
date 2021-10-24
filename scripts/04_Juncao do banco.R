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

# for (PRODUTO in colnames(Gas_by_municipio)) {
#   
#   if(PRODUTO == "ID_Municipio"){next()}
#   
#   tbl <- Gas_by_municipio %>%
#     group_by(.data[[!!PRODUTO]]) %>% 
#     summarise(QtdMunicipios=n(), .groups = "drop") %>%
#     arrange(QtdMunicipios)
#     
#   readr::write_excel_csv(x = tbl, file = sprintf("./export/04-%s.csv", PRODUTO))
# }



# Analise de mesoregioes com dados de consumo energetico ------------------

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



# Analise de periodos com mais mesoregioes --------------------------------


Total_Meso <- length(unique(Gas_meso$ID_Meso))

Gas_meso %>%
  group_by(DATA_INICIAL, DATA_FINAL, PRODUTO) %>% 
  summarise(n=n(), .groups = "drop") %>% 
  pivot_wider(names_from = PRODUTO, values_from = n) %>% 
  dplyr::select(DATA_INICIAL, DATA_FINAL, ETANOL_HIDRATADO, OLEO_DIESEL, GASOLINA_COMUM) %>% 
  mutate(perc_ETANOL = ETANOL_HIDRATADO/Total_Meso,
         perc_OLEO = OLEO_DIESEL/Total_Meso,
         perc_GASOLINA = GASOLINA_COMUM/Total_Meso) -> Gas_by_periodo_resumo


ggplot(Gas_by_periodo_resumo) +
  geom_line(aes(x= DATA_INICIAL, y=perc_GASOLINA, colour="GAS")) + 
  geom_line(aes(x= DATA_INICIAL, y=perc_ETANOL, colour="ETANOL")) + 
  geom_line(aes(x= DATA_INICIAL, y=perc_OLEO, colour="OLEO")) + 
  labs()


Gas_by_meso_resumo %>% filter(check < 0.95) %>% transmute(Estado=trunc(ID_Meso/100)) %>% distinct()





# Analise de estados com dados de consumo energetico ----------------------

Gas_estado <- readRDS("./database/Gasolina_estado.rds")

Gas_estado <- Gas_estado %>% filter(DATA_FINAL < final_date)

total_N <- length(unique(Gas_estado$DATA_INICIAL))

Gas_by_estado <- Gas_estado %>%
  group_by(ESTADO, PRODUTO) %>%
  summarise(n=n(), .groups = "drop") %>% 
  pivot_wider(names_from = PRODUTO, values_from = n) %>% 
  dplyr::select(ESTADO, ETANOL_HIDRATADO, OLEO_DIESEL, GASOLINA_COMUM)

Gas_by_estado %>%
  mutate(Min = pmin(ETANOL_HIDRATADO, OLEO_DIESEL, GASOLINA_COMUM),
         check = Min /total_N) %>% 
  arrange(desc(Min)) -> Gas_by_estado_resumo

readr::write_excel_csv(x = Gas_by_estado_resumo, file = "./export/04-Gas_by_estado_resumo.csv")



# Analise de periodos com mais mesoregioes --------------------------------


Total_Estado <- length(unique(Gas_estado$ESTADO))

Gas_estado %>%
  group_by(DATA_INICIAL, DATA_FINAL, PRODUTO) %>% 
  summarise(n=n(), .groups = "drop") %>% 
  pivot_wider(names_from = PRODUTO, values_from = n) %>% 
  dplyr::select(DATA_INICIAL, DATA_FINAL, ETANOL_HIDRATADO, OLEO_DIESEL, GASOLINA_COMUM) %>% 
  mutate(perc_ETANOL = ETANOL_HIDRATADO/Total_Meso,
         perc_OLEO = OLEO_DIESEL/Total_Meso,
         perc_GASOLINA = GASOLINA_COMUM/Total_Meso) -> Gas_by_periodo_resumo_estado


ggplot(Gas_by_periodo_resumo_estado) +
  geom_line(aes(x= DATA_INICIAL, y=perc_GASOLINA, colour="GAS")) + 
  geom_line(aes(x= DATA_INICIAL, y=perc_ETANOL, colour="ETANOL")) + 
  geom_line(aes(x= DATA_INICIAL, y=perc_OLEO, colour="OLEO")) + 
  labs()


Gas_by_meso_resumo %>% filter(check < 0.95) %>% transmute(Estado=trunc(ID_Meso/100)) %>% distinct()

