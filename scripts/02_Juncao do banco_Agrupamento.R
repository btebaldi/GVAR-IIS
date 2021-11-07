
# Setup -------------------------------------------------------------------

rm(list=ls())

library(dplyr)
library(readxl)
library(tidyr)

# Configuracao de labels and levels
level_n_labels_path <- file.path("./database/levels_and_labels.xlsx")

# Carrega banco de dados  -------------------------------------------------

# Carrega banco de dados de historico de gasolina
Gas <- readRDS("./database/Gasolina.rds")

# Carrega o banco de dados dos municipios
Cadastro_de_municipios <- read_excel("database/Cadastro de municipios.xlsx", 
                                     sheet = "Mapeamento")

# Carrega levels and labels
LL_Estados <- read_excel(level_n_labels_path, sheet = "Estados")
LL_Regiao <- read_excel(level_n_labels_path, sheet = "Regiao")

Cadastro_de_municipios$REGIAO <- factor(Cadastro_de_municipios$REGIAO, labels = LL_Regiao$labels, levels = LL_Regiao$levels)
Cadastro_de_municipios$ESTADO <- factor(Cadastro_de_municipios$ESTADO, labels = LL_Estados$labels, levels = LL_Estados$levels)


Gas <- Gas %>% inner_join(Cadastro_de_municipios, by = c("REGIAO", "ESTADO", "MUNICIPIO"))


# Agrupamento -------------------------------------------------------------

summary(Gas)

tbl.Agrupamento <- Gas %>% 
  select(DATA_INICIAL,
         DATA_FINAL,
         REGIAO,
         Agrupamento,
         PRODUTO,
         NUM_PESQUISADOS,
         PRECO_MEDIO_REVENDA,
         DESVIO_PADRAO_REVENDA,
         PRECO_MINIMO_REVENDA,
         PRECO_MAXIMO_REVENDA) %>% 
  group_by(DATA_INICIAL, DATA_FINAL, REGIAO, Agrupamento, PRODUTO) %>% 
  summarise( NUM_PESQUISADOS = sum(NUM_PESQUISADOS, na.rm = TRUE),
             PRECO_MEDIO_REVENDA = mean(PRECO_MEDIO_REVENDA, na.rm = TRUE),
             DESVIO_PADRAO_REVENDA = (sum(DESVIO_PADRAO_REVENDA^2, na.rm = TRUE))^0.5,
             PRECO_MINIMO_REVENDA = min(PRECO_MINIMO_REVENDA, na.rm = TRUE),
             PRECO_MAXIMO_REVENDA = max(PRECO_MAXIMO_REVENDA, na.rm = TRUE),
             .groups = "drop")


write_rds(x = tbl.Agrupamento, file = "./database/Gasolina_Agrupamento.rds")
