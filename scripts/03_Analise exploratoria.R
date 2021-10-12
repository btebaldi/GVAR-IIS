

# Setup -------------------------------------------------------------------

rm(list=ls())

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)


# Load data ---------------------------------------------------------------

tbl.estado <- readr::read_rds("./database/Gasolina_estado.rds")
tbl.Meso <- readr::read_rds("./database/Gasolina_meso.rds")

tbl.estado$PRODUTO

# Analise exploratoria dos dados ------------------------------------------

summary(tbl.estado)

tbl.estado %>% 
  select(DATA_INICIAL, DATA_FINAL, ESTADO, PRODUTO, PRECO_MEDIO_REVENDA) %>% 
  mutate(PRECO_MEDIO_REVENDA = log(PRECO_MEDIO_REVENDA)) %>% 
  # pivot_wider(id_cols = c("DATA_INICIAL", "DATA_FINAL", "PRODUTO"), names_from = ESTADO, values_from = PRECO_MEDIO_REVENDA) %>% 
  dplyr::filter(PRODUTO %in% c("GASOLINA_COMUM", "GASOLINA_ADITIVADA", "ETANOL_HIDRATADO", "OLEO_DIESEL", "OLEO_DIESEL_S10")) %>% 
  ggplot() + 
  geom_line(aes(x=DATA_INICIAL, y=PRECO_MEDIO_REVENDA, colour=PRODUTO)) + 
  facet_wrap(~ESTADO)




tbl.estado %>% 
  select(DATA_INICIAL, DATA_FINAL, REGIAO, ESTADO, PRODUTO, PRECO_MEDIO_REVENDA) %>% 
  mutate(PRECO_MEDIO_REVENDA = log(PRECO_MEDIO_REVENDA)) %>% 
  # pivot_wider(id_cols = c("DATA_INICIAL", "DATA_FINAL", "PRODUTO"), names_from = ESTADO, values_from = PRECO_MEDIO_REVENDA) %>% 
  # dplyr::filter(PRODUTO %in% c("GASOLINA_COMUM", "GASOLINA_ADITIVADA", "ETANOL_HIDRATADO", "OLEO_DIESEL", "OLEO_DIESEL_S10")) %>% 
  dplyr::filter(PRODUTO %in% c("ETANOL_HIDRATADO")) %>% 
  ggplot() + 
  geom_boxplot(aes(y=ESTADO, x=PRECO_MEDIO_REVENDA, fill=REGIAO)) + 
  # facet_wrap(~ESTADO) +
  labs()


tbl.Meso2 <- tbl.Meso %>% 
  select(DATA_INICIAL, DATA_FINAL, ID_Meso, PRODUTO, PRECO_MEDIO_REVENDA) %>% 
  dplyr::filter(PRODUTO %in% c("GASOLINA_COMUM", "GASOLINA_ADITIVADA", "ETANOL_HIDRATADO", "OLEO_DIESEL", "OLEO_DIESEL_S10")) %>%
  pivot_wider(id_cols = c("DATA_INICIAL", "DATA_FINAL", "PRODUTO"),
              names_from = ID_Meso,
              names_prefix = "R_",
              values_from = PRECO_MEDIO_REVENDA)

for (col in colnames(tbl.Meso2)) {
  if (col %in% c("DATA_INICIAL", "DATA_FINAL", "PRODUTO") ) {
    next()
  }
  
  cat(sprintf("%s: %d",col, sum(is.na(tbl.Meso2[[col]]))), "\n")
}

tbl.Meso3 <- tbl.Meso %>% 
  select(DATA_INICIAL, DATA_FINAL, ID_Meso, PRODUTO, PRECO_MEDIO_REVENDA) %>% 
  dplyr::filter(PRODUTO %in% c("GASOLINA_COMUM", "GASOLINA_ADITIVADA", "ETANOL_HIDRATADO", "OLEO_DIESEL", "OLEO_DIESEL_S10")) %>%
  arrange(PRODUTO, ID_Meso) %>% 
  pivot_wider(id_cols = c("DATA_INICIAL", "DATA_FINAL"),
              names_from = c("ID_Meso", "PRODUTO"),
              names_prefix = "R_",
              values_from = PRECO_MEDIO_REVENDA)

for (col in colnames(tbl.Meso3)) {
  if (col %in% c("DATA_INICIAL", "DATA_FINAL", "PRODUTO") ) {
    next()
  }
  
  cat(sprintf("%s: %d",col, sum(is.na(tbl.Meso3[[col]]))), "\n")
}



tbl.estado2 <- tbl.estado %>% 
  select(DATA_INICIAL, DATA_FINAL, ESTADO, PRODUTO, PRECO_MEDIO_REVENDA) %>% 
  dplyr::filter(PRODUTO %in% c("GASOLINA_COMUM", "GASOLINA_ADITIVADA", "GLP", "GNV", "ETANOL_HIDRATADO", "OLEO_DIESEL", "OLEO_DIESEL_S10")) %>%
  arrange(PRODUTO, ESTADO) %>% 
  pivot_wider(id_cols = c("DATA_INICIAL", "DATA_FINAL"),
              names_from = c("ESTADO", "PRODUTO"),
              names_prefix = "R_",
              values_from = PRECO_MEDIO_REVENDA)

for (col in colnames(tbl.estado2)) {
  if (col %in% c("DATA_INICIAL", "DATA_FINAL", "PRODUTO") ) {
    next()
  }
  
  cat(sprintf("%s: %d",col, sum(is.na(tbl.estado2[[col]]))), "\n")
}

tbl.estado2$DATA_INICIAL[ which(is.na(tbl.estado2$R_PA))]

range(tbl.estado3$DATA_INICIAL)

tbl.estado3 %>% 
ggplot() + 
  geom_line(aes(x=DATA_INICIAL, y=R_SP)) + 
  facet_wrap(.~PRODUTO)




ETANOL_HIDRATADO
OLEO_DIESEL
GASOLINA_COMUM
GLP
GNV
OLEO_DIESEL_S10
GASOLINA_ADITIVADA



