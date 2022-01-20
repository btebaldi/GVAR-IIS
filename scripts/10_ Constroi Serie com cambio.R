#' Author: Bruno Tebaldi Barbosa
#'
#' 2021-11-09
#'
#' Script acrescenta informacao de cambio ao banco de dados.
#' 
#' 
# Setup -------------------------------------------------------------------

rm(list = ls())

library(readr)
library(dplyr)
library(tidyr)
library(dlm)
library(readxl)

# User defined Functions --------------------------------------------------


# Data Load ---------------------------------------------------------------
tbl <- readRDS(file = "./database/db_oil_forForecast.rds")

cambio <- read_excel("database/Cambio.xlsx",
                     col_types = c("text",  "numeric", "numeric")
                     )

cambio$Data2 <- as.Date(cambio$Data, format = "%d/%m/%Y")

# tbl.dates <- tbl %>% select(DATA_INICIAL, DATA_FINAL)

tb.cambio <- cambio %>%
  group_by(Grupo) %>% 
  summarise(DATA_INICIAL = min(Data2),
            DATA_FINAL = max(Data2),
            Cambio = mean(Cambio, na.rm=TRUE))

tbl <- tbl %>%
  inner_join(tb.cambio, by = c("DATA_INICIAL", "DATA_FINAL")) %>% 
  mutate(lnCambio = log(Cambio))


saveRDS(object = tbl, file = "./database/db_oil_forForecast2.rds")
readr::write_csv(x = tbl, file = "./export/database for ox/db_oil_forForecast2.csv")

tbl <- tbl %>% filter(year(DATA_INICIAL) < 2019)

saveRDS(object = tbl, file = "./database/db_oil_withDummies2.rds")
readr::write_csv(x = tbl, file = "./export/database for ox/db_oil_withDummies2.csv")



