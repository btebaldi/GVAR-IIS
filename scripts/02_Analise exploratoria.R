#' AUTOR: Bruno Tebaldi de Q Barbosa
#' 
#' Data: 2021-09-24
#' 
#' Analise exploratoria no banco de dados


# Setup -------------------------------------------------------------------

rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)


# Load --------------------------------------------------------------------

tbl <- readRDS("database/Gasolina.rds")


tbl1 <- tbl %>% filter(DATA_INICIAL < as.Date("2020-01-01"))
dt <- sort(unique(tbl1$DATA_INICIAL))

dt - lag(dt)



