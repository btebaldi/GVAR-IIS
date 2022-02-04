#' Author: Bruno Tebaldi Barbosa
#'
#' 2021-11-09
#'
#' Script Pada extracao de estatistica descritiva das series.
#' 
#' 

# Setup -------------------------------------------------------------------
rm(list = ls())

library(dplyr)



# Data load ---------------------------------------------------------------

tbl <- readRDS(file = "./database/db_oil_forForecast2.rds")



# Table with summary ------------------------------------------------------

tabble_summary <- tibble(Regiao = as.character(NA),
                         Serie =  as.character(NA),
                         Mean = as.numeric(NA),
                         Median = as.numeric(NA),
                         Max = as.numeric(NA),
                         Min = as.numeric(NA),
                         Std_Dev = as.numeric(NA),
                         Skewness = as.numeric(NA),
                         Kurtosis = as.numeric(NA),
                         .rows = length(colnames(tbl)))

for (i in seq_along(colnames(tbl))) {
  print(colnames(tbl)[i])
  tabble_summary$Regiao[i] <- colnames(tbl)[i]
  tabble_summary$Serie[i] <- colnames(tbl)[i]
  
  mean(tbl[[i]])
  median(tbl[[i]])
  
}

