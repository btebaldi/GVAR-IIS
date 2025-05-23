#' Tese Capitulo 2 - Analise de precos de gasolina, alcool e Oleo Diesel
#' 
#' Script para interpretar as matrizes do Ox.
#' 
#' Author: Bruno Tebaldi de Queiroz Barbosa
#' 
#' Data: 2022-01-06
#' 


# Setup -------------------------------------------------------------------
# Clear all
rm(list = ls())

# Load library
library(stringr)


# Configs -----------------------------------------------------------------

# Nome do modelo a ser lido
dir <- "Result 2025-04-27v5"

# Caminho dos arquivos
filepath = file.path("Ox","mat_files", "Result_Matrix", dir, "Read Me Config.txt")
file.out.path = file.path("Ox","mat_files", "Result_Matrix", dir, "Criterios_de_Inforacao.csv")
file.exists(filepath)

main_dir <- dirname(filepath)

# lista de arquivos a serem interpretados
mGyL.file_list <-   c(
  "mGy_inv_X_mC",
  "mGy_inv_X_mGyL1",
  "mGy_inv_X_mGyL2",
  "mGy_inv_X_mGyL3",
  "mGy_inv_X_mGyL4",
  "mGy_inv_X_mGyL5",
  "mGy_inv_X_mGyL6",
  "mGy_inv_X_mGyL7",
  "mGy_inv_X_mGyL8",
  # "mGy_inv_X_mGyL9",
  # "mGy_inv_X_mGyL10",
  # "mGy_inv_X_mGyL11",
  # "mGy_inv_X_mGyL12",
  # "mGy_inv_X_mGyL13",
  "mGy_inv_X_mL",
  "mGy_inv"
)

# for(item in mGyL.file_list) {}
for(item in mGyL.file_list) {
  
  # Caminho do arquivo a ser lido
  fileName <- sprintf("%s.mat", item)
  # fileName.mask <- paste(main_dir, "%s.mat", sep = "")
  
  fileName <- file.path(main_dir, fileName)
  # fileName <- sprintf(fileName.mask, item)
  
  cat(" Iniciando o processamento:", fileName, " \n")
  # Abre o aqruivo para leitura
  con <- file(fileName, open="r")
  
  # Faz a leitura das linhas
  line <- readLines(con) 
  
  # Fecha a conexao com o arquivo
  close(con)
  
  matrix_dimensions <- unlist(str_match(line[1], pattern = "^(\\d{1,4}) (\\d{1,4})"))
  matrix_dimensions <- as.numeric(matrix_dimensions[-1])
  
  # Matriz de pesos
  M <- matrix(NA, nrow = matrix_dimensions[1], ncol = matrix_dimensions[2])
  
  col = 1
  row <- 1
  for (i in 2:length(line)){
    
    line.splited <- str_split(line[i], "\\s+", n = Inf, simplify = FALSE)
    line.splited <- as.numeric(unlist(line.splited))
    
    for(j in 2:length(line.splited)){
      M[row, col] = line.splited[j]
      
      col = col + 1
    }
    
    # se chegou ao final, refaz as colunas
    if(col==(matrix_dimensions[2] + 1)){
      row = row+1
      col=1
    }
    
  }
  
  file.out <- file.path(main_dir, sprintf("%s.rds", item))
  cat("Salvando o arquivo:", file.out, "\n")
  saveRDS(M, file = file.out)
}




# Arquivo mGy_inv_X_mC ----------------------------------------------------
# 
# # Interpretação da constantes e Dummies sazonais associadas ao modelo.
# 
# # Caminho do arquivo a ser lido
# fileName <- sprintf("%s.mat", "mGy_inv_X_mC")
# 
# fileName <- file.path(main_dir, fileName)
# 
# 
# # Abre o aqruivo para leitura
# con <- file(fileName, open="r")
# 
# # Faz a leitura das linhas
# line <- readLines(con) 
# 
# # Fecha a conexao com o arquivo
# close(con)
# 
# # Matriz de pesos
# M <- matrix(NA, nrow = 1107, ncol = 300)
# 
# col = 1
# row <- 1
# for (i in 2:length(line)){
#   
#   line.splited <- str_split(line[i], "\\s+", n = Inf, simplify = FALSE)
#   line.splited <- as.numeric(unlist(line.splited))
#   
#   for(j in 2:length(line.splited)){
#     M[row, col] = line.splited[j]
#     
#     col = col + 1
#   }
#   
#   # se chegou ao final, refaz as colunas
#   if(col==301){
#     row= row+1
#     col=1
#   }
#   
# }
# 
# file.out <- file.path(main_dir, sprintf("%s.rds", "mGy_inv_X_mC"))
# saveRDS(M, file = file.out)

