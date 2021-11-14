#' Author: Bruno Tebaldi Barbosa
#'
#' 2021-11-09
#'
#' Script que faz a Construcao das matrizes de peso do GVAR
#' 
#' 
# Setup -------------------------------------------------------------------

rm(list = ls())

library(dplyr)
library(tidyr)
# library(dlm)
library(readxl)


# User defined Functions --------------------------------------------------

SaveMatrixForOx <- function(Matrix, file_name, comment = NULL){
  if(is.null(comment)){comment = file_name}
  
  file.name <- sprintf("./export/W_mat/%s.mat", file_name)
  
  fileConn <- file(file.name)
  writeLines( text = sprintf("%1$d %2$d // A %1$d by %2$d matrix (%3$s)", 
                             dim(Matrix)[1],
                             dim(Matrix)[2],
                             comment),
              con = fileConn)
  close(fileConn)
  
  write.table(x = Matrix, file = file.name,
              append = TRUE,
              col.names = FALSE,
              row.names = FALSE)
}


# Load Data ---------------------------------------------------------------

# Carrega o mapeamento de cidades, municipios e agrupamentos
Mapeamento <- read_excel("database/Cadastro de municipios.xlsx", sheet = "Mapeamento")
head(Mapeamento)

#  Seleciona os agrupamentos unicos.
Mapeamento <- Mapeamento %>% 
  select(ID_Meso, Agrupamento) %>% distinct()


# Carrega o cadastro de municipios
Municipios <- read_excel("database/Cadastro de municipios.xlsx", 
                         sheet = "Municipios",
                         range = cell_limits(ul = c(1, 1), lr = c(NA, 11)))
head(Municipios)

# Carrega a base de dados de conexoes entre as cidades
Connexoes.df <- read_excel("C:/Users/bruno.barbosa/Downloads/Ligacoes_entre_Cidades2.xlsx", 
                           sheet = "ligacoes")
head(Connexoes.df)




# Ajustes -----------------------------------------------------------------

# Agrupa os municipios em mesoregioes para que possamos calcular os agrupamentos
# e mesoregioes
Meso <- Municipios %>% 
  filter(!is.na(PIB)) %>% 
  group_by(ID_Meso) %>% 
  summarise(PIB = sum(PIB),
            POP = sum(Populacao_2016),
            .groups = "drop")

head(Meso)

tbl <- Mapeamento %>% 
  left_join(Meso, by = c("ID_Meso" = "ID_Meso")) %>% 
  group_by(Agrupamento) %>% 
  summarise(PIB = sum(PIB),
            POP = sum(POP),
            .groups = "drop") %>% 
  mutate(PIB_PC = PIB/POP)

tail(tbl)
# Contrucao de Matriz de pesos --------------------------------------------

# Constroe uma matrix de pessos zerada
qtd_of_regions <- nrow(tbl)

# Vetor com codigo dos agrupamentos
cod_agrupamento <- sort(tbl$Agrupamento)

# Inicializa as matrizes de peso
W.mat <- matrix(NA, ncol = qtd_of_regions, nrow = qtd_of_regions)
colnames(W.mat) <- paste("R", cod_agrupamento, sep = "_")
rownames(W.mat) <- paste("R", cod_agrupamento, sep = "_")

W.mat_con <- W.mat
W.mat_pop_1 <- W.mat
W.mat_pop_2 <- W.mat
W.mat_pib_1 <- W.mat
W.mat_pib_2 <- W.mat
W.mat_pib_pc_1 <- W.mat
W.mat_pib_pc_2 <- W.mat

# 151699
# 2199

counter = 0
for (col in 1:ncol(W.mat)) {
  
  # busca qual o codigo de agrupamento do indice atual (origem)
  # Agrupamento_origem <- 151699
  Agrupamento_origem <- cod_agrupamento[col]
  
  # busca o codigo da coluna (regiao de origem)
  Origem <- Mapeamento %>% filter(Agrupamento == Agrupamento_origem) %>% pull(ID_Meso)
  
  # Agrupamento_destino <- 1101
  for (row in 1:nrow(W.mat)) {
    counter = counter+1
    cat(sprintf("%d : ", counter))
    
    # busca qual o codigo de agrupamento do indice atual (destino)
    Agrupamento_destino <- cod_agrupamento[row]
    
    # busca o codigo da coluna (regiao de origem)
    Destino <- Mapeamento %>% filter(Agrupamento == Agrupamento_destino) %>% pull(ID_Meso)
    
    # Busca informacoes de destino 
    Destino_pop <- tbl %>% filter(Agrupamento == Agrupamento_destino) %>% pull(POP)
    Destino_pib <- tbl %>% filter(Agrupamento == Agrupamento_destino) %>% pull(PIB)
    Destino_pib_pc <- tbl %>% filter(Agrupamento == Agrupamento_destino) %>% pull(PIB_PC)
    
    
    if(Agrupamento_origem == Agrupamento_destino){
      cat("Origem  = destino", "\n")
      W.mat_con[row, col] <- 0;
      
      W.mat_pop_1[row, col] <- 0;
      W.mat_pop_2[row, col] <- 0;
      W.mat_pib_1[row, col] <- 0;
      W.mat_pib_2[row, col] <- 0;
      W.mat_pib_pc_1[row, col] <- 0;
      W.mat_pib_pc_2[row, col] <- 0;
      
    } else {
      
      tbl_aux <- Connexoes.df %>% 
        filter(Meso_ori %in% Origem,
               Meso_des %in% Destino) %>% 
        summarise(Total_Conn=n(),
                  Dist_avg = mean(dist_km))
      
      if(tbl_aux$Total_Conn > 0){
        cat(sprintf("n = %s", tbl_aux$Total_Conn), "\n")
        W.mat_con[row, col] <- tbl_aux$Total_Conn
        
        W.mat_pop_1[row, col] <- Destino_pop;
        W.mat_pop_2[row, col] <- Destino_pop/tbl_aux$Dist_avg;
        W.mat_pib_1[row, col] <- Destino_pib;
        W.mat_pib_2[row, col] <- Destino_pib/tbl_aux$Dist_avg;
        W.mat_pib_pc_1[row, col] <- Destino_pib_pc;
        W.mat_pib_pc_2[row, col] <- Destino_pib_pc/tbl_aux$Dist_avg;
        
        # W.mat_dist[row, col] <- tbl_aux$Dist_avg
      } else{
        cat("Nao existe destino", "\n")
        W.mat_con[row, col] <- 0;
        
        W.mat_pop_1[row, col] <- 0;
        W.mat_pop_2[row, col] <- 0;
        W.mat_pib_1[row, col] <- 0;
        W.mat_pib_2[row, col] <- 0;
        W.mat_pib_pc_1[row, col] <- 0;
        W.mat_pib_pc_2[row, col] <- 0;
      }
    }
    
  }
  
}


# --- Normaliza as colunas da matrix ----
NormalizaColunas <- function(Matrix){
  for (col in 1:ncol(Matrix)) {
    somaColuna <- sum(Matrix[,col], na.rm = TRUE);
    if(somaColuna == 0){
      stop("Coluna zerada")
    }
    Matrix[,col] <- Matrix[,col] / somaColuna
  }
  
  return(Matrix)
}

W.mat_con <- NormalizaColunas(W.mat_con)

W.mat_pop_1 <- NormalizaColunas(W.mat_pop_1)
W.mat_pop_2 <- NormalizaColunas(W.mat_pop_2)

W.mat_pib_1 <- NormalizaColunas(W.mat_pib_1)
W.mat_pib_2 <- NormalizaColunas(W.mat_pib_2)

W.mat_pib_pc_1 <- NormalizaColunas(W.mat_pib_pc_1)
W.mat_pib_pc_2 <- NormalizaColunas(W.mat_pib_pc_2)

# --- Salva a matrix em arquivo .mat ----
SaveMatrixForOx(Matrix = W.mat_con, file_name = "CONN", comment = "Connections")

SaveMatrixForOx(Matrix = W.mat_pop_1, file_name = "POP_1", comment = "Population Raw")
SaveMatrixForOx(Matrix = W.mat_pop_2, file_name = "POP_2", comment = "Population by distance")

SaveMatrixForOx(Matrix = W.mat_pib_1, file_name = "PIB_1", comment = "Pib Raw")
SaveMatrixForOx(Matrix = W.mat_pib_2, file_name = "PIB_2", comment = "Pib by distance")

SaveMatrixForOx(Matrix = W.mat_pib_pc_1, file_name = "PIB_PC_1", comment = "Pib Raw")
SaveMatrixForOx(Matrix = W.mat_pib_pc_2, file_name = "PIB_PC_2", comment = "Pib by distance")

















