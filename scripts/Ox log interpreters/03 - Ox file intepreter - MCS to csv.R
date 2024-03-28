
# Setup -------------------------------------------------------------------
rm(list=ls())
# You should take care with readLines(...) and big files. Reading all lines at
# memory can be risky. Below is a example of how to read file and process just
# one line at time:


# Bibliotecas -------------------------------------------------------------

library(stringr)
library(dplyr)

getwd()
# Variaveis internas ------------------------------------------------------

fileName = "MCS no OX.out.txt"

filepath = file.path(".", fileName)

if(!file.exists(filepath)) {
  stop("Arquivo nao existe")
}


# Leitura do arquivo de texto ---------------------------------------------

ReadCon  <- file(description = filepath, open = "r")
WriteCon <- file(description = "./MCS_Results.txt", open = "w")


tbl.results <- tibble(ModelCode = as.character(NA),
                      mad = as.numeric(NA),
                      MCS_pValue = as.numeric(NA),
                      Critical = as.character(NA) )

isWriteOn = FALSE
while ( TRUE ) {
  
  line = readLines(ReadCon, n = 1)
  # line
  # Se a linha tem tamanho zero entao para o processamento
  if ( length(line) == 0 ) {
    break
  }
  
  # verifica e o comeco do processamento
  if(stringr::str_detect(line, stringr::regex("Bootstrap parameters: "))){
    isWriteOn = TRUE
    print(line)
  }
  
  # verifica e o comeco do processamento
  if(stringr::str_detect(line, stringr::regex("Level 0\\.1 Model Confidence Set"))){
    isWriteOn = FALSE
    print(line)
  }
  
  
  # verifica a regiao atual
  if(isWriteOn){
    
    m_row <- unlist(stringr::str_split(string = line, pattern = " +"))
    
    if(m_row[1] == "Bootstrap"){
      next
    } else if(m_row[1] == "Model"){
      next
    }
    tbl.results <- tbl.results %>% add_row(ModelCode = m_row[1],
                                           mad = as.numeric(m_row[2]),
                                           MCS_pValue = as.numeric(m_row[3]),
                                           Critical = m_row[4]  )
    rm(list = "m_row")
    tbl.results
    writeLines(text = line, con = WriteCon)
    
    print(line)
    
  }
  
}

close(ReadCon)
close(WriteCon)


# Data Cleaning -----------------------------------------------------------

tbl.results <- tbl.results %>% 
  filter(!is.na(ModelCode)) %>% 
  filter( ModelCode != "")


tbl.results <- stringr::str_split(tbl.results$ModelCode, "_", simplify = TRUE) %>% 
  as_tibble() %>% 
  dplyr::bind_cols(tbl.results)

tbl.results <- tbl.results %>% 
  rename(Combustivel = V1, 
         Model = V2,
         Region = V3) %>% 
  mutate(Combustivel = factor(x = Combustivel,
                              levels = c("E", "G", "D"),
                              labels = c("Ethanol", "Gasoline", "Diesel")))

saveRDS(object = tbl.results, file = "tbl.results.RDS")
write.csv2(tbl.results, file = "tbl.results.CSV")
