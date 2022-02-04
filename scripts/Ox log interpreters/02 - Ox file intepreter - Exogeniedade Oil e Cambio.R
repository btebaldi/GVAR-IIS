
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

fileName = "2022-01-20 - Teste de Exogeniedade de Oil e cambio.txt"

filepath = file.path(".", "Ox", "output", fileName)

if(!file.exists(filepath)) {
  stop("Arquivo nao existe")
}

ReadCon  <- file(description = filepath, open = "r")
WriteCon <- file(description = "./Ox_Results.txt", open = "w")

ncount <- 0
ncount_2S  <- 0

tbl.results <- tibble(region = 1:110,
                      ExoTest = as.numeric(NA),
                      ExoTest_Boot = as.numeric(NA)
                      )



while ( TRUE ) {
  
  # inicializa as variaveis
  # region_number <- 0  # Numero da Regiao
  rank <- NA            # Rank test
  
  ExoTest <- NA         # Weak Exo Test
  ExoTest_Boot <- NA    # Alpha Beta Test
  # zerorank <- NA        # rank zero detectado
    WriteInfo <- FALSE

  # Auto1 <- NA
  # Auto2 <- NA
  # 
  #  Faz leitura da linha
  line = readLines(ReadCon, n = 1)
  
  # Se a linha tem tamanho zero entao para o processamento
  if ( length(line) == 0 ) {
    break
  }
  
  # verifica e o comeco do processamento
  if(stringr::str_detect(line, stringr::regex("-* Ox at .* on .* -*"))){
    # writeLines( text = line, con = WriteCon)
    print(line)
  }
  
  # verifica a regiao que foi skiped
  #   if(stringr::str_detect(line, stringr::regex(pattern2))){
  #     # writeLines( text = line, con = WriteCon)
  #     # writeLines( text = "No data", con = WriteCon)
  #     # writeLines( text = "No data", con = WriteCon)
  #     # writeLines( text = "No data", con = WriteCon)
  
  #     region_number <- stringr::str_match(line,  stringr::regex("(?<=SKIP: Regiao )\\d*"))
  #     WriteInfo <- TRUE
  
  #     print(line)
  #     print("No data")
  #     print("No data")
  #   }
  

  # verifica a regiao atual
  if(stringr::str_detect(line, stringr::regex("             Regiao "))){
    # writeLines( text = line, con = WriteCon)
    
    region_number <- stringr::str_match(line,  stringr::regex("(?<=             Regiao )\\d*"))
    WriteInfo <- TRUE
    print(line)
  }
  
  

  # Verifica a restrição em alpha e beta
  if(stringr::str_detect(line, stringr::regex("Test of restrictions on alpha( and beta)?:"))){
    # writeLines( text = line, con = WriteCon)
    ExoTest <- stringr::str_match(line,  stringr::regex("(?<=\\[).*(?=\\])"))
    WriteInfo <- TRUE
    # print(line)
    
    # Detecta se o teste passou ou nao.
    if(stringr::str_detect(line, stringr::regex("Test of restrictions on alpha:\\s*.*\\[.*\\]\\*{2}"))) {
      ncount_2S = ncount_2S + 1
    }
    ncount = ncount + 1
  }
  
  # Verifica a restrição em alpha (2020-11-18: A principio tera dois testes por regiao)
  if(stringr::str_detect(line, stringr::regex("Bootstrap test\\s+:"))){
    # writeLines(text = line, con = WriteCon)
    ExoTest_Boot <- stringr::str_match(line,  stringr::regex("(?<=\\[).*(?=\\])"))
    WriteInfo <- TRUE
    # print(line)
    
  }
  
  
  if(WriteInfo) {
    selctVector <- tbl.results$region == as.integer(region_number)
    
    if(!is.na(rank)){
      tbl.results$rank[selctVector] <- as.integer(rank)
    }
    
    if(!is.na(ExoTest)){
      tbl.results$ExoTest[selctVector] <- as.numeric(ExoTest)  
    }
    
    if(!is.na(ExoTest_Boot)){
      tbl.results$ExoTest_Boot[selctVector] <- as.numeric(ExoTest_Boot)  
      # stop()
    }
    
  }
  
}

close(ReadCon)
close(WriteCon)

cat(sprintf("Total de 1S: %d\nTotal de 2S: %d", ncount - ncount_2S, ncount_2S))

tbl.results %>% filter(ExoTest_Boot < 0.01)

View(tbl.results)

print("Exogeniedade fraca")
tbl.results %>%
  count(Overall) %>% 
  print()

print("Autocorrelacao")
tbl.results %>% 
  mutate(Max_auto_TF = Max_auto > 0.01) %>% 
  count(Max_auto_TF) %>% 
  print()

print("Rank")
tbl.results %>%
  count(Max_rank) %>%
  print()


# readr::write_excel_csv(x=tbl.results,
#                        file = sprintf("%s on %s.txt", stringr::str_match(fileName,  stringr::regex(".*(?=\\.\\w{3})")), Sys.Date()))


SaveMatrixForOx <- function(Matrix, file_name, comment = NULL){
  if(is.null(comment)){comment = file_name}
  
  file.name <- sprintf("./export/%s", file_name)
  
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

rankOfRegions <- tbl.results %>% select(rank_boot)
SaveMatrixForOx(Matrix = rankOfRegions, file_name = "rankOfRegions.mat", comment = "(Indication of the rank of the regions)")

