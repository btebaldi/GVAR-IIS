
# Setup -------------------------------------------------------------------
rm(list=ls())
# You should take care with readLines(...) and big files. Reading all lines at
# memory can be risky. Below is a example of how to read file and process just
# one line at time:


# Bibliotecas -------------------------------------------------------------

library(dplyr)
library(readxl)
library(ggplot2)

getwd()
# Variaveis internas ------------------------------------------------------


tbl <- readRDS(file = "./database/db_Ox_sem_buraco.rds")


brent <- read_excel("database/brent.xlsx", 
                    range = "A7:C4571", col_types = c("date", "numeric", "numeric"),
                    na = "#N/A N/A")

brent$Date <- as.Date(brent$Date)

WTI <- read_excel("database/WTI.xlsx", 
                  range = "A7:C4571", col_types = c("date", "numeric", "numeric"),
                  na = "#N/A N/A")

WTI$Date <- as.Date(WTI$Date)

Oil <- tbl

i=1
for (i in seq_len(nrow(Oil)) ) {
  dt_ini <- Oil$DATA_INICIAL[i]
  dt_fim <- Oil$DATA_FINAL[i]
  
  tb_brent <- brent %>% filter(Date >= dt_ini & Date <= dt_fim)
  tb_wti <- WTI %>% filter(Date >= dt_ini & Date <= dt_fim)
  
  Oil$brent[i] <-  mean(tb_brent$PX_LAST, na.rm = TRUE)
  Oil$WTI[i] <-  mean(tb_wti$PX_LAST, na.rm = TRUE)
}


ggplot(Oil) +
  geom_line(aes(x=DATA_INICIAL, y=brent, colour="brent")) +
  geom_line(aes(x=DATA_INICIAL, y=WTI, colour = "WTI"))


saveRDS(object = Oil, file = "./database/db_oil.rds")
readr::write_csv(x = Oil, file = "./export/database for ox/db_Oil.csv")







