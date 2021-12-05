
# Setup -------------------------------------------------------------------
rm(list=ls())
# You should take care with readLines(...) and big files. Reading all lines at
# memory can be risky. Below is a example of how to read file and process just
# one line at time:


# Bibliotecas -------------------------------------------------------------

# library(stringr)
# library(dplyr)
library(lubridate)

# # Variaveis internas ------------------------------------------------------

tbl <- readRDS(file = "./database/db_oil.rds")


# for(i in 1:12){
#   tbl[, sprintf("M%d", i)] <- 0
# }

tbl$date_2 <- NA
i=1
for (i in seq_len(nrow(tbl)) ) {
  dt_ini <- tbl$DATA_INICIAL[i]
  dt_fim <- tbl$DATA_FINAL[i]
  
  tbl$date_2[i] <-  sprintf("%d.%d", year(dt_ini),  isoweek(dt_ini))
}

tbl <- tbl %>% mutate_if(.predicate = is.numeric, .funs = log)

tbl <- tbl %>% filter(year(DATA_INICIAL) < 2019)

saveRDS(object = tbl, file = "./database/db_oil_withDummies.rds")
readr::write_csv(x = tbl, file = "./export/database for ox/db_oil_withDummies.csv")

