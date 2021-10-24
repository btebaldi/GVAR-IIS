
# Setup -------------------------------------------------------------------
rm(list = ls())

library(stringi)


# Example -----------------------------------------------------------------

base <- data.frame(terme = c("Millésime",
                             "boulangère",
                             "üéâäàåçêëèïîì",
                             "ãõ"))

base$ASCII <- stri_trans_general(str = base$terme, id = "Latin-ASCII")

head(base, 10)
