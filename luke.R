#####| |\   | | ----- #####
#####| | \  | |   |   #####
#####| |  \ | |   |   #####
#####| |   \| |   |   #####

library(readr)
Welzijn <- read_csv2("C:/Users/Luke Eising/Downloads/85542NED_UntypedDataSet_03062025_093918.csv")

Welzijn$ScoreGeluk_1
write.csv(Welzijn, "data/Welzijn.csv")

Welzijn <- read_csv("data/Welzijn.csv")

library(readr)
Levensverwachting <- read_delim("C:/Users/Luke Eising/Downloads/85445NED_UntypedDataSet_03062025_092005.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

write.csv(Levensverwachting, "data/Levensverwachting.csv")

Levensverwachting <- read_csv("data/Levensverwachting.csv")