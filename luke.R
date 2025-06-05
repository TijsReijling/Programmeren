#####| |\   | | ----- #####
#####| | \  | |   |   #####
#####| |  \ | |   |   #####
#####| |   \| |   |   #####

library(readr)
Welzijn <- read_csv2("C:/Users/Luke Eising/Downloads/85542NED_UntypedDataSet_03062025_093918.csv")

Welzijn$ScoreGeluk_1
write.csv(Welzijn, "data/Welzijn.csv")
