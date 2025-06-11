###########################
###########################
#####| |\   | | ----- #####
#####| | \  | |   |   #####
#####| |  \ | |   |   #####
#####| |   \| |   |   #####
###########################
###########################

setwd("~/GitHub/Programmeren/data")

library(readr)
Welzijn <- read_delim("C:/Users/Luke Eising/Downloads/85542NED_UntypedDataSet_03062025_093918.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

library(readr)
Ervaren_Gezondheid <- read_csv("Ervaren_Gezondheid.csv")

library(readr)
Levensverwachting <- read_delim("C:/Users/Luke Eising/Downloads/85445NED_UntypedDataSet_03062025_092005.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

#References
write.csv(Levensverwachting, "Levensverwachting.csv")

Levensverwachting <- read_csv("Levensverwachting.csv")

write.csv(`Ervaren Gezondheid`, "Ervaren_Gezondheid.csv")

#Load Data
library(readr)
Welzijn <- read_delim("C:/Users/Luke Eising/Downloads/85542NED_UntypedDataSet_03062025_093918.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

library(readr)
Ervaren_Gezondheid <- read_csv("Ervaren_Gezondheid.csv")

library(readr)
Levensverwachting <- read_delim("C:/Users/Luke Eising/Downloads/85445NED_UntypedDataSet_03062025_092005.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

Welzijn = read.csv("Welzijn.csv") 
Levensverwachting = read.csv("Levensverwachting.csv")
Ervaren_Gezondheid <- read_csv("Ervaren_Gezondheid.csv")

###########################
##### DATA PROCESSING #####
###########################

library(dplyr)

#Isolate the rows that contain 2015G400 rows to get the data of the years 2015-2018 in Dataset Levensverwachting
Levensverwachting <- Levensverwachting[grepl("2015G400", Levensverwachting$Perioden),]

#Isolate the rows that contain 2015JJ00, 2016JJ00, 2017JJ00, 2018JJ00 to get the data of the years 2015-2018 in Dataset Welzijn 
Welzijn <- Welzijn[Welzijn$Perioden %in% c("2015JJ00", "2016JJ00", "2017JJ00", "2018JJ00"), ]

#Isolate
Welzijn_averages <- Welzijn %>%
  group_by(Kenmerken, Marges) %>%
  summarise(
    Perioden = "2015–2018",
    across(where(is.numeric), mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(ID = row_number()) %>%
  relocate(ID, .before = Kenmerken)


#Check if the columns in Welzijn and Welzijn_averages are the same

setdiff(names(Welzijn), names(Welzijn_averages))

sapply(Welzijn[setdiff(names(Welzijn), names(Welzijn_averages))], class)

#Change the columns in Welzijn that are not numeric to numeric

Welzijn <- Welzijn %>%
  mutate(across(all_of(c(
    "ScoreTevredenheidMetWerk_13", "Ontevreden_14", "NietTevredenNietOntevreden_15",
    "Tevreden_16", "ScoreTevredenheidMetReistijd_17", "Ontevreden_18",
    "NietTevredenNietOntevreden_19", "Tevreden_20", "ScoreTevredenheidDagelijkseBezigheden_21",
    "Ontevreden_22", "NietTevredenNietOntevreden_23", "Tevreden_24", "Ontevreden_58"
  )), ~ as.numeric(as.character(.))))

#Rerun the averaging process after converting the columns to numeric

Welzijn_averages <- Welzijn %>%
  group_by(Kenmerken, Marges) %>%
  summarise(
    Perioden = "2015–2018",
    across(where(is.numeric), mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(ID = row_number()) %>%
  relocate(ID, .before = Kenmerken)

#Check if the columns in Welzijn and Welzijn_averages are the same again
setdiff(names(Welzijn), names(Welzijn_averages))

Ervaren_Gezondheid <- read_csv("Ervaren_Gezondheid.csv", col_names = TRUE, skip = 1)

#Isolate the rows with the averages of gemeenten 'Amsterdam' & 'Rotterdam'
Ervaren_Gezondheid <- Ervaren_Gezondheid[
  Ervaren_Gezondheid$Gemeentenaam_1 %in% c("Amsterdam", "Rotterdam") &
    Ervaren_Gezondheid$SoortRegio_2 == "Gemeente",]
