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
Welzijn_Goed <- read_csv("Welzijn.goed.csv")

library(readr)
Ervaren_Gezondheid <- read_csv("Ervaren_Gezondheid.csv")

library(readr)
Levensverwachting <- read_csv("Levensverwachting.csv")

#References
##write.csv(Levensverwachting, "Levensverwachting.csv")

##Levensverwachting <- read_csv("Levensverwachting.csv")

##write.csv(`Ervaren_Gezondheid`, "Ervaren_Gezondheid.csv")

##write.csv(Welzijn_Goed, "Welzijn.goed.csv")



Welzijn_Goed = read.csv("Welzijn.goed.csv") 
Levensverwachting = read.csv("Levensverwachting.csv")
Ervaren_Gezondheid <- read_csv("Ervaren_Gezondheid.csv")

###########################
##### DATA PROCESSING #####
###########################

library(dplyr)

#Isolate the rows that contain 2015G400 rows to get the data of the years 2015-2018 in Dataset Levensverwachting
Levensverwachting <- Levensverwachting[grepl("2015G400", Levensverwachting$Perioden),]

#Isolate the rows that contain 2015JJ00, 2016JJ00, 2017JJ00, 2018JJ00 to get the data of the years 2015-2018 in Dataset Welzijn 
Welzijn_Goed <- Welzijn_Goed[Welzijn_Goed$Perioden %in% c("2015JJ00", "2016JJ00", "2017JJ00", "2018JJ00"), ]

#Isolate
Welzijn_averages <- Welzijn_Goed %>%
  group_by(Kenmerken, Marges) %>%
  summarise(
    Perioden = "2015–2018",
    across(where(is.numeric), mean, na.rm = TRUE),
    .groups = "drop"
  ) 


#Check if the columns in Welzijn and Welzijn_averages are the same

setdiff(names(Welzijn_Goed), names(Welzijn_averages))

sapply(Welzijn_Goed[setdiff(names(Welzijn_Goed), names(Welzijn_averages))], class)

#Change the columns in Welzijn_Goed that are not numeric to numeric

Welzijn_Goed <- Welzijn_Goed %>%
  mutate(across(all_of(c(
    "ScoreTevredenheidMetWerk_13", "Ontevreden_14", "NietTevredenNietOntevreden_15",
    "Tevreden_16", "ScoreTevredenheidMetReistijd_17", "Ontevreden_18",
    "NietTevredenNietOntevreden_19", "Tevreden_20", "ScoreTevredenheidDagelijkseBezigheden_21",
    "Ontevreden_22", "NietTevredenNietOntevreden_23", "Tevreden_24", "Ontevreden_58"
  )), ~ as.numeric(as.character(.))))

#Rerun the averaging process after converting the columns to numeric

Welzijn_averages <- Welzijn_Goed %>%
  group_by(Kenmerken, Marges) %>%
  summarise(
    Perioden = "2015–2018",
    across(where(is.numeric), mean, na.rm = TRUE),
    .groups = "drop"
  ) 

#Check if the columns in Welzijn and Welzijn_averages are the same again
setdiff(names(Welzijn_Goed), names(Welzijn_averages))

Ervaren_Gezondheid <- read_csv("Ervaren_Gezondheid.csv", col_names = TRUE,)

#Isolate the rows with the averages of gemeenten 'Amsterdam' & 'Rotterdam'
Ervaren_Gezondheid <- Ervaren_Gezondheid[
  Ervaren_Gezondheid$Gemeentenaam_1 %in% c("Amsterdam", "Rotterdam") &
    Ervaren_Gezondheid$SoortRegio_2 == "Gemeente",]

#create another dataset copy of one of the three original datasets.
#Here we will merge everything together based on the year 2016
#Note that we will use the 2015-2018 averages of the welzijn dataset together with the "2015G400" (2015/2018) of the Levensverwachting dataset as a substitute for the year 2016
#This way we can compare all data based on the same year(/variable).

#Filter Ervaren_Gezondheid for the year 2016
Ervaren_Gezondheid_2016 <- Ervaren_Gezondheid[Ervaren_Gezondheid$Perioden == "2016JJ00", ]

#Isolate only the relevant absolute value of the year 2016 from the Levensverwachting dataset
Levensverwachting_2016 <- Levensverwachting %>%
  filter(Marges == "MW00000", Perioden == "2015G400") %>%
  mutate(Perioden = "2016")
#Isolate only the relevant absolute value of the year 2016 from the Welzijn_averages dataset
Welzijn_averages_2016 <- Welzijn_averages %>%
  filter(Marges == "MW00000", Perioden == "2015–2018") %>%
  mutate(Perioden = "2016")

#full_join(
# Zorg dat de kolomnamen gelijk zijn
Levensverwachting_2016_renamed <- Levensverwachting_2016 %>%
  rename(Kenmerken = InkomenEnWelvaart)  # pas aan als dit de koppelsleutel is

# Voer de merge uit op 'Kenmerken' en 'Perioden'
merged_data <- full_join(
  Levensverwachting_2016_renamed,
  Welzijn_averages_2016,
  by = c("Kenmerken", "Perioden")
)

# Transform everything into numerics
Levensverwachting_2016_renamed <- Levensverwachting_2016_renamed %>%
  mutate(Kenmerken = as.character(Kenmerken))

Welzijn_averages_2016 <- Welzijn_averages_2016 %>%
  mutate(Kenmerken = as.character(Kenmerken))

#join them together into merged_data
merged_data <- full_join(
  Levensverwachting_2016_renamed,
  Welzijn_averages_2016,
  by = c("Kenmerken", "Perioden")
)




#dit gekke ding gedaan om scale te kunnen veranderen.
Ervaren_Gezondheid$ErvarenGezondheidGoedZeerGoed_4 <-
  as.numeric(as.character(Ervaren_Gezondheid$ErvarenGezondheidGoedZeerGoed_4))

#Making a graph showing development in "ervaren gezondheid" in de gemeenten
#Amsterdam en Rotterdam over de jaren: 2012, 2016, 2020
ggplot(Ervaren_Gezondheid, 
       aes(x = Perioden, y = ErvarenGezondheidGoedZeerGoed_4,
           colour = Gemeentenaam_1, group = Gemeentenaam_1)) +
  geom_line(size = 1.5) +
  geom_text(aes(label = ErvarenGezondheidGoedZeerGoed_4),
            colour = "black", 
            vjust = -1,
            show.legend = F) +
  geom_point(colour = "black", size = 2.5, show.legend = F) +
  
  scale_colour_manual(values = c("#C41230", "#39B54A", "black")) +
  scale_x_discrete(label = c(2012, 2016, 2020)) +
  scale_y_continuous(
    limits = c(65, 80),
    breaks = seq(0, 100, by = 5)) +
  
  labs(x = "Jaar (2012, 2016, 2020)", 
       y = "Ervaren gezondheid (% Goed of Zeer Goed)",
       title = "Ervaren Gezondheid per Gemeente (2012-2020)",
       colour = "Gemeente") +
  theme_minimal() +
  theme(legend.position = "bottom")
#voeg lijn toe met gemiddelde van Nederland  