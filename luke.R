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

# ...existing code...

# Zorg dat de kolomnamen gelijk zijn
Levensverwachting_2016_renamed <- Levensverwachting_2016 %>%
  rename(Kenmerken = InkomenEnWelvaart)  # pas aan als dit de koppelsleutel is

# Zet 'Kenmerken' in beide dataframes om naar character
Levensverwachting_2016_renamed <- Levensverwachting_2016_renamed %>%
  mutate(Kenmerken = as.character(Kenmerken))

Welzijn_averages_2016 <- Welzijn_averages_2016 %>%
  mutate(Kenmerken = as.character(Kenmerken))

# Voer de merge uit op 'Kenmerken' en 'Perioden'
merged_data <- full_join(
  Levensverwachting_2016_renamed,
  Welzijn_averages_2016,
  by = c("Kenmerken", "Perioden")
)

# ...existing code...



#dit gekke ding gedaan om scale te kunnen veranderen.
Ervaren_Gezondheid$ErvarenGezondheidGoedZeerGoed_4 <-
  as.numeric(as.character(Ervaren_Gezondheid$ErvarenGezondheidGoedZeerGoed_4))

#Making a graph showing development in "ervaren gezondheid" in de gemeenten
#Amsterdam en Rotterdam over de jaren: 2012, 2016, 2020

#check if you got these packages so it runs properly 
library(tidyverse)
library(cbsodataR)
library(sf)
#otherwise:
#install.packages("tidyverse")
#install.packages("cbsodataR")
#install.packages("sf")

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



























# Create a new dataset for the year 2016, with only a couple of values kept
Welzijn_index_2016 <- Welzijn_averages_2016 %>%
  select(
    Kenmerken, Marges, Perioden,
    ScoreGeluk_1,
    ScoreTevredenheidMetHetLeven_5,
    ScoreTevredenheidMetWerk_13,
    ScoreTevredenheidDagelijkseBezigheden_21,
    ScoreTevredenheidMetLichGezondheid_25,
    ScoreTevredenheidPsychischeGezondheid_29,
    ScoreTevredenheidMetSociaalLeven_57
  )

# With these values we can create a new column called WelzijnIndex 
Welzijn_index_2016 <- Welzijn_index_2016 %>%
  mutate(
    WelzijnIndex = rowMeans(select(., 
                                   ScoreGeluk_1,
                                   ScoreTevredenheidMetHetLeven_5,
                                   ScoreTevredenheidMetWerk_13,
                                   ScoreTevredenheidDagelijkseBezigheden_21,
                                   ScoreTevredenheidMetLichGezondheid_25,
                                   ScoreTevredenheidPsychischeGezondheid_29,
                                   ScoreTevredenheidMetSociaalLeven_57
    ), na.rm = TRUE)
  )

# Move New column WelzijnIndex to the front and delete the old columns
Welzijn_index_2016 <- Welzijn_index_2016 %>%
  select(Kenmerken, Marges, Perioden, WelzijnIndex)

# Filter the dataset to only include rows with specific educational levels
Welzijn_naar_opleidingsniveau <- Welzijn_index_2016 %>%
  filter(Kenmerken %in% c(
    "2018710",  # Basisonderwijs
    "2018720",  # Vmbo, havo-, vwo-onderbouw, mbo1
    "2018750",  # Havo, vwo, mbo2-4
    "2018800",  # Hbo-, wo-bachelor
    "2018810"   # Hbo-, wo-master, doctor
  ))

# Rename the 'Kenmerken' column to a new 'Opleidingsniveau' column for clarity
Welzijn_naar_opleidingsniveau <- Welzijn_naar_opleidingsniveau %>%
  mutate(
    Opleidingsniveau = case_when(
      Kenmerken == "2018710" ~ "Basisonderwijs",
      Kenmerken == "2018720" ~ "Vmbo, havo-, vwo-onderbouw, mbo1",
      Kenmerken == "2018750" ~ "Havo, vwo, mbo2-4",
      Kenmerken == "2018800" ~ "Hbo-, wo-bachelor",
      Kenmerken == "2018810" ~ "Hbo-, wo-master, doctor",
      TRUE ~ NA_character_
    )
  )

# Remove the old 'Kenmerken' column and rearrange the columns
Welzijn_naar_opleidingsniveau <- Welzijn_naar_opleidingsniveau %>%
  select(-Kenmerken) %>%
  relocate(Opleidingsniveau, .before = everything())

# Filter the Levensverwachting dataset for the relevant characteristics (3000 and 4000)
Levensverwachting_geslacht <- Levensverwachting_2016_renamed %>%
  filter(Geslacht %in% c(3000, 4000))

# Rename 3000 and 4000 to "Mannen" & "Vrouwen" respectively
Levensverwachting_geslacht <- Levensverwachting_geslacht %>%
  mutate(
    Geslacht = case_when(
      Geslacht == 3000 ~ "Mannen",
      Geslacht == 4000 ~ "Vrouwen"
    ))

# Take the average life expectancy of Men & Women
Levensverwachting_geslacht_gemiddeld <- Levensverwachting_geslacht %>%
  group_by(Geslacht) %>%
  summarise(
    Jaar = "2016",
    Levensverwachting = mean(Levensverwachting_1, na.rm = TRUE)
  )
# Note: The value for life expectancy may seem 'low' because it reflects the
# number of years a person in this age group is still expected to live.
# For example, someone in the 80-year age group has have a life expectancy of around 8 years.

# Create a new dataset for to showcase the WelzijnIndex for both 'Men' & 'Women'
# Give it the same form as Levensverwachting_geslacht_gemiddeld
Welzijn_naar_geslacht <- Welzijn_index_2016 %>%
  filter(Kenmerken %in% c(3000, 4000)) %>%
  mutate(
    Geslacht = case_when(
      Kenmerken == 3000 ~ "Mannen",
      Kenmerken == 4000 ~ "Vrouwen"
    )
  ) %>%
  select(Geslacht, Jaar = Perioden, WelzijnIndex)

# Merge the two datasets into one 
Ultimate_dataset_of_doom_hell_and_destruction <- full_join(
  Levensverwachting_geslacht_gemiddeld,
  Welzijn_naar_geslacht,
  by = c("Geslacht", "Jaar")
)