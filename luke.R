###########################
###########################
#####| |\   | | ----- #####
#####| | \  | |   |   #####
#####| |  \ | |   |   #####
#####| |   \| |   |   #####
###########################
###########################

#################################
##### Set working directory #####
#################################

setwd("~/GitHub/Programmeren/data")

####################################
##### Run if packages required #####
####################################

#install.packages("dplyr")
#install.packages("pkg")
#install.packages("tidyverse")
#install.packages("cbsodataR")
#install.packages("sf")
#install.packages("readr")
#install.packages("ggplot2")

################################
##### library all packages #####
################################

library(cbsodataR)
library(sf)
library(readr)
library(ggplot2)
library(tidyverse)
#library(pkg)
library(dplyr)

##############################################
##### Load all data into the Environment #####
##############################################

Welzijn_Goed <- read_csv("Welzijn.goed.csv")

Ervaren_Gezondheid <- read_csv("Ervaren_Gezondheid.csv")

Levensverwachting <- read_csv("Levensverwachting.csv")

Ervaren_gezondheid_wijk <- read_delim("Ervarengezondheid_Wijk&Buurt.csv", delim = ";")

Inkomen_per_gemeente <- read_delim("Inkomen_gemeente.csv", delim = ";")

Levensverwachting_Gemeente <- read_delim("Levensverwacht_Gemeente_Wijk&Buurt.csv", delim = ";")

ErvarenGezondheidNL <- read_csv("ErvarenGezondheidNL.csv")

gemeentegrenzen <- st_read("https://service.pdok.nl/cbs/gebiedsindelingen/2023/wfs/v1_0?request=GetFeature&service=WFS&version=2.0.0&typeName=gemeente_gegeneraliseerd&outputFormat=json")


###########################
##### DATA PROCESSING #####
###########################

#Isolate the rows that contain 2015G400 rows to get the data of the years 2015-2018 in Dataset Levensverwachting
Levensverwachting <- Levensverwachting[grepl("2015G400", Levensverwachting$Perioden),]

#Isolate the rows that contain 2015JJ00, 2016JJ00, 2017JJ00, 2018JJ00 to get the data of the years 2015-2018 in Dataset Welzijn 
Welzijn_Goed <- Welzijn_Goed[Welzijn_Goed$Perioden %in% c("2015JJ00", "2016JJ00", "2017JJ00", "2018JJ00"), ]

#Create a new variable, averages of all welzijn questionnaire scores
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

#Make sure the column names are consistent for merging
Levensverwachting_2016_renamed <- Levensverwachting_2016 %>%
  rename(Kenmerken = InkomenEnWelvaart) 

#Transform 'kenmerken' in both datasets to character type
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


#Change the scale in Ervaren_GEzondheid
Ervaren_Gezondheid$ErvarenGezondheidGoedZeerGoed_4 <-
  as.numeric(as.character(Ervaren_Gezondheid$ErvarenGezondheidGoedZeerGoed_4))

#Making a graph showing development in "ervaren gezondheid" in the municipalities
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
  
  labs(x = "Year (2012, 2016, 2020)", 
       y = "Perceived Health (% Good or Very Good)",
       title = "Perceived Health per Municipality (2012-2020)",
       colour = "Municipality") +
  theme_minimal() +
  theme(legend.position = "bottom")

#Add a line with the average of the Netherlands 


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

#Move New column WelzijnIndex to the front and delete the old columns
Welzijn_index_2016 <- Welzijn_index_2016 %>%
  select(Kenmerken, Marges, Perioden, WelzijnIndex)

#Filter the dataset to only include rows with specific educational levels
Welzijn_naar_opleidingsniveau <- Welzijn_index_2016 %>%
  filter(Kenmerken %in% c(
    "2018710",  # Basisonderwijs
    "2018720",  # Vmbo, havo-, vwo-onderbouw, mbo1
    "2018750",  # Havo, vwo, mbo2-4
    "2018800",  # Hbo-, wo-bachelor
    "2018810"   # Hbo-, wo-master, doctor
  ))

#Rename the 'Kenmerken' column to a new 'Opleidingsniveau' column for clarity
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

#Remove the old 'Kenmerken' column and rearrange the columns
Welzijn_naar_opleidingsniveau <- Welzijn_naar_opleidingsniveau %>%
  select(-Kenmerken) %>%
  relocate(Opleidingsniveau, .before = everything())

#Filter the Levensverwachting dataset for the relevant characteristics (3000 and 4000)
Levensverwachting_geslacht <- Levensverwachting_2016_renamed %>%
  filter(Geslacht %in% c(3000, 4000))

#Rename 3000 and 4000 to "Mannen" & "Vrouwen" respectively
Levensverwachting_geslacht <- Levensverwachting_geslacht %>%
  mutate(
    Geslacht = case_when(
      Geslacht == 3000 ~ "Mannen",
      Geslacht == 4000 ~ "Vrouwen"
    ))

#Take the average life expectancy of Men & Women
Levensverwachting_geslacht_gemiddeld <- Levensverwachting_geslacht %>%
  group_by(Geslacht) %>%
  summarise(
    Jaar = "2016",
    Levensverwachting = mean(Levensverwachting_1, na.rm = TRUE)
  )

#Note: The value for life expectancy may seem 'low' because it reflects the
#number of years a person in this age group is still expected to live.
#For example, someone in the 80-year age group has have a life expectancy of around 8 years.

#Create a new dataset for to showcase the WelzijnIndex for both 'Men' & 'Women'
#Give it the same form as Levensverwachting_geslacht_gemiddeld
Welzijn_naar_geslacht <- Welzijn_index_2016 %>%
  filter(Kenmerken %in% c(3000, 4000)) %>%
  mutate(
    Geslacht = case_when(
      Kenmerken == 3000 ~ "Mannen",
      Kenmerken == 4000 ~ "Vrouwen"
    )
  ) %>%
  select(Geslacht, Jaar = Perioden, WelzijnIndex)

#Merge the two datasets into one 
Ultimate_dataset_of_doom_hell_and_destruction <- full_join(
  Levensverwachting_geslacht_gemiddeld,
  Welzijn_naar_geslacht,
  by = c("Geslacht", "Jaar")
)

#Take the marges out of Welzijn_naar_opleidingsniveau and change the perioden to the year
Welzijn_naar_opleidingsniveau <- Welzijn_naar_opleidingsniveau %>%
  filter(Marges == "MW00000") %>%
  select(-Marges) %>%
  rename(Jaar = Perioden)

#Install required packages automatically
packages <- c("sf", "dplyr", "ggplot2", "readr", "tmap", "stringr")
installed <- rownames(installed.packages())
for (pkg in packages) {
  if (!pkg %in% installed) install.packages(pkg)
}
lapply(packages, library, character.only = TRUE)

#Load the municipality borders map
gemeente_shape <- st_read(
  "https://service.pdok.nl/cbs/gebiedsindelingen/2020/wfs/v1_0?request=GetFeature&service=WFS&version=2.0.0&typeName=gemeente_gegeneraliseerd&outputFormat=json",
  quiet = TRUE
)

#Isolate only relevant columns and rename them
gemeente_shape <- gemeente_shape %>%
  select(statcode, statnaam, geometry) %>%
  rename(GM_CODE = statcode, GM_NAAM = statnaam)

#Load new GemeenteJuist data
data <- read_csv("GemeentesJuist.csv", locale = locale(encoding = "UTF-8"))

#Isolate only municipalities
gemeente_data <- data %>%
  filter(str_to_lower(SoortRegio_2) == "gemeente") %>%
  select(Gemeentenaam_1, ErvarenGezondheidGoedZeerGoed_4)

#Normalize names for the join
gemeente_shape$GM_NAAM <- str_to_lower(gemeente_shape$GM_NAAM)
gemeente_data$Gemeentenaam_1 <- str_to_lower(gemeente_data$Gemeentenaam_1)

# Join shapefile with health data
kaart_data <- gemeente_shape %>%
  left_join(gemeente_data, by = c("GM_NAAM" = "Gemeentenaam_1"))

#Check for unmatched municipalities
na_count <- sum(is.na(kaart_data$ErvarenGezondheidGoedZeerGoed_4))
cat("Aantal gemeenten zonder data: ", na_count, "\n")

#Simplify geometry for faster plotting (optional)
kaart_data <- st_simplify(kaart_data, dTolerance = 100)

#Set tmap to plot mode
tmap_mode("plot")  # "view" = interactive, "plot" = static

#Create the heatmap
kaart_plot <- tm_shape(kaart_data) +
  tm_fill("ErvarenGezondheidGoedZeerGoed_4",
          palette = "Blues",
          title = "Perceived Health per Municipality (%)",
          textNA = "No data") +
  tm_borders() +
  tm_layout(
    title = "Perceived Health per Municipality in 2020",
    title.position = c("center", "top"),
    inner.margins = c(0.12, 0.02, 0.10, 0.02),
    title.size = 1.5,
    title.color = "black",
    legend.outside = TRUE
  )

#Print the map
print(kaart_plot)

# Clean Levensverwachting_2016_renamed dataset a bit more in a specific direction for another plot
# Isolate baby's born in 2016, with age 0
Levensverwachting_2016_renamed_baby <- Levensverwachting_2016_renamed %>%
  filter(LeeftijdOp31December == "10010", Geslacht == "T001038") %>%
  mutate(Leeftijd = 0)

#Clean up some non-relevant columns
Levensverwachting_2016_renamed_baby <- Levensverwachting_2016_renamed_baby %>%
  select(-Marges, -Geslacht, -LeeftijdOp31December)

#Change Perioden to 'Jaar'
Levensverwachting_2016_renamed_baby <- Levensverwachting_2016_renamed_baby %>%
  rename(Jaar = Perioden)

#Filter only the 'kenmerken' that indicate the Welfare quintiles
welvaart_codes <- c(2021770, 2021780, 2021790, 2021800, 2021810)

Levensverwachting_2016_renamed_baby <- Levensverwachting_2016_renamed_baby %>%
  filter(Kenmerken %in% welvaart_codes)

#Create a new column for Welfare Quintiles with their respective labels
Levensverwachting_2016_renamed_baby <- Levensverwachting_2016_renamed_baby %>%
  arrange(Kenmerken) %>%
  mutate(WelvaartQuintiles = c("1st quintile", "2nd quintile", "3rd quintile", "4th quintile", "5th quintile"))

#Put the columns back in the order that is preferred
Levensverwachting_2016_renamed_baby <- Levensverwachting_2016_renamed_baby %>%
  select(Leeftijd, Jaar, WelvaartQuintiles, Levensverwachting_1)

#Convert the Levensverwachting_1 column to numeric (Otherwise there will be an error)
Levensverwachting_2016_renamed_baby$Levensverwachting_1 <- as.numeric(as.character(Levensverwachting_2016_renamed_baby$Levensverwachting_1))
str(Levensverwachting_2016_renamed_baby)


#Plot: Life expectancy at birth per Total Wealth Quintile (2016)
ggplot(Levensverwachting_2016_renamed_baby, aes(x = WelvaartQuintiles, y = Levensverwachting_1)) +
  geom_col(fill = "steelblue") +
  coord_flip(ylim = c(60, NA)) +  # <<--- set limit here
  labs(
    title = "Life expectancy at birth per Total Wealth Quintile (2016)",
    x = "Total Wealth Quintile",
    y = "Life expectancy (in years)"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10))
  )

#Plot: Education level vs. Welfare Index
#Ensure the education level order is correct (most basic to most advanced)
Welzijn_naar_opleidingsniveau$Opleidingsniveau <- factor(
  Welzijn_naar_opleidingsniveau$Opleidingsniveau,
  levels = c(
    "Basisonderwijs",
    "Vmbo, havo-, vwo-onderbouw, mbo1",
    "Havo, vwo, mbo2-4",
    "Hbo-, wo-bachelor",
    "Hbo-, wo-master, doctor"
  )
)

ggplot(Welzijn_naar_opleidingsniveau, aes(x = Opleidingsniveau, y = WelzijnIndex, fill = Opleidingsniveau)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(
    aes(label = round(WelzijnIndex, 3)),   # 3 decimals
    vjust = -0.4,
    size = 4
  ) +
  scale_y_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, 1)
  ) +
  labs(
    title = "Welfare Index per Level of education (The Netherlands, 2016)",
    x = "Education level",
    y = "Welfare Index (scale of 1-10)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 7),
    legend.position = "none"
  )


#specify the right 'kenmerken' (Level of education groups)
opleidingskenmerken <- c(2018710, 2018720, 2018750, 2018800, 2018810)

#Create new dataset
Welzijn_temporal_visualization <- Welzijn_Goed %>%
  #Filter on the right 'Kenmerken' and 'Marges'
  filter(Kenmerken %in% opleidingskenmerken, Marges == "MW00000") %>%
  
  #Add column year (i.e. "2016JJ00" → 2016)
  mutate(Jaar = as.numeric(substr(Perioden, 1, 4))) %>%
  
  #Make 'WelzijnIndex' the middle row
  rowwise() %>%
  mutate(WelzijnIndex = mean(c_across(c(
    ScoreGeluk_1,
    ScoreTevredenheidMetHetLeven_5,
    ScoreTevredenheidMetWerk_13,
    ScoreTevredenheidDagelijkseBezigheden_21,
    ScoreTevredenheidMetLichGezondheid_25,
    ScoreTevredenheidPsychischeGezondheid_29,
    ScoreTevredenheidMetSociaalLeven_57
  )), na.rm = TRUE)) %>%
  ungroup() %>%
  
  #Select only the relevant rows
  select(Kenmerken, Jaar, WelzijnIndex)

Welzijn_temporal_visualization <- Welzijn_temporal_visualization %>%
  mutate(
    Opleidingsniveau = case_when(
      Kenmerken == 2018710 ~ "Basisonderwijs",
      Kenmerken == 2018720 ~ "Vmbo, havo-, vwo-onderbouw, mbo1",
      Kenmerken == 2018750 ~ "Havo, vwo, mbo2-4",
      Kenmerken == 2018800 ~ "Hbo-, wo-bachelor",
      Kenmerken == 2018810 ~ "Hbo-, wo-master, doctor",
      TRUE ~ NA_character_
    )
  ) %>%
  select(Opleidingsniveau, Jaar, WelzijnIndex)

##Next plot
Welzijn_temporal_visualization$Opleidingsniveau <- factor(
  Welzijn_temporal_visualization$Opleidingsniveau,
  levels = c(
    "Hbo-, wo-master, doctor",
    "Hbo-, wo-bachelor",
    "Havo, vwo, mbo2-4",
    "Vmbo, havo-, vwo-onderbouw, mbo1",
    "Basisonderwijs"
  )
)

ggplot(Welzijn_temporal_visualization, aes(x = Jaar, y = WelzijnIndex, color = Opleidingsniveau, group = Opleidingsniveau)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2015:2018) +
  scale_color_manual(
    values = c(
      "Basisonderwijs" = "#F8766D",
      "Vmbo, havo-, vwo-onderbouw, mbo1" = "#C49A00",
      "Havo, vwo, mbo2-4" = "#00BA38",
      "Hbo-, wo-bachelor" = "#00BFC4",
      "Hbo-, wo-master, doctor" = "#C77CFF"
    )
  ) +
  labs(
    title = "Evolution of Welfare Index per Level of Education (2015–2018)",
    x = "Year",
    y = "Welfare Index",
    color = "Education level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )

#Create the 'two temporal visualizations with an event analysis weaved in'-plot
#number 1
ggplot(Ervaren_Gezondheid, 
       aes(x = Perioden, y = ErvarenGezondheidGoedZeerGoed_4,
           colour = Gemeentenaam_1, group = Gemeentenaam_1)) +
  
  geom_line(size = 1.5) +
  geom_text(aes(label = ErvarenGezondheidGoedZeerGoed_4),
            colour = "black", vjust = -1, show.legend = FALSE) +
  geom_point(colour = "black", size = 2.5, show.legend = FALSE) +
  
  # Rotterdam event (2016)
  geom_segment(aes(x = 2, xend = 2, y = 68.4 - 1.7, yend = 68.4 + 2.1), 
               color = "black", linewidth = 0.4) +
  annotate("text", x = 2, y = 69.9 - 3.3, label = "Rotterdam Vitaal en Healthy", 
           color = "black", angle = 0, vjust = 1, size = 3) +
  
  #Amsterdam event (between 2016 and 2020 = x = 2.33)
  geom_segment(aes(x = 2.33, xend = 2.33, y = 73.8 - 0.4, yend = 73.8 + 2.8), 
               color = "black", linewidth = 0.4) +
  annotate("text", x = 2.25, y = 79 - 1.3, label = "Amsterdam Vitaal & Gezond", 
           color = "black", angle = 0, vjust = 1, size = 3) +
  
  scale_colour_manual(values = c("Amsterdam" = "#C41230", "Rotterdam" = "#39B54A")) +
  scale_x_discrete(label = c(2012, 2016, 2020)) +
  scale_y_continuous(limits = c(65, 80), breaks = seq(0, 100, 5)) +
  
  labs(
    x = "Year (2012, 2016, 2020)", 
    y = "Perceived Health (% Good or Very Good)",
    title = "Perceived Health per Municipality (2012-2020)",
    colour = "Municipality"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

#Number 2

#Prepare data
plot_data <- Ultimate_dataset_of_doom_hell_and_destruction %>%
  select(Geslacht, Levensverwachting, WelzijnIndex) %>%
  pivot_longer(
    cols = c("Levensverwachting", "WelzijnIndex"),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = recode(Metric,
                    "Levensverwachting" = "Life Expectancy",
                    "WelzijnIndex" = "Well-Being Index"
    ),
    Label = round(Value, 2)
  )

plot_data$Geslacht <- factor(plot_data$Geslacht, levels = c("Mannen", "Vrouwen"))

ggplot(plot_data, aes(x = Metric, y = Value, fill = Geslacht)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.9) +
  geom_text(
    aes(label = Label),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    color = "black",
    size = 5
  ) +
  scale_fill_manual(
    name = "Gender",
    values = c("Mannen" = "#4B9CD3", "Vrouwen" = "#E06F8B"),
    labels = c("Mannen" = "Men", "Vrouwen" = "Women")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # <- extra ruimte boven balken
  labs(
    title = "Difference in life expectancy and well-being: Men vs Women",
    x = "",
    y = "Value"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.4,
      margin = margin(b = 15)  # <- ruimte onder titel
    )
  )


##############################
###de correlation map maken###
##############################

Ervaren_gezondheid_wijk <- read_delim("Ervarengezondheid_Wijk&Buurt.csv", delim = ";")
Inkomen_per_gemeente <- read_delim("Inkomen_gemeente.csv", delim = ";")
Levensverwachting_Gemeente <- read_delim("Levensverwacht_Gemeente_Wijk&Buurt.csv", delim = ";")
gemeentegrenzen <- st_read("https://service.pdok.nl/cbs/gebiedsindelingen/2023/wfs/v1_0?request=GetFeature&service=WFS&version=2.0.0&typeName=gemeente_gegeneraliseerd&outputFormat=json")

#Clean non-relevant information
Inkomen_per_wijk <- Inkomen_per_gemeente
Inkomen_per_wijk <- Inkomen_per_wijk %>%
  filter(Regionaam != "Totaal")
Inkomen_per_wijk <- Inkomen_per_wijk %>%
  filter(Wijkcode != "Totaal")



Ervaren_gezondheid_wijk <- Ervaren_gezondheid_wijk %>%
  rename(Wijkcode = Codering_3, statnaam = Gemeentenaam_1)

Ervaren_gezondheid_wijk$SoortRegio_2 <- trimws(Ervaren_gezondheid_wijk$SoortRegio_2)
Ervaren_gezondheid_wijk <- Ervaren_gezondheid_wijk %>%
  mutate(Wijkcode = trimws(toupper(Wijkcode)))
Ervaren_gezondheid_wijk <- filter(Ervaren_gezondheid_wijk, SoortRegio_2 %in% c("Gemeente", "Wijk"))

Ervaren_gezondheid_wijk <- Ervaren_gezondheid_wijk %>%
  filter(SoortRegio_2 == "Wijk")

#creating new data set for calculating and mapping later on
#Merge 'gezondheid' and 'welvaart' datasets
gemeente_wijk_data <- inner_join(
  Inkomen_per_wijk, Ervaren_gezondheid_wijk, by = "Wijkcode") %>%
  select(statnaam, Regionaam, Wijkcode, Gemiddeld, ErvarenGezondheidGoedZeerGoed_4)

gemeente_wijk_data$ErvarenGezondheidGoedZeerGoed_4 <- as.numeric(gsub(",", ".", gemeente_wijk_data$ErvarenGezondheidGoedZeerGoed_4))
gemeente_wijk_data$Gemiddeld <- as.numeric(gsub(",", ".", gemeente_wijk_data$Gemiddeld)) #commas omzetten naar punten

##making the correlation variable per gemeente
gemeente_cor <- gemeente_wijk_data %>%
  group_by(statnaam) %>%
  filter(n() >= 3) %>%
  summarise(
    correlation = if (sum(complete.cases(as.numeric(Gemiddeld, ErvarenGezondheidGoedZeerGoed_4))) > 1)
    {cor(as.numeric(Gemiddeld), as.numeric(ErvarenGezondheidGoedZeerGoed_4), use = "complete.obs")} 
    else {NA_real_})


#trimming white spaces so I can add the geometry for the heat map
gemeentegrenzen <- gemeentegrenzen %>%
  mutate(statnaam = tolower(trimws(statnaam)))
gemeente_cor <- gemeente_cor %>%
  mutate(statnaam = tolower(trimws(statnaam))) #negative_cor = 30, NA = 49, positive cor = 263

gemeente_wijk_mapdata <- gemeentegrenzen %>%
  left_join(gemeente_cor, by = "statnaam")


#plotting the heat map
ggplot(gemeente_wijk_mapdata) +
  geom_sf(aes(fill = correlation)) + 
  theme_minimal() +
  scale_fill_gradient2(high = "#6A0DAD", mid = "lightgrey", low = "#FFD700",
                       limits = c(-1, 1),
                       breaks = c(-1, -0.5, 0, 0.5, 1),
                       name = "Correlation 'income & health'") +
  theme(legend.position = "left") + 
  labs(caption = "Grey areas = insufficient data") 


################################################################
######MAKING THE plot SHOWING CORRELATION WEALTH AND HEALTH#####
################################################################

#statcode weg filteren bij Inkomen_per_gemeenten
Inkomen_per_gemeente <- filter(Inkomen_per_gemeente, Wijkcode == "Totaal")
Levensverwachting_Gemeente <- filter(Levensverwachting_Gemeente, Perioden == "2019G400") #2019G400 = gemiddelde van 2019/2022
Levensverwachting_Gemeente <- filter(Levensverwachting_Gemeente, Geslacht == "T001038") #T001038 = Man + Vrouw
Levensverwachting_Gemeente <- filter(Levensverwachting_Gemeente, Marges == "MW00000") #MW00000 = Waarde
Levensverwachting_Gemeente <- filter(Levensverwachting_Gemeente, Leeftijd == "10010") #10010 = 0 jarige

Levensverwachting_Gemeente <- Levensverwachting_Gemeente %>%
  filter(RegioS %in% Inkomen_per_gemeente$Gemeentecode)


#'Gezondheid' and 'welvaart' data merge
gemeente_data <- left_join(Inkomen_per_gemeente, Levensverwachting_Gemeente, by = c("Gemeentecode" = "RegioS"))

#connecting the merged data to the map
gemeente_mapdata <- left_join(gemeentegrenzen, gemeente_data, by = c("statcode" = "Gemeentecode"))

####### calculate the covariance for every municipality ######
gemeente_mapdata$Gemiddeld <- as.numeric(gsub(",", ".", gemeente_mapdata$Gemiddeld)) #commas omzetten naar punten
gemeente_mapdata$Levensverwachting_1 <- as.numeric(gemeente_mapdata$Levensverwachting_1)

cor(gemeente_mapdata$Levensverwachting_1, gemeente_mapdata$Gemiddeld, use = "complete.obs") # +1 = perfect positive relation, 0 = no correlation, -1 = perfect negative correlation                                                                      


ggplot(gemeente_mapdata, aes(x = Gemiddeld, y = Levensverwachting_1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Income", y = "Life Expectancy")

#Making a graph showing development in "ervaren gezondheid" in the municipalities
#Amsterdam and Rotterdam over the years: 2012, 2016, 2020

#Isolating only the entirety of the "Amsterdam" and "Rotterdam" municipalities
Ervaren_Gezondheid <- Ervaren_Gezondheid[
  Ervaren_Gezondheid$Gemeentenaam_1 %in% c("Amsterdam", "Rotterdam") &
    Ervaren_Gezondheid$SoortRegio_2 == "Gemeente",]

#Creating new data set with Amsterdam, Rotterdam, and entire Netherlands
Ervaren_Gezondheid <- Ervaren_Gezondheid %>%
  select(-Gemeentenaam_1, -SoortRegio_2, -Codering_3)

Ervaren_Gezondheid_NL_AM_RO <- rbind(Ervaren_Gezondheid, ErvarenGezondheidNL)

#Converting the created dataset to numerics
Ervaren_Gezondheid_NL_AM_RO$ErvarenGezondheidGoedZeerGoed_4 <- 
  as.numeric(Ervaren_Gezondheid_NL_AM_RO$ErvarenGezondheidGoedZeerGoed_4)

Ervaren_Gezondheid_NL_AM_RO <- Ervaren_Gezondheid_NL_AM_RO %>%
  mutate(Region = case_when(
    WijkenEnBuurten == "GM0363" ~ "Amsterdam",
    WijkenEnBuurten == "GM0599" ~ "Rotterdam",
    WijkenEnBuurten == "NL01"   ~ "The Netherlands",
    TRUE ~ NA_character_
  ))

#Plotting the line graph
ggplot(Ervaren_Gezondheid_NL_AM_RO, 
       aes(x = Perioden, y = ErvarenGezondheidGoedZeerGoed_4,
           colour = Region, group = Region)) +
  geom_line(size = 1.5) +
  geom_point(colour = "black", size = 2.5, show.legend = FALSE) +
  geom_text(aes(label = ErvarenGezondheidGoedZeerGoed_4,
                vjust = ifelse(Region == "The Netherlands", -0.8, 1.9)),
            colour = "black", 
            show.legend = FALSE) +
  scale_colour_manual(values = c(
    "Amsterdam" = "#C41230",
    "Rotterdam" = "#39B54A",
    "The Netherlands" = "orange"
  )) +
  scale_x_discrete(labels = c("2012", "2016", "2020")) +
  scale_y_continuous(limits = c(65, 80), breaks = seq(0, 100, by = 5)) +
  labs(
    x = "Year (2012, 2016, 2020)",
    y = "Perceived health (% wel or very well)",
    title = "Perceived Health per Municipality",
    colour = "Region"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
