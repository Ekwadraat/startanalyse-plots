## Laad de libraries
library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)


#### Inlezen ####

## bepaal de vesta mapnaam
vesta_map <- "input_data/vesta"

## Lees alle strategie data in uit de map
bestanden <- list.files(vesta_map,
                        pattern = "Strategie_[1-5]_Hoofdindicatoren.csv",
                        full.names = TRUE)

strategieen <- map_dfr(bestanden,
                   read_delim,
                      delim = ";", 
                      quote = "'", 
                      escape_double = FALSE, 
                      trim_ws = TRUE,
                      col_types = cols(
                        .default = col_double(),
                        BU_CODE = col_character(),
                        V01_Strategievariant = col_character(),
                        H20_beschikbaarheid_hernieuwbaar_gas = col_character()
                      ))

## Lees de statische buurtgegevens in
buurtnamen <- read_delim(paste0(vesta_map, "/statisch.csv"),
                         delim = ";",
                         quote = "'",
                         col_types = cols(BU_CODE = col_character(),
                                          I01_buurtcode = col_character(),
                                          I02_buurtnaam = col_character(),
                                          I03_wijkcode = col_character(),
                                          I04_wijknaam = col_character(),
                                          I05_gemeentecode = col_character(),
                                          I06_gemeentenaam = col_character(),
                                          I07_energieregionaam = col_character(),
                                          I08_provincienaam = col_character(),
                                          `I09_aantal_woningen [Aansluiting]` = col_double(),
                                          `I10_aantal_utiliteit [Aansluiting]` = col_double(),
                                          `I11_woningequivalenten [Woning]` = col_double(),
                                          `I12_CO2_startjaar [ mega g per yr]` = col_double(),
                                          I13_Uitgesloten = col_logical()))

#### Bewerken ####

## Maak een tabel met buurtnamen om aan de strategiedata te koppelen
buurtnamen <- buurtnamen %>% 
  select(BU_CODE,
         I01_Buurtcode   = I01_buurtcode,
         I02_Buurtnaam   = I02_buurtnaam,
         I06_Gemeentenaam = I06_gemeentenaam)

## Selecteer de juiste gegevens uit de verschillende strategieen
strategie_weq <- strategieen %>% 
  select(BU_CODE, 
         Strategie            = V01_Strategievariant, 
         Nationale_meerkosten = `H18_Nat_meerkost_WEQ [Euro per Woning*yr]`) %>% 
  mutate(Strategie_nr         = str_extract(Strategie, "[1-5]"),
         Nationale_meerkosten = as.numeric(Nationale_meerkosten),
         Strategie_tekst = case_when(Strategie_nr == "1" ~ "individuele elektrische warmtepomp",
                                     Strategie_nr == "2" ~ "warmtenet met MT- of HT-bron",
                                     Strategie_nr == "3" ~ "warmtenet met LT-bron",
                                     Strategie_nr == "4" ~ "groengas",
                                     Strategie_nr == "5" ~ "waterstof")) %>% 
  left_join(buurtnamen, by = "BU_CODE") %>% 
  select(-BU_CODE)

saveRDS(strategie_weq, "data/strategie_weq.rds")

