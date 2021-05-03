## Laad de libraries
library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

#### Voorbereidingen ####
## Maak een mapje voor de plots en de data
dir.create("plots")
dir.create("data")


#### Uitpakken data startanalyse ####
## Bepaal welke zip-bestanden er zijn. Zoek in de input_data map naar een zipfile. 
gemeente_zips <- list.files(path = "input_data",
                            pattern = "GM[0-9]{4}.zip",
                            full.names = TRUE)

gemeente_zip <- gemeente_zips[1]

uitpakken_inlezen_gemeente(gemeente_zip)

uitpakken_inlezen_gemeente <- function(gemeente_zip) {
  ## Pak de zip uit in de map /data
  unzip(gemeente_zip, exdir = "data", overwrite = T)
  
  ## bepaal gemeentecode uit de bestandsnaam van het zip-bestand
  code_gemeente <- str_extract(gemeente_zip, pattern = "GM[0-9]*")
  
  ## Bepaal datamap waarin de buurtdata staat
  datamap <- paste("data", code_gemeente, paste0(code_gemeente, "_data_buurten_csv"), sep = "/")
  
  ## bepaal de in te lezen buurten door te bekijken welke csv-bestanden beschikbaar zijn
  buurt_csvs <- list.files(datamap, pattern = "strategie", full.names = TRUE)
  
  
  map(buurt_csvs, inlezen_buurt)


}

inlezen_buurt <- function(buurt_csv) {
  ## Haal de buurtcode uit de bestandsnaam
  buurtcode <- str_extract(buurt_csv, "BU[0-9]*")
  
  ## Lees de data in
  buurt_data <- read_delim(buurt_csv, delim = ";", col_types = cols(
    .default = col_character()
  )) %>% 
    select(BU_CODE,
           I01_buurtcode,
           I02_buurtnaam,
           I06_gemeentenaam)
  
  buurt_data
}

gemeente_data <- map_df(gemeente_zips, uitpakken_inlezen_gemeente)


## Selecteer de informatie met nationale meerkosten, maak er een tabel van met meerkosten per strategie
strategie_weq <- gemeente_data %>% 
  filter(Code_Indicator %in% c("H18_Nat_meerkost_WEQ", "V01_Strategievariant")) %>% 
  select(starts_with("Strategie_"), Code_Indicator, I01_Buurtcode, I02_Buurtnaam, I06_Gemeentenaam) %>% 
  ## Klap de tabel om
  gather(key = "strategie",
         value = "waarde", 
         -Code_Indicator,
         -I01_Buurtcode,
         -I02_Buurtnaam, 
         -I06_Gemeentenaam) %>% 
  pivot_wider(names_from = Code_Indicator, values_from = waarde) %>% 
  select(-strategie) %>% 
  rename("Strategie" = V01_Strategievariant,
         "Nationale_meerkosten" = H18_Nat_meerkost_WEQ) %>% 
  ## Maak een aantal nieuwe variabelen aan
  mutate(Nationale_meerkosten = na_if(Nationale_meerkosten, "-"),
         Nationale_meerkosten = as.numeric(Nationale_meerkosten),
         Strategie_nr = str_extract(Strategie, "[1-5]"),
         Strategie_tekst = case_when(Strategie_nr == "1" ~ "individuele elektrische warmtepomp",
                                     Strategie_nr == "2" ~ "warmtenet met MT- of HT-bron",
                                     Strategie_nr == "3" ~ "warmtenet met LT-bron",
                                     Strategie_nr == "4" ~ "groengas",
                                     Strategie_nr == "5" ~ "waterstof")) 

saveRDS(strategie_weq, "data/strategie_weq.rds")

