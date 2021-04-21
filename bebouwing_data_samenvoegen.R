
## installeer de benodigde packages
# install.packages("readr",
#                  "dplyr",
#                  "tidyr",
#                  "purrr")


## Laad de benodigde libraries in
library(readr)
library(dplyr)
library(tidyr)
library(purrr)

## bekijk welke databestanden er zijn in de data_map
gemeentecode <- "GM1950"
data_map <- paste0("data/", gemeentecode)

## Bepaal de csv bestanden die ingelezen moeten worden
bestanden <- list.files(data_map, pattern = "*BU[0-9]{1,10}_bebouwing.csv", recursive = TRUE, full.names = TRUE)


inlezen_filteren_buurtdata <- function(bestandspad) {
  ## bestand inlezen
  data_buurt <- read_delim(bestandspad, 
                           ";", 
                           escape_double = FALSE,
                           trim_ws = TRUE, 
                           col_types = cols(
                             .default = col_character(),
                             Voor_1920 = col_double(),
                             `1921_1975` = col_double(),
                             `1976_1990` = col_double(),
                             `1991_1995` = col_double(),
                             `1996_2019` = col_double(),
                             Onbekend = col_double(),
                             `Totaal_Utiliteit_[Typegebouw_Bouwjaar]` = col_double()
                           ))
  ## selecteer de juiste kolommen
  data_gefilterd <- data_buurt %>% 
    select(I01_Buurtcode, 
           I02_Buurtnaam, 
           Code_Energielabel, 
           Vrijstaande_woning,
           `2_onder_1_kap`,
           Rijwoning_hoek,
           Rijwoning_tussen,
           Appartementen,
           `Totaal_Woningen_[Energielabel_Typegebouw]`) %>% 
    ## filter rijen er uit die geen waardevolle informatie hebben
    filter(Code_Energielabel != "-") %>% 
    ## Transponeer de tabel
    gather(key = Woning_type, value = aantal,
           -I01_Buurtcode,
           -I02_Buurtnaam,
           -Code_Energielabel) %>% 
    spread(key = Code_Energielabel, value = aantal)
  ## Retourneer de gefilterde tabel
  data_gefilterd
}

## voer de functie inlezen_filteren_buurtdata uit voor alle bestanden. alle data komt in 1 dataframe
totaal_buurten <- map_dfr(bestanden,
                          inlezen_filteren_buurtdata)

## Sla de data op als CSV in de output-map
dir.create("output")
write_csv2(totaal_buurten,
           file = paste0("output/", gemeentecode, "_totaal_buurten.csv"))

