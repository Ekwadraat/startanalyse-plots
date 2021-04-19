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

## Bepaal welke zip-bestanden er zijn. Zoek in de huidige map naar een zipfile. 
gemeentebestanden <- list.files(pattern = "GM[0-9]{4}.zip")

#### functie om buurtdata in te lezen ####
plot_meerkosten <- function(buurtpad) {
  ## Haal de buurtcode uit de bestandsnaam
  buurtcode <- str_extract(buurtpad, "BU[0-9]*")
  
  ## Lees de data in
  strategie_data <- read_delim(buurtpad, delim = ";", col_types = cols(
    .default = col_character()
  ))
  ## Bepaal de naam van de buurt en de gemeente
  buurtnaam <- strategie_data$I02_Buurtnaam[1]
  gemeentenaam <- strategie_data$I06_Gemeentenaam[1]
  ## Selecteer de informatie met nationale meerkosten, maak er een tabel van met meerkosten per strategie
  nat_meerkosten <- strategie_data %>% 
    filter(Code_Indicator %in% c("H18_Nat_meerkost_WEQ", "V01_Strategievariant")) %>% 
    select(starts_with("Strategie_"), Code_Indicator, I01_Buurtcode) %>% 
    ## Klap de tabel om
    gather(key = "strategie",
           value = "waarde", 
           -Code_Indicator,
           -I01_Buurtcode) %>% 
    pivot_wider(names_from = Code_Indicator, values_from = waarde) %>% 
    select(-strategie) %>% 
    rename("Strategie" = V01_Strategievariant,
           "Nationale_meerkosten" = H18_Nat_meerkost_WEQ) %>% 
    ## Maak een aantal nieuwe variabelen aan
    mutate(Nationale_meerkosten = as.numeric(Nationale_meerkosten),
           Nationale_meerkosten_m = Nationale_meerkosten / 1000000,
           Strategie_nr = str_extract(Strategie, "[1-5]"),
           Strategie_tekst = case_when(Strategie_nr == "1" ~ "individuele elektrische warmtepomp",
                                       Strategie_nr == "2" ~ "warmtenet met MT- of HT-bron",
                                       Strategie_nr == "3" ~ "warmtenet met LT-bron",
                                       Strategie_nr == "4" ~ "groengas",
                                       Strategie_nr == "5" ~ "waterstof")) 
  ## Maak de plot
  plot <- ggplot(nat_meerkosten, aes(y = Nationale_meerkosten, x = Strategie, fill = Strategie_nr)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=round(Nationale_meerkosten, 2)), vjust=1.6, color="white", size=3.5) +
    labs(y = "Nationale meerkosten (WEQ)",
         title = paste("Nationale meerkosten per warmtestrategie"),
         subtitle = paste0("gemeente ", gemeentenaam,", ", "buurt ", buurtnaam, " (", buurtcode, ")")) +
    theme_minimal() + 
    ## verwijder de legenda
    theme(legend.position = "none") +
    ## Gebruik de kleuren uit de startanalyse voor de strategieen  
    scale_fill_manual(values = c("1" = "#9a0363",
                                 "2" = "#c1241f",
                                 "3" = "#1d9cdc",
                                 "4" = "#288042",
                                 "5" = "#E6AD1F"))
    ## Sla de plot op in de plot-folder
  plot_dir <- paste0("plots/", gemeentenaam)
  dir.create(plot_dir, recursive = T)
  ggsave(paste0("plots/", gemeentenaam, "/Nationale_meerkosten_", buurtcode, ".png"), plot, device = "png", )
}

plot_gemeente <- function(zippad) {
  ## Pak de zip uit in de map /data
  unzip(zippad, exdir = "data")
  
  ## bepaal gemeentecode uit de bestandsnaam van het zip-bestand
  code_gemeente <- str_extract(zippad, pattern = "GM[0-9]*")
  
  ## Bepaal datamap waarin de buurtdata staat
  datamap <- paste("data", code_gemeente, paste0(code_gemeente, "_data_buurten_csv"), sep = "/")
  
  ## bepaal de in te lezen buurten door te bekijken welke csv-bestanden beschikbaar zijn
  buurten_bestanden <- list.files(datamap, pattern = "strategie", full.names = TRUE)
  
  ## Voer de plotfunctie uit voor iedere buurt
  walk(buurten_bestanden, plot_meerkosten)
}


walk(gemeentebestanden, plot_gemeente)
