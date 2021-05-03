## Laad de libraries
library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

## Inlezen data
strategie_weq <- readRDS("data/strategie_weq.rds")

#### Plotfunctie maken ####

plot_meerkosten <- function(buurt_data,
                            buurtnaam,
                            buurtcode,
                            gemeentenaam) {
  
  
  
  
  ## Maak de plot
  plot <- ggplot(buurt_data, aes(y = Nationale_meerkosten, x = Strategie, fill = Strategie_nr)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=round(Nationale_meerkosten, 2)), vjust=1.6, color="white", size=3.5) +
    labs(y = "Nationale meerkosten (€/weq/jaar)",
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
                                 "5" = "#E6AD1F")) +
    coord_cartesian(ylim = c(0,y_limiet)) 
  
  ## Deze if-statement checkt of de plot relevante data heeft. Dit gaat fout als
  ## er geen strategieen zijn doorgerekend.
  ## deze check zit hier omdat het volgende if-statement anders een fout geeft.
  if (!all(is.na(buurt_data$Strategie))) {
    ## Als er waarden zijn die hoger zijn dan de y-limiet (meestal MT/HT-warmtebronnen), moet dit geannoteerd worden
    if (any(na.omit(buurt_data$Nationale_meerkosten > y_limiet))) {
      te_hoog_nr <- which(buurt_data$Nationale_meerkosten > y_limiet)
      plot <- plot +
        annotate("text", x = te_hoog_nr, y = y_limiet, label = "Waarde buiten\nbereik", col = "grey80")
    }
  }
  
  
  plot
}

#### Plot voor iedere buurt uitvoeren ####

## Bepaal de y-limiet
y_limiet <- 3000


## Maak een functie om voor een buurt de data te selecteren en de plot op te slaan
select_en_plot <- function(buurt, data) {
  
  
  
  ## Selecteer de data
  buurt_data <- data %>% 
    filter(I01_Buurtcode == buurt)
  
  
  ## Bepaal de gemeente en buurtnamen voor de legenda van de plot en het opslaan van de bestanden
  buurtnaam    <- buurt_data$I02_Buurtnaam[1]
  buurtcode    <- buurt_data$I01_Buurtcode[1]
  gemeentenaam <- buurt_data$I06_Gemeentenaam[1]
  
  
  burt_plot <- plot_meerkosten(buurt_data,
                               buurtnaam,
                               buurtcode,
                               gemeentenaam)
  
  
  ## Sla de plot op in de plot-folder
  plot_dir <- paste0("plots/", gemeentenaam)
  if (!dir.exists(plot_dir)) {
    dir.create(plot_dir, recursive = T)
  }
  
  ggsave(paste0("plots/", gemeentenaam, "/Nationale_meerkosten_", buurtcode, ".png"), burt_plot, device = "png", )
  
  
  
}


## Voer de plot uit voor iedere buurt
## Maak een lijst met alle buurten
buurten <- unique(strategie_weq$I01_Buurtcode)

## Voer de plots uit
walk(buurten, select_en_plot, data = strategie_weq)




