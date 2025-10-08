# Charger la librairie ggplot2 pour créer des visualisations
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)

call_filtered_data <- function(date_debut,date_fin){
  #Définir les variables de début et de fin de la période
  

  
  # date_debut <- strptime(as.character(date_debut), "%Y-%m-%d %H:%M:%S")
  # date_fin <- strptime(as.character(date_fin), "%Y-%m-%d %H:%M:%S")
  
  # debut <- as.POSIXct(date_debut, format="%Y-%m-%d")  # Date et heure de début de la période
  # fin <- as.POSIXct(date_fin, format = "%Y-%m-%d")    # Date et heure de fin de la période
  
  # debut <- as.POSIXct(date_debut, format="%Y-%m-%d %H:%M:%S" )  # Date et heure de début de la période
  # fin <- as.POSIXct(date_fin, format = "%Y-%m-%d %H:%M:%S")    # Date et heure de fin de la période
  debut <- tryCatch(
    ymd(date_debut),
    error = function(e) NA
  )
  fin <- tryCatch(
    ymd(date_fin),
    error = function(e) NA
  )
  
  # Check if conversion succeeded
  if (is.na(debut)) {
    # handle error
    debut <- as.POSIXct("2021-03-01") 
    
  } else {
    # use date_debut
  }
  if (is.na(fin)) {
    # handle error
    fin <- as.POSIXct("2021-04-03") 
    
  } else {
    # use date_fin
  }
  
  
  
  # Filtrer les données en fonction de la période spécifiée et sélectionner les colonnes de concentration de chlore 1 et chlore 2
  #donnees_sondes$DATEREF <- as.POSIXct(donnees_sondes$DATEREF, format("%Y-%m-%d %H:%M:%S"))
  filtered_data <- donnees_sondes %>%
    filter(donnees_sondes$DATEREF >= debut & donnees_sondes$DATEREF <= fin, !is.na(`Concentration chlore 1 (mg/L)`) & !is.na(`Concentration chlore 2 (mg/L)`)) %>%
    dplyr::select(`Concentration chlore 1 (mg/L)`, `Concentration chlore 2 (mg/L)`) %>%
    tidyr::pivot_longer(cols = everything(), values_to = "Concentration (mg/L)") # Regrouper les colonnes en une seule colonne
  
  # Ajouter les données de concentration de chlore du fichier psv_data dans la colonne "Concentration" et renommer la colonne "Numero" en "name"
  filtered_data <- bind_rows(filtered_data, psv_data %>%
                               filter(Date.de.prelevement >= as.Date(debut) & Date.de.prelevement <= as.Date(fin)) %>%
                               dplyr::select(Numero, Resultat, Unite) %>%
                               filter(Unite %in% c("mg(C)/L", "mg(Cl2)/L")) %>%
                               dplyr::mutate(name = as.character(Numero), `Concentration (mg/L)` = Resultat, .keep = "unused") %>%
                               dplyr::select(name, `Concentration (mg/L)`))
  return(filtered_data)
}

# Créer un histogramme avec ggplot2
create_histo <- function(debut= "2021-03-01",fin= "2021-04-03"){

filter_data <- call_filtered_data(debut,fin)

histogramme <- ggplot(filter_data, aes(x = `Concentration (mg/L)`)) +
  geom_histogram(fill = "#B19CD9", color = "#61589C", bins = 0.05, 
                 breaks = seq(0, 0.3, by = 0.05)) +
  labs(title = paste("Répartition des Concentrations de Chlore\nPériode :", debut, "à", fin),
       x = "Concentration de Chlore (mg/L)", y = "Fréquence") +
  xlim(0, 0.3) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

return(histogramme)
}

# Afficher l'histogramme
print(create_histo())

# Créer un boxplot avec ggplot2
create_box <- function(debut= "2021-03-01 00:00:00",fin = "2021-04-03 12:59:59") {

filter_data <- call_filtered_data(debut,fin)
  
boxplot <- ggplot(filter_data, aes(x = "", y = `Concentration (mg/L)`)) +
  geom_boxplot(fill = "#B19CD9", color = "#61589C", width = 0.5) +
  labs(title = paste("Répartition des Concentrations de Chlore\nPériode :", debut, "à", fin), 
       x = "", y = "Concentration de Chlore (mg/L)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

return(boxplot)

}
# Afficher le boxplot
#print(histogramme, boxplot)


