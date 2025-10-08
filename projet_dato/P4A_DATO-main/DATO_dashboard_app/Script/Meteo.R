

# Charge les données CSV
donnees <- read_excel("./Donnees/Dato donnees clean/MasterMeteo.xlsx")

# Convertir les colonnes "Date" et "Heure" en format Date et POSIXct
donnees$Date <- as.Date(donnees$Date, format = "%d/%m/%Y")
donnees$Heure <- format(as.POSIXct(donnees$Heure, format = "%H:%M"), "%H:%M") # Convertir en chaîne de caractères
donnees$Heure <- factor(donnees$Heure, levels = sprintf("%02d:00", 0:23)) # Convertir en facteur

# Fonction pour créer les graph de pluie
create_plot_meteo <- function(debut,fin){
  debut <- tryCatch(
    ymd(debut),
    error = function(e) NA
    )
  # Check if conversion succeeded
  if (is.na(debut)) {
    # handle error
    debut <- as.POSIXct("2021-03-01") 
    
  } else {
    # use date_debut
  }
  
  fin <- tryCatch(
    ymd(fin),
    error = function(e) NA
  )
  # Check if conversion succeeded
  if (is.na(fin)) {
    # handle error
    fin <- as.POSIXct("2021-05-01") 
    
  } else {
    # use date_debut
  }
  
  debut <- as.Date(debut)
  fin <- as.Date(fin)
  
  donnees_filtrées <- donnees %>%
    filter(Date >= debut & Date <= fin) %>%
    dplyr::select(Date, RR1)
  
  p <- ggplot(donnees_filtrées, aes(x = Date, y = RR1)) +
    geom_bar(stat = "identity", fill = "purple") + # Utiliser un diagramme à barres verticales
    labs(title = "Quantité de pluie sur la période",
         x = "Date",
         y = "Quantité de pluie (RR1)") +
    theme_minimal()

  plot_meteo <- ggplotly(p, tooltip = c("x", "y"))  # Conversion en plotly
   
  
  return(plot_meteo)

}