
source("./Script/Carto_2.R")


PSV_pivoted <- pivot_wider(psv_data, names_from = Parametre, values_from = Resultat)
merged_data <- merge(position_PSV, PSV_pivoted, by = "Numero")

# Fonction pour créer le PSV plot
create_PSV_plot <- function(date_debut= "2021-03-01",date_fin= "2021-04-03", choix_zone){
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
  
  # Filtrage
  filtered_data_PSV <- merged_data %>%
    filter(Date.de.prelevement >= debut & Date.de.prelevement <= fin, !is.na(`Cl2 libre (1398)`), Sectorisat == choix_zone) %>%
    dplyr::group_by(Date.de.prelevement, Sectorisat) %>%
    dplyr::summarise(`Moyenne chlore` = mean(`Cl2 libre (1398)`, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(Date.de.prelevement, `Moyenne chlore`) %>%
    dplyr::rename(Date = Date.de.prelevement) # Renomme les colonnes 
  
  PSV_plot <- ggplot(filtered_data_PSV, aes(x = Date, y = `Moyenne chlore`)) + # Création du graphique
    geom_bar(stat = "identity", fill = "purple") +
    labs(x = "Date", y = "Concentration en chlore (mg/L)", title = "Concentration en chlore (mg/L)") +
    theme_minimal()
  
  PSV_plot <- ggplotly(PSV_plot, tooltip = c("x", "y")) # Conversion en plotly
  
  # Retourne le graph
  return(PSV_plot)
}

# Fonction pour créer le graph kapta
create_kapta_plot <- function(debut= "2021-03-01",fin= "2021-04-03", choix_zone){
  
  # Filtrage
  filtered_data_kapta <- donnees_sondes %>%
    filter(DATEREF >= as.POSIXct(debut) & DATEREF <= as.POSIXct(fin), ENDPOINTREF == choix_zone) %>%
    dplyr::group_by(DATEREF, ENDPOINTREF) %>%
    dplyr::summarise(`Moyenne chlore` = mean(`Concentration chlore 2 (mg/L)`, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(DATEREF, `Moyenne chlore`) %>%
    dplyr::rename(Date = DATEREF)
  
  kapta_plot <- ggplot(filtered_data_kapta, aes(x = Date, y = `Moyenne chlore`)) + # Création du graphique
    geom_bar(stat = "identity", fill = "purple") +
    labs(x = "Date", y = "Concentration en chlore (mg/L)", title = "Concentration en chlore (mg/L)") +
    theme_minimal()
  
  kapta_plot <- ggplotly(kapta_plot, tooltip = c("x", "y")) # Conversion en plotly
  
  # Retourne le graph
  return(kapta_plot)
}