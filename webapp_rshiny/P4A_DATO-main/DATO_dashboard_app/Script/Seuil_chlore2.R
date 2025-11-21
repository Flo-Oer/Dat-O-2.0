# Chargement des données depuis le fichier xlsx
# donnees <- read_excel("../Donnees/Dato donnees clean/donnees_PSV.xlsx", col_types = c(Mois = "text"))
df3 <- psv_data

df3$Mois <- as.character(df3$Mois) # Modifier les données de la colonne Mois en chaîne de caractères
contexte <- read_excel("./Donnees/Dato donnees clean/contextePSV_ajour.xlsx")
# Fusionner les données du fichier "contextePSV_ajour.xlsx" avec les données du tableau principal en utilisant la colonne "Numero" comme clé de fusion
donnees_avec_secteur <- merge(df3, contexte[, c("Numero", "Sectorisation")], by = "Numero", all.x = TRUE)

# Obtention de l'année en cours et du mois précédent
annee_actuelle <- max(df3$Annees)
mois_actuel <- format(Sys.Date(), "%m")
mois_precedent <- tail(unique(df3$Mois), 1)

# Extractions des secteurs disponibles
secteurs_disponibles <- unique(contexte$Sectorisation)

create_seuil_table <- function(seuil, mois, annees, secteur, recherche, up_down) {
  
  
  if (is.null(mois) || length(mois) == 0) {
    mois <- mois_precedent
  }
  if (is.null(annees) || length(annees) == 0) {
    annees <- annee_actuelle
  }
  
  subset_data <- donnees_avec_secteur
  
  # Conversion de la colonne "Resultat" en nombres
  subset_data$Resultat <- as.numeric(subset_data$Resultat)
  
  if(up_down == "Inférieur"){
  
  # Filtrage des données en fonction du seuil, du paramètre, du mois et de l'année
  subset_data <- subset(subset_data, Resultat <= seuil & (Parametre == "Cl2 libre (1398)" | Parametre == "Cl2 total (1399)") & Mois %in% mois & Annees %in% annees)
  }
  else {
    subset_data <- subset(subset_data, Resultat >= seuil & (Parametre == "Cl2 libre (1398)" | Parametre == "Cl2 total (1399)") & Mois %in% mois & Annees %in% annees)
  }
  
  
  if (secteur != "Tous les secteurs") {
    subset_data <- subset_data[subset_data$Sectorisation == secteur, ]
  }
  
  if (!is.null(recherche) && recherche != "") {
    subset_data <- subset_data[subset_data$Numero == as.integer(recherche), ]
  }
  
  subset_data$Resultat <- as.numeric(subset_data$Resultat)
  subset_data$Resultat <- format(subset_data$Resultat, digits = 2, nsmall = 2)
  
  subset_data <- subset_data[, c("Numero", "X.2", "X.3", "Jours", "Mois", "Annees", "Parametre", "Resultat", "Unite", "Sectorisation")]
  
  return(subset_data)
}

create_occurence_table <- function(subset_data){
  tryCatch({
  occurrences <- table(subset_data$Numero, subset_data$Mois, subset_data$Annees)
  occurrences <- as.data.frame(occurrences)
  colnames(occurrences) <- c("Numero", "Mois", "Annees", "Occurrence")
  
  # Filtrer les occurrences où Occurrence > 0
  filtered_occurrences <- subset(occurrences, Occurrence > 0)
  filtered_occurrences
  
  # Obtenir la liste des numéros avec plusieurs occurrences
  multiple_occurrences <- subset(filtered_occurrences, ave(Occurrence, Numero, FUN = length) > 1)$Numero
  return(list(filtered_occurrences, multiple_occurrences))
  
  }, error = function(err){
  "Pas de données pour ces paramètres"
  }
)
  
  
}