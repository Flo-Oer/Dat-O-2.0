# Charger la bibliothèque readxl
library(readxl)

debut <- as.POSIXct("2017-03-01 00:00:00")  # Date et heure de début de la période
fin <- as.POSIXct("2022-03-10 12:59:59")

# Lire le fichier Excel
donnees_meteo <- read_excel("/Users/delaygues/P4A_DATO/Dato donnees clean/MasterMeteo.xlsx")
donnees_meteo <- na.omit(donnees_meteo, cols = "RR1")

# Supprimer la partie erronée de la colonne "Heure"
donnees_meteo$Heure <- gsub("1899-12-31 ", "", donnees_meteo$Heure)

# Concaténer la colonne "Date" et la colonne "Heure" en une seule colonne "Datetime"
donnees_meteo$Datetime <- paste(donnees_meteo$Date, donnees_meteo$Heure)

# Convertir la colonne "Datetime" au format de date et d'heure
donnees_meteo$Datetime <- as.POSIXct(donnees_meteo$Datetime, format = "%Y-%m-%d %H:%M:%S")

# Sélectionner les colonnes que vous souhaitez conserver
donnees_meteo <- subset(donnees_meteo, select = c("Datetime", "T° (°C)", "RR1"))

# Vérifier le format de la colonne "Datetime"
format(donnees_meteo$Datetime, "%Y-%m-%d %H:%M:%S")

# Extraire les données pour la période spécifiée
donnees_meteo_subset <- subset(donnees_meteo, Datetime >= debut & Datetime <= fin)

# Calculer la moyenne de la colonne "variable"
mean = mean(donnees_meteo_subset$RR1)

# Charger la bibliothèque ggplot2
library(ggplot2)

# Créer un graphique en ligne des données RR1 en fonction du temps
ggplot(donnees_meteo_subset, aes(x = Datetime, y = RR1)) +
  geom_line() +
  labs(x = "Date et Heure", y = "RR1")

q <- quantile(donnees_meteo$RR1, 0.99)
q







