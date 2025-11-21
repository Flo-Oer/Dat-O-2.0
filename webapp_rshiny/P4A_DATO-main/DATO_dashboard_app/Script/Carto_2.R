

donnees_sondes <<- read_excel("./Donnees/Dato donnees clean/MasterKaptaV2.xlsx", range = cell_cols("C:P"))

# Read the shapefile with polylines
cd_bar <- st_read("./Donnees/Donnees_Dato/wMain_202205_Nice_secteurs/wMain_202205_Nice_secteurs.shp")
cd_bar2 <- st_zm(cd_bar)
cd_bar2 <- st_transform(cd_bar2, CRS("+init=epsg:4326"))


# Read the Excel file with point coordinates
# ---- Load Kapta sondes coordinates safely ----
position_sondes <<- read_excel("./Donnees/Dato donnees clean/position-sondes-global.xlsx")

# Ensure consistent column naming (accepts either XWGS84/YWGS84 or Longitude/Latitude)
if (!("Longitude" %in% names(position_sondes)) && "XWGS84" %in% names(position_sondes)) {
  position_sondes <- position_sondes %>% rename(Longitude = XWGS84)
}
if (!("Latitude" %in% names(position_sondes)) && "YWGS84" %in% names(position_sondes)) {
  position_sondes <- position_sondes %>% rename(Latitude = YWGS84)
}

# Confirm coordinates are numeric
position_sondes$Longitude <- as.numeric(position_sondes$Longitude)
position_sondes$Latitude  <- as.numeric(position_sondes$Latitude)


# Read the Excel file with point data
psv_data <<- read_excel("./Donnees/Dato donnees clean/donnees_PSV.xlsx")


# Read the shapefile with position_PSV and Transform the spatial coordinates to EPSG 4326 to extract X and Y coordinates
position_PSV <- st_read("./Donnees/Donnees_Dato/Loc_PSV_V4/Loc_PSV_V4.shp")
position_PSV <- st_transform(position_PSV, CRS("+init=epsg:4326"))
coords <- st_coordinates(position_PSV)
position_PSV$XWGS84 <- coords[,1]
position_PSV$YWGS84 <- coords[,2]

# Fonction pour générer le contenu du popup
get_popup_content_sondes <- function(endpointref, avg_concentration, latitude, longitude) {
  popup_content <- paste0("<div style='text-align: center; font-weight: bold;'>Sonde ", endpointref, "</div>",
                          "<div><strong> Secteur: </strong>", "NICE / BAS SERVICE </div>",
                          "<div><strong>Average Concentration:</strong> ", avg_concentration, "</div>")
  return(popup_content)
}

# Define the function to generate the popup content
get_popup_content_psv <- function(numero, sectorisat, avg_concentration, lng, lat) {
  paste("<div style='text-align: center; font-weight: bold;'>PSV ", numero, "</div>",
        "<div><strong> Secteur: </strong>", sectorisat, "</div>",
        "<div><strong>Concentration moyenne:</strong> ", avg_concentration, "</div>")
}


create_leafMap <- function(date_debut = "2021-03-01",date_fin = "2021-05-03"){

# Create the leaflet map
carte <- leaflet() %>%
  addTiles() %>%
  setView(lng = 7.25, lat = 43.7, zoom = 13)

# Define a color palette for the sectors
color_palette <- colorFactor(palette = "Set1", domain = cd_bar2$SECTORISAT)


# Définir les variables de début et de fin de la période
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
  debut <- as.POSIXct("2014-04-03") 
  
} else {
  # use date_debut
}
if (is.na(fin)) {
  # handle error
  fin <- as.POSIXct("2022-09-03") 
  
} else {
  # use date_fin
} 

# Filtrer les données en fonction de la période spécifiée
filtered_data <- donnees_sondes %>%
  filter(DATEREF >= debut & DATEREF <= fin)

# Calculer la moyenne des concentrations de chlore pour chaque point
avg_conc <- filtered_data %>%
  group_by(ENDPOINTREF) %>%
  summarise(avg_concentration = round((mean(`Concentration chlore 1 (mg/L)`, na.rm = TRUE) + mean(`Concentration chlore 2 (mg/L)`, na.rm = TRUE)) / 2, digits = 2))

# Joindre les données de coordonnées avec les moyennes des concentrations de chlore
position_sondes <- left_join(position_sondes, avg_conc, by = "ENDPOINTREF")

# Fonction pour générer le contenu du popup
get_popup_content_sondes <- function(endpointref, avg_concentration, latitude, longitude) {
  popup_content <- paste0("<div style='text-align: center; font-weight: bold;'>Sonde ", endpointref, "</div>",
                          "<div><strong> Secteur: </strong>", "NICE / BAS SERVICE </div>",
                          "<div><strong>Concentration moyenne:</strong> ", avg_concentration, "</div>")
  return(popup_content)
}


# Filtrer les données en fonction de la période spécifiée
filtered_data_PSV <- psv_data %>%
  filter(Unite %in% c("mg(Cl2)/L (165)")) %>%
  filter(Date.de.prelevement >= debut & Date.de.prelevement <= fin &
           hour(`Heure arrondi`) >= hour(debut) &
           hour(`Heure arrondi`) <= hour(fin) &
           minute(`Heure arrondi`) >= minute(debut) &
           minute(`Heure arrondi`) <= minute(fin) &
           second(`Heure arrondi`) >= second(debut) &
           second(`Heure arrondi`) <= second(fin)
  )

# Calculer la moyenne des concentrations de chlore pour chaque point
avg_conc_PSV <- filtered_data_PSV %>%
  group_by(Numero) %>%
  summarise(avg_concentration_psv = round(mean(Resultat), digits = 2))

# Add the Sectorisat column to avg_conc_PSV
avg_conc_PSV$Sectorisat <- position_PSV$Sectorisat[match(avg_conc_PSV$Numero, position_PSV$Numero)]

# Remove any rows where Sectorisat is missing or Numero does not exist in position_PSV
avg_conc_PSV <- avg_conc_PSV[!is.na(avg_conc_PSV$Sectorisat) & avg_conc_PSV$Numero %in% position_PSV$Numero,]

# Ajouter les colonnes XWGS84 et YWGS84 à avg_conc_PSV
avg_conc_PSV$XWGS84 <- position_PSV$XWGS84[match(avg_conc_PSV$Numero, position_PSV$Numero)]
avg_conc_PSV$YWGS84 <- position_PSV$YWGS84[match(avg_conc_PSV$Numero, position_PSV$Numero)]

# Define the function to generate the popup content
get_popup_content_psv <- function(numero, sectorisat, avg_concentration, lng, lat) {
  paste("<div style='text-align: center; font-weight: bold;'>PSV ", numero, "</div>",
        "<div><strong> Secteur: </strong>", sectorisat, "</div>",
        "<div><strong>Concentration moyenne:</strong> ", avg_concentration, "</div>")
}

avg_conc_secteur <- avg_conc_PSV %>%
  group_by(Sectorisat) %>%
  summarise(Concentration_moyenne = round(mean(avg_concentration_psv), digits = 2))

# calculate the mean of the avg_concentration column from the avg_conc table
mean_avg_conc <- mean(avg_conc$avg_concentration)

# Modify the Concentration_moyenne column in the avg_conc_secteur table
avg_conc_secteur <- avg_conc_secteur %>%
  mutate(Concentration_moyenne = ifelse(Sectorisat == "NICE / BAS SERVICE",
                                        round(mean(c(Concentration_moyenne, mean_avg_conc)), 2),
                                        round(Concentration_moyenne, 2)))


# Fusionner les données de concentration moyenne par secteur avec cd_bar2
cd_bar2 <- cd_bar2 %>%
  left_join(avg_conc_secteur, by = c("SECTORISAT" = "Sectorisat"))

get_color <- function(concentration) {
  ifelse(is.na(concentration), "#808080", # gris pour les valeurs manquantes
         ifelse(concentration < 0.1, "#CB2B3E", # rouge
                ifelse(concentration < 0.15, "#FFD326", # jaune
                       "#006400"))) # vert
}


# Ajouter les polylignes à la carte avec une couleur en fonction de la concentration
carte <- carte %>%
  addPolylines(data = cd_bar2, 
               color = ~get_color(Concentration_moyenne), 
               weight = 1.5, opacity = 0.8, fillOpacity = 0, 
               label = cd_bar2$SECTORISAT,
               popup = paste("<div style='text-align: center; font-weight: bold;'>", cd_bar2$SECTORISAT, "</div>",
                             "<div style='font-size: 1.2em;'><strong>Average Concentration:</strong> ", cd_bar2$Concentration_moyenne, "</div>"),
               group = cd_bar2$SECTORISAT)

get_icon_url <- function(concentration) {
  ifelse(is.na(concentration),
         "./www/Image/icon/marker-icon-2x-black.png",
         ifelse(concentration < 0.1,
                "./www/Image/icon/marker-icon-2x-red.png",
                ifelse(concentration < 0.15,
                       "./www/Image/icon/marker-icon-2x-gold.png",
                       "./www/Image/icon/marker-icon-2x-green.png")))
}


# Add Markers to the leaflet map with different colors based on concentration
carte <- carte %>%
  addMarkers(data = avg_conc_PSV, lng = ~XWGS84, lat = ~YWGS84, 
             popup = ~get_popup_content_psv(Numero, Sectorisat, avg_concentration_psv, XWGS84, YWGS84),
             icon = ~makeIcon(iconUrl = get_icon_url(avg_concentration_psv), 
                              iconWidth = 21, iconHeight = 34, iconAnchorX = 12, iconAnchorY = 41))


# Function to generate icon URL based on concentration value
get_icon_url_sondes <- function(concentration) {
  ifelse(is.na(concentration),
         "./www/Image/icon/icon_sondes_gris.png",
         ifelse(concentration < 0.1,
                "./www/Image/icon/icon_sondes_rouge_fonce.png",
                ifelse(concentration < 0.15,
                       "./www/Image/icon/icon_sondes_jaune.png",
                       "./www/Image/icon/icon_sondes_vert.png")))
}

# Ajouter les marqueurs à la carte avec le popup et l'icône correspondants
carte <- carte %>%
  addMarkers(data = position_sondes, lng = ~Longitude, lat = ~Latitude,
             popup = ~ifelse(!is.na(avg_concentration),
                             get_popup_content_sondes(ENDPOINTREF, avg_concentration, Latitude, Longitude),
                             paste("<div style='text-align: center; font-weight: bold;'>Sonde ", ENDPOINTREF, "</div>",
                                   "<div> Aucune valeur sur la période<div>")),
             icon = ~makeIcon(iconUrl = get_icon_url_sondes(avg_concentration), 
                              iconWidth = 29, iconHeight = 38, iconAnchorX = 14.5, iconAnchorY = 30))

# Afficher la carte
carte
}