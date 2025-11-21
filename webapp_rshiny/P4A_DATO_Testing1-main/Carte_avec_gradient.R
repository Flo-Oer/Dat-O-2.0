library(sf)
library(sp)
library(leaflet)
library(readxl)
library(dplyr)
library(dismo)

# Read the shapefile with polylines
shpaffich <- "/Users/delaygues/Desktop/4A/DATO/donnees_dato/wMain_202205_Nice_secteurs/wMain_202205_Nice_secteurs.shp"
filechem <- file.path(shpaffich)
cd_bar <- st_read(filechem)
cd_bar2 <- st_zm(cd_bar)
cd_bar2 <- st_transform(cd_bar2, CRS("+init=epsg:4326"))

# Read the Excel file with point coordinates
data_file <- "/Users/delaygues/Desktop/4A/DATO/Dato donnees clean/position-sondes-global.xlsx"
data <- readxl::read_excel(data_file)

# Read the shapefile with points
points <- st_read("/Users/delaygues/Desktop/4A/DATO/donnees_dato/Loc_PSV_V4/Loc_PSV_V4.shp")

# Transform the spatial coordinates to EPSG 4326
points <- st_transform(points, CRS("+init=epsg:4326"))

# Extract the X and Y coordinates 
coords <- st_coordinates(points)
points$XWGS84 <- coords[,1]
points$YWGS84 <- coords[,2]

colnames(coords) <- c("Longitude", "Latitude")


# Create the leaflet map
carte <- leaflet() %>%
  addTiles() %>%
  setView(lng = 7.25, lat = 43.7, zoom = 12)

# Add polylines to the leaflet map
carte <- carte %>%
  addPolylines(data = cd_bar2, color = "black", weight = 1.5, opacity = 0.7, fillOpacity = 1,
               highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
               label = cd_bar2$FACILITYID)

# Define the function
get_popup_content <- function(ENDPOINTREF, last_mesure, Latitude, Longitude, DATE, HEURE) {
  paste("ID Sonde: ", ENDPOINTREF,
        " <br> Last Concentration: ", last_mesure,
        " <br> Latitude: ", Latitude,
        " <br> Longitude: ", Longitude,
        " <br> Date: ", format(DATE, "%Y-%m-%d"),
        " <br> Time: ", format(HEURE, "%H:%M:%S"))
}

# Read the Excel file with chlorine concentrations
cl_data_file <- "/Users/delaygues/Desktop/4A/DATO/Dato donnees clean/MasterKaptaV2.xlsx"
cl_data <- readxl::read_excel(cl_data_file)

# Find the last concentration for each probe
last_conc <- cl_data %>%
  arrange(desc(DATE), desc(HEURE)) %>%
  group_by(ENDPOINTREF) %>%
  slice(1) %>%
  mutate(last_mesure = mean(`Concentration chlore 1 (mg/L)`, `Concentration chlore 2 (mg/L)`, na.rm = TRUE)) %>%
  dplyr::select(ENDPOINTREF, last_mesure, DATE, HEURE)

# Read the Excel file with point coordinates
coord_data_file <- "/Users/delaygues/Desktop/4A/DATO/Dato donnees clean/position-sondes-global.xlsx"
coord_data <- readxl::read_excel(coord_data_file)

# Combine the coordinate data with the last concentration data
data <- left_join(coord_data, last_conc, by = "ENDPOINTREF")

# Create a data frame with Longitude and Latitude columns
points2 <- data.frame(Longitude = data$Longitude, Latitude = data$Latitude)
all_points <- rbind(points2, coords)

# Convert the data frame to an sf object with WGS84 CRS
all_points <- na.omit(all_points)
all_points_sf <- st_as_sf(all_points, coords = c("Longitude", "Latitude"))
st_crs(all_points_sf) <- 4326

polys <- all_points_sf %>%
  as("Spatial") %>%
  voronoi() %>%
  st_as_sf() %>%
  st_set_crs(., 4326)

# Add markers to the leaflet map with popup content generated using the function
carte <- carte %>%
  addMarkers(data = data, lng = ~Longitude, lat = ~Latitude, 
             label = ~paste("Latitude: ", data$Latitude, "Longitude: ", data$Longitude),
             popup = ~get_popup_content(ENDPOINTREF, last_mesure, Latitude, Longitude, DATE, HEURE))

colorScale <- function(x) {
  ifelse(x < 0.03, "red", ifelse(x < 0.8, "green", "purple"))
}

# Add polygons to the leaflet map with a gradient of color based on last_mesure
carte <- carte %>%
  addPolygons(data = polys, 
              fillColor = ~colorScale(data$last_mesure), 
              weight = 1, 
              color = "black",
              label = ~paste("Latitude: ", data$Latitude, "Longitude: ", data$Longitude),
              popup = ~get_popup_content(data$ENDPOINTREF, data$last_mesure, data$Latitude, data$Longitude, data$DATE, data$HEURE),
              fillOpacity = 0.7, 
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE))

# Remove the last four rows from the point shapefile
points <- points %>% 
  slice(1:(nrow(.)-4))

# Add points to the leaflet map
carte <- carte %>%
  addCircleMarkers(data = points, lng = ~XWGS84, lat = ~YWGS84, popup = ~Sectorisat)

# Display the leaflet map
carte
