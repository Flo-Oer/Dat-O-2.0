library(shiny)
library(semantic.dashboard)
library(DT)
library(lintr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(sf)
library(readxl)
library(lubridate)

source("./Carto_2.R")

# position_PSV <- st_read("./www/Donnees_Dato/Loc_PSV_V4/Loc_PSV_V4.shx")
# donnees_PSV <- read_excel("./www/Dato donnees clean/donnees_PSV.xlsx")
# donnees_sondes <- read_excel("./www/Dato donnees clean/MasterKaptaV2.xlsx")
PSV_pivoted <- pivot_wider(psv_data, names_from = Parametre, values_from = Resultat)
merged_data <- merge(position_PSV, PSV_pivoted, by = "Numero")

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
  
  filtered_data_PSV <- merged_data %>%
    filter(Date.de.prelevement >= debut & Date.de.prelevement <= fin, !is.na(`Cl2 libre (1398)`), Sectorisat == choix_zone) %>%
    dplyr::select(Date.de.prelevement, `Cl2 libre (1398)`)
  
  PSV_plot <- ggplot(filtered_data_PSV, aes(x = Date.de.prelevement, y = `Cl2 libre (1398)`)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(x = "Date", y = "Concentration en chlore (mg/L)", title = "Concentration en chlore (mg/L)") +
    theme_minimal()
  return(PSV_plot)
}

create_kapta_plot <- function(debut= "2021-03-01",fin= "2021-04-03"){
  
  
  if (!is.null(debut)) {
    filtered_data <- subset(donnees_sondes, DATE >= as.POSIXct(debut, format = "%Y-%m-%d") & DATE <= as.POSIXct(fin, format = "%Y-%m-%d"))
  } else {
    filtered_data <- donnees_sondes
  }
  
  kapta_plot <- ggplot(filtered_data, aes(x = as.POSIXct(DATEREF), y = `Concentration chlore 2 (mg/L)`)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(x = "Date", y = "Concentration en chlore (mg/L)", title = "Concentration en chlore (mg/L)") +
    theme_minimal()
  
  return(kapta_plot)
}