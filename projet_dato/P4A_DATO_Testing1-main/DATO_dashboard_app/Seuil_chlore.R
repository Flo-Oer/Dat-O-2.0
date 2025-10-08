# Installer les packages nécessaires s'ils ne sont pas déjà installés
if (!require('shiny')) install.packages('shiny')
if (!require('shinydashboard')) install.packages('shinydashboard')
if (!require('readr')) install.packages('readr')
if (!require('dplyr')) install.packages('dplyr')
if (!require('DT')) install.packages('DT')

# Charger les packages
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(DT)
library(readxl)

df1 <- read_excel("./www/Dato donnees clean/MasterKaptaV2.xlsx", range = cell_cols("C:P"))
df2 <- read_excel("./www/Dato donnees clean/position-sondes-global.xlsx")

# Convert comma to dot in numeric columns if necessary
df1$`Concentration chlore 2 (mg/L)` <- gsub(",", ".", df1$`Concentration chlore 2 (mg/L)`)
df1$`Concentration chlore 1 (mg/L)` <- gsub(",", ".", df1$`Concentration chlore 1 (mg/L)`)

# Add a decimal point to the latitude and longitude
df2$Latitude <- as.numeric(paste0(substr(df2$Latitude, 1, nchar(df2$Latitude)-6), ".", substr(df2$Latitude, nchar(df2$Latitude)-5, nchar(df2$Latitude))))
df2$Longitude <- as.numeric(paste0(substr(df2$Longitude, 1, nchar(df2$Longitude)-5), ".", substr(df2$Longitude, nchar(df2$Longitude)-4, nchar(df2$Longitude))))

df1 <- df1 %>% mutate(`Concentration chlore 2 (mg/L)` = as.numeric(`Concentration chlore 2 (mg/L)`),
                      `Concentration chlore 1 (mg/L)` = as.numeric(`Concentration chlore 1 (mg/L)`))

df <- merge(df1, df2, by = "RFID")

create_datatable <- function(df_filtered){
  
  data_table <- DT::datatable(df_filtered,
            options = list(
              columnDefs = list(
                list(width = '100px', targets = c(1,2,3)), # ajuster les largeurs des colonnes selon vos préférences
                list(width = '200px', targets = c(4,5,6))
              ),
              scrollX = TRUE)
  ) %>% formatStyle(
    'Moyenne_chlore',
    target = 'row',
    color = styleEqual(c(0), c('red'))
  )
  return(data_table)
}