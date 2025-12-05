options(download.file.method = "wininet") 
library("openxlsx", character.only = TRUE, options("openxlsx.dateFormat" = "mm/dd/yyyy"))
library("readxl")
library(lubridate)
st_mart2014_2017 = readxl::read_excel('../prediction_sulfate/input/obs_ST_MARTIN_VESUBIE_SAPC_2014_2017.xls')
st_mart2017_2020 = readxl::read_excel('../prediction_sulfate/input/obs_ST_MARTIN_VESUBIE_SAPC_mod.xls')
st_mart2021_2023 = readxl::read_excel('../prediction_sulfate/input/obs_ST_MARTIN_VESUBIE_OBS_2021-2023.xls')
st_mart2023_2025 = readxl::read_excel('../prediction_sulfate/input/obs_ST_MARTIN_VESUBIE_OBS_2025.xls')


meteo_st_martin = rbind(st_mart2014_2017,st_mart2017_2020)
meteo_st_martin = rbind(meteo_st_martin,st_mart2021_2023)
meteo_st_martin = rbind(meteo_st_martin,st_mart2023_2025)


library(tidyr)
library(dplyr)

# Exemple de dataframe

meteo_st_martin <- meteo_st_martin %>%
  separate(DATE, into = c("jour", "Heure"), sep = " : ")

# Optionnel : convertir les colonnes aux bons formats
meteo_st_martin$jour <- as.Date(meteo_st_martin$jour, format = "%Y/%m/%d")
meteo_st_martin$Heure <- as.integer(meteo_st_martin$Heure)

meteo_st_martin <- meteo_st_martin %>%
  group_by(jour) %>%
  summarise(St_martin = sum(RR1), .groups = "drop")

meteo_st_martin <- meteo_st_martin %>%
  mutate(
    jour = as.Date(jour),
    jour_mois = format(jour, "%m-%d")  # Extrait le jour et le mois
  )

# Étape 2 : Calcul de la moyenne par jour/mois
meteo_st_martin <- meteo_st_martin %>%
  group_by(jour_mois) %>%
  mutate(
    St_martin = ifelse(is.na(St_martin),
                       mean(St_martin, na.rm = TRUE),
                       St_martin)
  ) %>%
  ungroup() %>%
  select(-jour_mois)  # Nettoyage

write.csv(meteo_st_martin, "../prediction_sulfate/output/meteo_st_martin.csv", row.names = FALSE)
getwd() 

