

source("./Script/Kapta-PSV.R")

df1 <- donnees_sondes # Données KAPTA
df2 <- position_sondes # Position Kapta

# Transformation du dataset

# remplacement d'un virgule si nécessaire
df1$`Concentration chlore 2 (mg/L)` <- gsub(",", ".", df1$`Concentration chlore 2 (mg/L)`)
df1$`Concentration chlore 1 (mg/L)` <- gsub(",", ".", df1$`Concentration chlore 1 (mg/L)`)

# Ajout d'une virgule sur latitude et longitude
df2$Latitude <- as.numeric(paste0(substr(df2$Latitude, 1, nchar(df2$Latitude)-6), ".", substr(df2$Latitude, nchar(df2$Latitude)-5, nchar(df2$Latitude))))
df2$Longitude <- as.numeric(paste0(substr(df2$Longitude, 1, nchar(df2$Longitude)-5), ".", substr(df2$Longitude, nchar(df2$Longitude)-4, nchar(df2$Longitude))))

df1 <- df1 %>% mutate(`Concentration chlore 2 (mg/L)` = as.numeric(`Concentration chlore 2 (mg/L)`),
                      `Concentration chlore 1 (mg/L)` = as.numeric(`Concentration chlore 1 (mg/L)`))

# Merge les des datasets pour exploitation
df <- merge(df1, df2, by = "RFID")

# Formatage colonnes
df$DATE <- as.Date(df$DATE, format = "%d/%m/%Y")
df$YearMonth <- format(df$DATE, "%Y-%m")


# Fonction pour filtrer le datset
set_filtre <- function(debut, fin, seuil, choix, type_mesure, type_table){
  
  # Vérifie si debut et fin ne sont pas vide
  debut <- tryCatch(
    ymd(debut),
    error = function(e) NA
  )
  if (is.na(debut)) {
    debut <- as.POSIXct("2021-03-01") 
  } else {
    # Utilise variable
  }
  if (is.na(fin)) {
    fin <- as.POSIXct("2021-05-01") 
  } else {
    # Utilise variabel
  }
  
  
  req(type_mesure)
  
  # Cas des KAPTA
  if (type_mesure == "KAPTA"){
    
    # Filtre sur la période
    df_filtered <- df %>%
      filter(DATE >= debut & DATE <= fin) 
    
    # Cas du tableau des seuil
    if (type_table == "overview") {
      df_filtered$Moyenne_chlore <- (df_filtered$`Concentration chlore 1 (mg/L)` + df_filtered$`Concentration chlore 2 (mg/L)`) / 2
      req(choix)
      if (choix == "Inférieur") {
        df_filtered <- df_filtered[df_filtered$Moyenne_chlore < seuil,]
      } 
      else {
        df_filtered <- df_filtered[df_filtered$Moyenne_chlore > seuil,]
      }
    }
    
  # Calculer la moyenne des concentrations de chlore 1 et 2
  df_filtered$Moyenne_chlore <- (df_filtered$`Concentration chlore 1 (mg/L)` + df_filtered$`Concentration chlore 2 (mg/L)`) / 2
  
  }
  
  # Cas des PSV
  else {
    # Filtre sur la période
    df_filtered <- merged_data %>%
      filter(Date.de.prelevement >= debut & Date.de.prelevement <= fin, !is.na(`Cl2 libre (1398)`))
      
      if(type_table == "overview") {
    
        req(choix)
        if (choix == "Inférieur") {
          df_filtered <- df_filtered[df_filtered$`Cl2 libre (1398)` < seuil,]
        } 
        else {
          df_filtered <- df_filtered[df_filtered$`Cl2 libre (1398)` > seuil,]
        }
      }
  }
  # Retourne le dataset
  return(df_filtered)
}

# Fonction pour créer les tableaux de seuil
create_datatable <- function(debut, fin, seuil, choix, type_mesure){
  #Filtrage grâce à la fonction set_filtre
  df_filtered <- set_filtre(debut, fin, seuil, choix, type_mesure, "overview")
  
  # Cas des Kapta
  if(type_mesure == "KAPTA"){
    df_filtered <- df_filtered %>%
      dplyr::select(ENDPOINTREF.x, DATE, Moyenne_chlore) %>%  # Selectionne les colonnes nécessaire
      arrange(ENDPOINTREF.x) 
    
    data_table <- DT::datatable(df_filtered,  # Creation du tableau
                                options = list(
                                  scrollX = TRUE)
                                ) %>% 
      formatStyle(
      'Moyenne_chlore',
      target = 'row',
      color = styleEqual(c(0), c('red'))
      )
  }
  
  # Cas des PSV
  else {
    df_filtered <- df_filtered %>%
      dplyr::select(Numero, Nom, Sectorisat, Date.de.prelevement, `Cl2 libre (1398)`) %>% # Selectionne les colonnes nécessaire
      arrange(Sectorisat)

    data_table <- DT::datatable(df_filtered,
                                options = list(
                                  columnDefs = list(
                                    list(width = '25px', targets = c(0,1)), # Ajuste les largeurs des colonnes 
                                    list(width = '300px', targets = c(2,3,4)),
                                    list(width = '50px', targets = c(5)),
                                    list(targets = c(6), visible = FALSE)
                                    ),
                                  scrollX = TRUE)
                                ) %>% 
      formatStyle(
      'Cl2 libre (1398)',
      target = 'row',
      color = styleEqual(c(0), c('red'))
    )
  }
  
  # Retourne le tableau et le dataset filtrer
  return(list(table = data_table, set = df_filtered))
}

#Function pour les tableau de statistiques
create_stats_table <- function (debut, fin, seuil, choix, type_mesure){
  
  # Filtrage du dataset
  df_filtered <- set_filtre(debut, fin, seuil, choix, type_mesure, "stat")
  
  if(type_mesure == "KAPTA"){
    df_filtered <- df_filtered %>%
      dplyr::select(ENDPOINTREF.x, DATE, Moyenne_chlore) # Selectionne les colonnes nécessaire
    
    req(choix)
    if (choix == "Inférieur") { 
      df_stats <- df_filtered %>% 
        group_by(ENDPOINTREF.x) %>% 
        summarise(Total = n(),
                  `Nombre de dépassement` = sum(Moyenne_chlore < seuil)) %>%   # Calcule du nombre de dépassement de seuil
        mutate(`Pourcentage sur total` = round((`Nombre de dépassement` / Total) * 100,2)) %>%
        filter(`Nombre de dépassement` > 0) %>%
        arrange(desc(`Nombre de dépassement`))
    }
    else {
      df_stats <- df_filtered %>% 
        group_by(ENDPOINTREF.x) %>% 
        summarise(Total = n(),
                  `Nombre de dépassement` = sum(Moyenne_chlore > seuil)) %>%  # Calcule du nombre de dépassement de seuil
        mutate(`Pourcentage sur total` = round((`Nombre de dépassement` / Total) * 100,2)) %>%
        filter(`Nombre de dépassement` > 0) %>%
        arrange(desc(`Nombre de dépassement`))
    }
    
    stats_table <- DT::datatable(df_stats,   # Creation du tableau statistique
                                 options = list(
                                   scrollX = TRUE
                                   )
                                 )
  }
  
  # Cas des PSV
  else {
    df_filtered <- df_filtered %>%
      dplyr::select(Numero, Nom, Sectorisat, Date.de.prelevement, `Cl2 libre (1398)`)  # Selectionne les colonnes nécessaire
    
    req(choix)
    if (choix == "Inférieur"){
      df_stats <- df_filtered %>% 
        group_by(Sectorisat) %>% 
        summarise(Total = n(),
                  `Nombre de dépassement` = sum(`Cl2 libre (1398)` < seuil)) %>%
        mutate(`Pourcentage sur total` = round((`Nombre de dépassement` / Total) * 100,2)) %>%
        filter(`Nombre de dépassement` > 0) %>%
        arrange(desc(`Nombre de dépassement`))
    }
    else {
      df_stats <- df_filtered %>% 
        group_by(Sectorisat) %>% 
          summarise(Total = n(),
                    `Nombre de dépassement` = sum(`Cl2 libre (1398)` > seuil)) %>%
          mutate(`Pourcentage sur total` = round((`Nombre de dépassement` / Total) * 100,2)) %>%
          filter(`Nombre de dépassement` > 0) %>%
          arrange(desc(`Nombre de dépassement`))
    }
    stats_table <- DT::datatable(df_stats,  # Creation du tableau statistique
                                 options = list(
                                   columnDefs = list(
                                     list(targets = c(4), visible = FALSE)
                                     ), 
                                   scrollX = TRUE
                                   )
                                 )
  }
  
  # Retourne le tableau et le dataset filtré
  return(list(table = stats_table, set = df_stats))
}

# Fonction pour créer les graphiques
create_plot <- function(df_table, type_mesure, choix) {
    req(choix)
    req(type_mesure)
    if(type_mesure == "PSV"){
      # Try catch pour handle une error lors du changement de type de mesure (nécessite d'appuyer sur le bouton go pour reload les graphiques)
      tryCatch({
        df_table <- df_table %>%
          filter(Sectorisat == choix) # Filtrage sur le point de surveillance
        
        plot <- ggplot(df_table, aes(x = Date.de.prelevement, y = `Cl2 libre (1398)`)) +  # Création du graphiques
        stat_summary(fun = "mean", geom = "bar", fill = "purple") +
        coord_cartesian(ylim = c(0, max(df_table$`Cl2 libre (1398)`)+0.03)) +
        labs(x = "Date", y = "Concentration en chlore (mg/L)", title = "Concentration en chlore (mg/L)") +
        theme_minimal()
      
      plot <- ggplotly(plot, tooltip = c("x", "y"))  # Conversion en plotly pour plus de fonctionnalité
      
      }, error = function(e) {
        "Appuyer sur le bouton GO pour reload le graphique"
      })
    }
    # Cas des Kapta
    else {
      # Try catch pour handle une error lors du changement de type de mesure (nécessite d'appuyer sur le bouton go pour reload les graphiques)
      tryCatch({
        df_table <- df_table %>%
          filter(ENDPOINTREF.x == choix)
        
      plot <- ggplot(df_table, aes(x = DATE, y = Moyenne_chlore)) + # Création du graphiques
        stat_summary(fun = "mean", geom = "bar", fill = "purple") +
        labs(x = "Date", y = "Concentration en chlore (mg/L)", title = "Concentration en chlore (mg/L)") +
        theme_minimal() +
        coord_cartesian(ylim = c(0, max(df_table$Moyenne_chlore)+0.03))
      
      plot <- ggplotly(plot, tooltip = c("x", "y"))   # Conversion en plotly pour plus de fonctionnalité
      
      }, error = function(e) {
        "Appuyer sur le bouton GO pour reload le graphique"
      })
      
    }
    
    # Retourne le graphique
    return(plot)
}