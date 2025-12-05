# Script principal: Définition de l'architecture et logique du dashboard
options(download.file.method = "wininet")
# setwd("S:/DPH/07 - DATASCIENCE/2-visualisation_valorisation_donnees/Qualite/projet_dato/P4A_DATO-main/DATO_dashboard_app")
# Installation des packages nécessaire au fonctionnement du dashboard 

if (!require('shiny')) install.packages('shiny')
if (!require('shinyWidgets')) install.packages('shinyWidgets')
if (!require('sf')) install.packages('sf')
if (!require('sp')) install.packages('sp')
if (!require('leaflet')) install.packages('leaflet')
if (!require('readxl')) install.packages('readxl')
if (!require('dplyr')) install.packages('dplyr')
if (!require('dismo')) install.packages('dismo')
if (!require('lubridate')) install.packages('lubridate')
if (!require('DT')) install.packages('DT')
if (!require('lintr')) install.packages('lintr')
if (!require('tidyr')) install.packages('tidyr')
if (!require('htmltools')) install.packages('htmltools')
if (!require('ggplot2')) install.packages('ggplot2')
if (!require('plotly')) install.packages('plotly')
if (!require('shinyalert')) install.packages('shinyalert')

library(shiny)
library(shinyWidgets)
library(sf)
library(sp)
library(leaflet)
library(readxl)
library(dplyr)
library(dismo)
library(lubridate)
library(DT)
library(lintr)
library(tidyr)
library(htmltools)
library(ggplot2)
library(plotly)
library(shinyalert)

# Appel des scripts R, pour définir les graphiques et données existantes
source("./scripts/Carto_2.R")        # Carte leaflet
source("./scripts/Graphes.R")        # Graphiques de répartition
source("./scripts/Kapta-PSV.R")      # Graphiques généraux KAPTA / PSV
source("./scripts/Seuil_chlore.R")   # Graphiques et tableau étude de seuil
source("./scripts/Meteo.R")          # Graphique pluviométrique
source("./scripts/Seuil_chlore2.R")

# Initialisation de données globales existantes
donnees_psv <- psv_data

# Compter le nombre unique de numéros
nombre_numeros_uniques <- donnees_psv %>% 
  distinct(Numero) %>% 
  n_distinct()

donnees_kapta <- donnees_sondes

# Compter le nombre unique de Kapta
nombre_kaptas <- donnees_kapta %>% 
  distinct(ENDPOINTREF) %>% 
  n_distinct()

# Extraction de la date minimale et maximale des Kapta
date_min_kapta <- min(donnees_kapta$DATE)
date_max_kapta <- max(donnees_kapta$DATE)

# Extraction de la date minimale et maximale des PSV
date_min_psv <- min(donnees_psv$Date.de.prelevement)
date_max_psv <- max(donnees_psv$Date.de.prelevement)

# Inititalisation de variables globales
choices_secteur_psv <<- unique(position_PSV$Sectorisat)
choices_secteur_kapta <<- unique(donnees_sondes$ENDPOINTREF)

# =========================
# UI
# =========================

ui <- fluidPage(
  
  # Définition d'éléments CSS
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
    tags$style(
      HTML(
        "#well-panel-1 { min-height: 400px; }",
           "#well-panel-2 { min-height: 400px; }",
           ".dropdown-toggle::after { content: '\\f201'; font-family: 'Font Awesome 5 Free'; font-weight: 900; }",
           ".nav-tabs > li > a { font-size: 16px; font-family: 'Arial', sans-serif; }",
           ".my-button {
            display: inline-block;
            padding: 10px 20px;
            background-color: #337ab7;
            color: #fff;
            font-size: 16px;
            border-radius: 5px;
            border: none;
            cursor: pointer;
          }
          .my-button:hover {
            background-color: #0056b3;
            color: #fff;
          }
          .my-button:active {
            background-color: #337ab7;
            color: #fff;
          }"
        )
      )
    ),
  
  # Titre et logo
  splitLayout(
    cellWidths = c("86%", "14%"),
    titlePanel("DATO dashboard"),
    img(src = "Image/logo.jpg", width = "auto", height = "70px")
  ),
  
  # Barre de navigation principale
  navbarPage(
    "Menu",
             
    # =====================
    # 1. Page principale
    # =====================
             tabPanel(
               "Page principale",
               tabsetPanel(
                 id = "tabs",
                 br(),
        tabPanel(
          "Contextualisation & objectifs",
                          h2("Contextualisation & objectifs", style = "color: #337ab7;font-weight:bold;"),
                          sidebarLayout(
                            sidebarPanel(
                              id = "sidebar1",
                              tabsetPanel(
                tabPanel(
                  "Contextualisation",
                                         br(),
                                         includeHTML("./www/HTML/Contexte.html")
                                         ),
                tabPanel(
                  "Objectif",
                                         br(),
                                         includeHTML("./www/HTML/Objectifs.html")
                                )
                              )
                            ),
                            mainPanel(
                              tableOutput("tableau"),
                              img(src ="Image/Nice_zones_al.png", width = "auto", height = "600px")
            )
                          )
                          ),
                 
        # =====================
        # 2. Visualisation seuil
        # =====================
        tabPanel(
          "Visualisation seuil",
                          h2("Visualisation seuil", style = "color: #337ab7;font-weight:bold;"),
                         tabsetPanel(
            tabPanel(
              "Periode simple",
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              id = "sidebar2",
                              wellPanel(
                              h3("Filtre paramètres", align = "center", style = "color: #337ab7;"),
                              br(),
                              sliderInput("seuil_select", "Seuil de concentration de chlore:", 0.08, min = 0, max = 0.2, step = 0.01),
                              splitLayout(
                      cellWidths = c("50%", "50%"),
                      radioButtons("up_down", "Option de dépassement:",
                                            choices = c("Inférieur", "Supérieur"),                
                                            selected = "Inférieur"),
                      radioButtons("choix_mesure", "Type de sondes:",
                                            choices = c("KAPTA", "PSV"),
                                            selected = "KAPTA")
                                ),
                              splitLayout(
                      cellWidths = c("50%", "50%"),
                      dateInput("date_debut_3", "Choisissez la date début :", value = "2021-03-01"),
                      dateInput("date_fin_3", "Choisissez la date fin :", value = "2021-07-01")
                              ),
                              actionButton("goButton1", "Go", class = "my-button")
                  ),
                              h3("Graphique pluviométrique", align = "center", style = "color: #337ab7;"),
                              plotlyOutput("graphique_pluie2")
                ),
                            mainPanel(
                              tabsetPanel(
                    tabPanel("Données dépassement", br(), DT::dataTableOutput("data_table")),
                    tabPanel("Statistiques", br(), DT::dataTableOutput("stats_table"))
                                ),
                                         br(),
                  conditionalPanel(
                    condition = "input.choix_mesure == 'PSV'",
                                               uiOutput("selectInputUI1"),
                    plotlyOutput("PSV_plot_output2")
                              ),
                  conditionalPanel(
                    condition = "input.choix_mesure == 'KAPTA'",
                                               uiOutput("selectInputUI2"),
                    plotlyOutput("kapta_plot_output2")
                  )
                )
              )
            ),
            
            tabPanel(
              "Periode flexible",
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("seuil", "Seuil:", min = 0, max = 1, value = 0.5),
                              radioButtons("up_down2", "Options de dépassement:",     
                                           choices = c("Inférieur", "Supérieur"),
                                           selected = "Inférieur"),
                              selectInput("mois", "Mois:", choices = unique(df3$Mois), selected = mois_precedent, multiple = TRUE, width = "50%"),
                              selectInput("annees", "Années:", choices = unique(df3$Annees), selected = annee_actuelle, multiple = TRUE, width = "50%"),
                              selectInput("secteur", "Sélectionnez un secteur :", choices = c("Tous les secteurs", secteurs_disponibles), selected = "Tous les secteurs", width = "50%"),
                  selectInput("recherche", "Numéro de recherche:", choices = NULL, multiple = TRUE,
                              selected = unique(donnees_avec_secteur$Numero), width = "50%")
                              ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Résultats", tableOutput("resultats")),
                                tabPanel("Occurrences", 
                                       p("Certains numéros peuvent avoir plusieurs occurrences selon le mois et l'année. Pour plus de précision, veuillez choisir un numéro."),
                                       tableOutput("occurrences"))
                            )
                )
                            )
            )
                         )
                 ),
                 
        # =====================
        # 3. Visualisation globale (seuil)
        # =====================
        tabPanel(
          "Visualisation globale",
                          h2("Visualisation globale", style = "color: #337ab7;font-weight:bold;"),
                          sidebarLayout(
                            sidebarPanel(
                              id = "sidebar3",
                              wellPanel(
                                h3("Filtre paramètres",align = "center", style = "color: #337ab7;"),
                                br(),
                                splitLayout(
                                  cellWidths = c("50%", "50%"),
                  dateInput("Date_Début", "Choisissez la date début :", value = "2021-03-01"),
                  dateInput("Date_Fin", "Choisissez la date fin :", value = "2021-07-01")
                                  ),
                                actionButton("goButton2", "Go", class = "my-button")
                                ),
                            h4("Graphes de Répartition" ,align = "center", style = "color: #337ab7;"),
                            dropdownButton(
                              circle = TRUE, 
                              status = "primary",
                              width = "1000px",
                              tooltip = tooltipOptions(title = "Cliquez pour afficher les graphiques"),
                              h4("Graphes de Répartition", style = "color: #337ab7;"),
                              p("Ces graphique permettent d'analyser la répartition de toutes les mesures effectuées (sondes et PSV) sur la période selectionné plus haut.\n Ils permettent aussi d'obtenir quelques données statistiques globales telles que les quartiles ou la médiane", align = "center"),
                              br(),
                              splitLayout(
                                cellWidths = c("50%", "50%"),
                  plotlyOutput("histo_chlore_output"),
                  plotlyOutput("box_chlore_output")
                              )
              )
                            ),
                            mainPanel(
                                  h3("Carte des canalisations", align = "center", style = "color: #337ab7;"),
              leafletOutput("carte_output", height = "800px")
            )
          )
        ),
        
        # =====================
        # 4. Visualisation Kapta/PSV
        # =====================
        tabPanel(
          "Visualisation Kapta/PSV",
                          h2("Visualisation Kapta/PSV", style = "color: #337ab7;font-weight:bold;"),
                          sidebarLayout(
                            sidebarPanel(
                              id = "sidebar4",
                              wellPanel(
                                h3("Filtre paramètres", align = "center", style = "color: #337ab7;"),
                                br(),
                                splitLayout(
                                  cellWidths = c("50%", "50%"),
                                    dateInput("Date_Début_2", "Choisissez la date début :", value = "2021-03-01"),
                                    dateInput("Date_Fin_2", "Choisissez la date fin :", value = "2021-07-01")
                                  ),
                conditionalPanel(
                  condition = "input.tabs1 == 'PSV'",
                                                 selectInput(
                                                   inputId = "zone_select_psv",
                                                   label = " Selectionnez une zone:",
                                                   choices = choices_secteur_psv,
                                                   width = "100%"
                                                 )
                                                 ),
                conditionalPanel(
                  condition = "input.tabs1 == 'KAPTA'",
                                                 selectInput(
                                                   inputId = "zone_select_kapta",
                                                   label = " Selectionnez une zone:",
                                                   choices = choices_secteur_kapta,
                                                   width = "50%"
                                                 )
                                                 ),
                                actionButton("goButton3", "Go", class = "my-button")
                                ),
                                h3("Graphique pluviométrique", align = "center", style = "color: #337ab7;"),
                                plotlyOutput("graphique_pluie1")
            ),
            mainPanel(
              tabsetPanel(
                id = "tabs1",
                tabPanel("PSV", br(), plotlyOutput("PSV_plot_output")),
                tabPanel("KAPTA", br(), plotlyOutput("kapta_plot_output"))
              )
            )
          )
        ),
        
        # =====================
        # 5. Visualisation Globale (Venuja)
        # =====================
                          tabPanel(
                            "Visualisation Globale",
                            fluidPage(
                              h3("Visualisation Globale", style = "font-weight:bold; margin-bottom:20px;"),
                              wellPanel(
                                h4("Sélection des sondes et période", style = "color:#337ab7;"),
                                fluidRow(
                                  column(4, dateInput("date_start_global", "Date début", value = "2019-01-01",
                                        min = "2019-01-01", max = "2021-06-16")),
                                  column(4, dateInput("date_end_global", "Date fin", value = "2021-06-16",
                                        min = "2019-01-01", max = "2021-06-16")),
                                  column(4, h5(textOutput("selected_sondes_count"), style = "margin-top:35px;"))
                                )
                              ),
                            wellPanel(
                              h4("Variables à afficher", style = "color:#337ab7;"),
                                checkboxGroupInput(
                                  "variables_kapta",
                                  "Kapta sondes :",
                                  choices = c(
                                    "Chlore 1 (mg/L)" = "chlore1",
                                    "Chlore 2 (mg/L)" = "chlore2",
                                    "Pression (Bar) × 0.1" = "pression",
                                    "Température (°C) / 100" = "temperature"
                                  ),
                                  selected = c("chlore1", "chlore2", "pression", "temperature")
                                ),
                                checkboxGroupInput(
                                  "variables_psv",
                                  "PSV sondes :",
                                  choices = c(
                                    "Conductivité (µS/cm)" = "conductivite",
                                    "COT (mg/L)" = "cot"
                                  ),
                                  selected = c("conductivite")
                              )
                            ),
                              wellPanel(
                                h4("Localisation des sondes", style = "color:#337ab7;"),
                                leafletOutput("global_map", height = 400),
                                tags$div(
                                  style = "text-align:center; margin-top:10px;",
                                  "Zone de Nice – Carte interactive des sondes"
                                )
                              ),
                              wellPanel(
                                h5("Sélectionnez jusqu'à 4 sondes sur la carte pour afficher les graphiques",
                                  style = "text-align:center; margin-bottom:20px;"),
                                uiOutput("global_graphs")
                              )
                            )
                          ),

        # =====================
        # 6. Statistiques Chlore (Venuja)
        # =====================
                          tabPanel(
                            "Statistiques Chlore",
                            fluidPage(
                              h3("Monitoring Qualité de l'Eau"),
                              p("Gestion des sondes Kapta et PSV"),
                              fluidRow(
                                column(3, numericInput("chlore_threshold", "Seuil de chlore (mg/L):", value = 0.3, min = 0, step = 0.01)),
                                column(3, dateInput("date_start", "Date début")),
                                column(3, dateInput("date_end", "Date fin")),
                                column(3, selectInput("threshold_filter", "Afficher :", 
                                                      choices = c("Tous les résultats", 
                                                                  "Seulement au-dessus du seuil", 
                                                                  "Seulement en dessous du seuil")))
                              ),
                              h4("Localisation des dépassements de seuil"),
                              leafletOutput("chlore_map", height = 400),
                              br(),
                              h4("Top 5 des points avec le plus de dépassements"),
                              uiOutput("top5_charts")
                            )
        ),

# =========================
# SERVER
# =========================

  server <- function(input, output, session) {
    
  # -----------------------
  # Tableau de synthèse (page principale)
  # -----------------------
    output$tableau <- renderTable({
    data.frame(
        "Nombre de Kaptas" = nombre_kaptas,
        "Plage temporelle pour les données Kapta" = paste(date_min_kapta, "  -  ", date_max_kapta),
        "Nombre de PSV" = nombre_numeros_uniques,
        "Plage temporelle pour les données PSV" = paste(date_min_psv, "  -  ", date_max_psv)
      )
    })
    
  # ======================
  # Visualisation seuil - GO 1
  # ======================
    observeEvent(input$goButton1, {
      tryCatch({
        selected_input_dDébut3 <- input$date_debut_3
      selected_input_dFin3   <- input$date_fin_3
      selected_input_seuil   <- input$seuil_select
      selected_input_upDown  <- input$up_down
      selected_input_mesure  <- input$choix_mesure
        
      if (input$date_debut_3 >= input$date_fin_3) {
        stop("La date début doit être avant la date fin")
        }
        
        output$selectInputUI1 <- renderUI({
        div(
          style = "justify-content: center;align-items: center",
              selectInput(
                inputId = "zone_select_psv2",
                label = " Selectionnez un point de surveillance:",
                choices =  NULL,
                width = "50%"
                )
              )
      })
        
        output$selectInputUI2 <- renderUI({
        div(
          style = "justify-content: center;align-items: center",
            selectInput(
              inputId = "zone_select_kapta2",
              label = " Selectionnez une sonde kapta:",
              choices =  NULL,
              width = "50%"
              )
            )
      })
      
      output_seuil <- create_datatable(
        selected_input_dDébut3, selected_input_dFin3,
        selected_input_seuil, selected_input_upDown,
        selected_input_mesure
      )
      output_stat <- create_stats_table(
        selected_input_dDébut3, selected_input_dFin3,
        selected_input_seuil, selected_input_upDown,
        selected_input_mesure
      )
      
      req(output_seuil)
      
      output$data_table  <- DT::renderDataTable(output_seuil$table)
      output$stats_table <- DT::renderDataTable(output_stat$table)
      output$graphique_pluie2 <- renderPlotly(
        create_plot_meteo(selected_input_dDébut3, selected_input_dFin3)
      )

      updateSelectInput(session, "zone_select_psv2",
                        choices = unique(output_seuil$set$Sectorisat))
      updateSelectInput(session, "zone_select_kapta2",
                        choices = unique(output_seuil$set$ENDPOINTREF.x))
      
      output$kapta_plot_output2 <- renderPlotly(
        create_plot(output_seuil$set, "KAPTA", input$zone_select_kapta2)
      )
      output$PSV_plot_output2 <- renderPlotly(
        create_plot(output_seuil$set, "PSV", input$zone_select_psv2)
      )
      
    }, error = function(err) {
        shinyalert(text = "La date début doit être avant la date fin")
    })
  })
    
  # ======================
  # Visualisation globale - GO 2
  # ======================
    observeEvent(input$goButton2, {
      tryCatch({
        selected_input_dDébut <- input$Date_Début
      selected_input_dFin   <- input$Date_Fin
        
      if (input$Date_Début >= input$Date_Fin) {
          stop("La date début doit être avant la date fin")
        }
        
      output$histo_chlore_output <- renderPlotly(
        create_histo(selected_input_dDébut, selected_input_dFin)
      )
      output$box_chlore_output <- renderPlotly(
        create_box(selected_input_dDébut, selected_input_dFin)
      )
      output$carte_output <- renderLeaflet({
        create_leafMap(selected_input_dDébut, selected_input_dFin)
      })
        
    }, error = function(err) {
          shinyalert(text = "La date début doit être avant la date fin")
    })
  })
    
  # ======================
  # Visualisation Kapta/PSV - GO 3
  # ======================
    observeEvent(input$goButton3, {
      tryCatch({
      selected_input_dDébut2      <- input$Date_Début_2
      selected_input_dFin2        <- input$Date_Fin_2
      selected_input_zone_kapta   <- input$zone_select_kapta
      selected_input_zone_psv     <- input$zone_select_psv
      
      if (input$Date_Début_2 >= input$Date_Fin_2) {
          stop("La date début doit être avant la date fin")
        }
      
      output$graphique_pluie1 <- renderPlotly(
        create_plot_meteo(selected_input_dDébut2, selected_input_dFin2)
      )
      output$kapta_plot_output <- renderPlotly(
        create_kapta_plot(selected_input_dDébut2, selected_input_dFin2, selected_input_zone_kapta)
      )
      output$PSV_plot_output <- renderPlotly(
        create_PSV_plot(selected_input_dDébut2, selected_input_dFin2, selected_input_zone_psv)
      )
        
    }, error = function(err) {
        shinyalert(text = "La date début doit être avant la date fin")
    })
  })
  
  # ======================
  # Période flexible - seuil chlore
  # ======================
    resultats <- reactive({
    seuil     <- input$seuil
    mois      <- input$mois
    annees    <- input$annees
    secteur   <- input$secteur
      recherche <- input$recherche
    up_down   <- input$up_down2
      subset_data <- create_seuil_table(seuil, mois, annees, secteur, recherche, up_down)
    subset_data
    })
    
    output$resultats <- renderTable({
      resultats()
    })
    
    observe({
      numeros_disponibles <- unique(donnees_avec_secteur$Numero)
      updateSelectInput(session, "recherche", choices = numeros_disponibles)
    })
  
    tryCatch({
    occurrences <- reactive({
      new_data <- resultats()
      create_occurence_table(new_data)
      })
    
    output$occurrences <- renderTable({
      occurrences()[[1]]
    })
    }, error = function(err) {
      "Pas de données pour ces paramètres"
  })
  
  # ======================
  # Statistiques chlore & visualisation globale (Venuja)
  # ======================
 
    reactive_data <- reactive({
    df <- psv_data
    df <- df %>% filter(Unite %in% c("mg(Cl2)/L (165)"))
    
    if (!is.null(input$date_start) && !is.null(input$date_end)) {
      df <- df[df$Date.de.prelevement >= input$date_start &
                 df$Date.de.prelevement <= input$date_end, ]
    }
    
    df$above <- df$Resultat > input$chlore_threshold
    
    if (input$threshold_filter == "Seulement au-dessus du seuil") {
      df <- df[df$above == TRUE, ]
    } else if (input$threshold_filter == "Seulement en dessous du seuil") {
      df <- df[df$above == FALSE, ]
    }
    
    df
  })

  output$top5_charts <- renderUI({
    df <- reactive_data()
    if (is.null(df) || nrow(df) == 0) return(h4("Aucune donnée disponible pour ces filtres."))

    top_sensors <- df %>%
      group_by(Numero) %>%
      summarise(depassements = sum(Resultat > input$chlore_threshold)) %>%
      arrange(desc(depassements)) %>%
      head(5)

    plot_list <- lapply(1:nrow(top_sensors), function(i) {
      plotlyOutput(paste0("plot_", i))
    })

    do.call(tagList, plot_list)
  })

  output$chlore_map <- renderLeaflet({
    df <- reactive_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    df_summary <- df %>%
      group_by(Numero) %>%
      summarise(
        depassements = sum(Resultat > input$chlore_threshold),
        moyenne = mean(Resultat, na.rm = TRUE)
      ) %>%
      filter(!is.na(Numero))
    
    if (exists("position_PSV")) {
      df_summary <- df_summary %>%
        left_join(
          position_PSV %>%
                    st_drop_geometry() %>%
                    select(Numero, XWGS84, YWGS84),
          by = "Numero"
        )
    } else {
      return(h4("Erreur : Les coordonnées des sondes (position_PSV) ne sont pas disponibles."))
    }
    
    df_summary <- df_summary %>% filter(!is.na(XWGS84), !is.na(YWGS84))
    if (nrow(df_summary) == 0)
      return(h4("Aucune position disponible pour les sondes sélectionnées."))

    df_summary$color <- cut(
      df_summary$depassements,
                            breaks = c(-Inf, 5, 10, 20, Inf),
      labels = c("green", "yellow", "orange", "red")
    )
    
    leaflet(df_summary) %>%
      addTiles() %>%
      addCircleMarkers(
        ~XWGS84, ~YWGS84,
                      color = ~as.character(color),
                      label = ~paste0("Sonde ", Numero, ": ", depassements, " dépassements"),
        radius = 8, fillOpacity = 0.8
      ) %>%
      addLegend(
        "bottomright",
                colors = c("green", "yellow", "orange", "red"),
                labels = c("≤ 5", "6–10", "11–20", "> 20"),
        title = "Dépassements"
      )
  })

  observe({
    df <- reactive_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)

    top_sensors <- df %>%
      group_by(Numero) %>%
      summarise(depassements = sum(Resultat > input$chlore_threshold)) %>%
      arrange(desc(depassements)) %>%
      head(5)

    for (i in 1:nrow(top_sensors)) {
      local({
        my_i <- i
        sensor_name <- top_sensors$Numero[my_i]
        
       output[[paste0("plot_", my_i)]] <- renderPlotly({
          df_sensor <- df[df$Numero == sensor_name, ]
          if (nrow(df_sensor) == 0) return(NULL)

          p <- ggplot(
            df_sensor,
            aes(
              x = Date.de.prelevement,
              y = Resultat,
              group = 1,
              text = paste0(
                "<b>Date :</b> ", format(Date.de.prelevement, "%d/%m/%Y"),
                "<br><b>Chlore (mg/L) :</b> ", round(Resultat, 3),
                "<br><b>Seuil :</b> ", input$chlore_threshold
              )
            )
          ) +
            geom_line(color = "#007bff", linewidth = 0.8, na.rm = TRUE) +
              geom_point(color = "#007bff", size = 2, na.rm = TRUE) +
              geom_hline(yintercept = input$chlore_threshold, color = "red", linetype = "dashed") +
              labs(
                title = paste("Sonde", sensor_name),
                y = "Chlore (mg/L)",
                x = "Date"
              ) +
              theme_minimal() +
              theme(plot.title = element_text(face = "bold"))

            ggplotly(p, tooltip = "text") %>%
              layout(hoverlabel = list(bgcolor = "white", font = list(color = "black")))
        })
      })
    }
  })

  # ======================
  # Visualisation globale Venuja (carte + séries)
  # ======================

  selected_sondes <- reactiveVal(character(0))

  output$global_map <- renderLeaflet({
    if (exists("position_PSV")) {
      psv_points <- tryCatch({
        position_PSV %>%
          st_drop_geometry() %>%
          mutate(
            Type = "PSV",
            ID = as.character(Numero),
            Lon = if ("XWGS84" %in% names(.)) XWGS84 else if ("Longitude" %in% names(.)) Longitude else NA,
            Lat = if ("YWGS84" %in% names(.)) YWGS84 else if ("Latitude" %in% names(.)) Latitude else NA
          )
      }, error = function(e) NULL)
    } else {
      psv_points <- NULL
    }

    if (exists("position_sondes")) {
      kapta_points <- tryCatch({
        position_sondes %>%
          mutate(
            Type = "Kapta",
            ID = as.character(ENDPOINTREF),
            Lon = if ("Longitude" %in% names(.)) Longitude else if ("XWGS84" %in% names(.)) XWGS84 else NA,
            Lat = if ("Latitude" %in% names(.)) Latitude else if ("YWGS84" %in% names(.)) YWGS84 else NA
          )
      }, error = function(e) NULL)
    } else {
      kapta_points <- NULL
    }

    sondes_all <- bind_rows(psv_points, kapta_points)
    sondes_all <- sondes_all %>% filter(!is.na(Lon), !is.na(Lat))

    if (nrow(sondes_all) == 0) {
      return(
        leaflet() %>%
          addTiles() %>%
          addPopups(7.25, 43.7, "Aucune sonde à afficher.")
      )
    }

    leaflet(sondes_all) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Lon, lat = ~Lat,
        color = ~ifelse(Type == "Kapta", "#007bff", "#00b894"),
        label = ~paste0("Sonde ", Type, " ", ID),
        layerId = ~paste0(Type, "_", ID),
        radius = 7, fillOpacity = 0.8
      ) %>%
      addLegend(
        "bottomright",
                colors = c("#007bff", "#00b894"),
                labels = c("Kapta", "PSV"),
        title = "Type de sonde"
      )
  })

  observeEvent(input$global_map_marker_click, {
    click <- input$global_map_marker_click
    id    <- click$id
    current <- selected_sondes()

    if (id %in% current) {
      current <- setdiff(current, id)
    } else {
      if (length(current) >= 4) {
        current <- c(tail(current, 3), id)
      } else {
        current <- c(current, id)
      }
    }
    selected_sondes(current)
  })

  output$selected_sondes_count <- renderText({
    paste0(length(selected_sondes()), " / 4 sondes sélectionnées")
  })

  output$global_graphs <- renderUI({
    sondes <- selected_sondes()
    if (length(sondes) == 0)
      return(h5("Sélectionnez jusqu'à 4 sondes sur la carte pour afficher les graphiques.",
                style = "text-align:center;"))

    plots <- lapply(sondes, function(id) {
      plotlyOutput(paste0("plot_global_", id), height = 300)
    })
    do.call(tagList, plots)
  })

  observe({
    sondes <- selected_sondes()
    if (length(sondes) == 0) return()

    for (id in sondes) {
      local({
        my_id <- id
        output[[paste0("plot_global_", my_id)]] <- renderPlotly({
          type <- substr(my_id, 1, 1)
          num  <- as.numeric(gsub("^[A-Za-z_]+", "", my_id))

          # KAPTA
          if (type == "K") {
            df <- donnees_sondes %>%
              filter(ENDPOINTREF == as.numeric(num)) %>%
              filter(
                DATEREF >= as.POSIXct(input$date_start_global),
                DATEREF <= as.POSIXct(input$date_end_global)
              )

            if (nrow(df) == 0) {
              return(
                plotly_empty(type = "scatter") %>%
                  layout(title = paste("Sonde", my_id, "- aucune donnée"))
              )
            }

            p <- ggplot(df, aes(x = DATEREF))

            if ("chlore1" %in% input$variables_kapta)
              p <- p + geom_line(aes(y = `Concentration chlore 1 (mg/L)`, color = "Chlore 1 (mg/L)"), linewidth = 1)
            if ("chlore2" %in% input$variables_kapta)
              p <- p + geom_line(aes(y = `Concentration chlore 2 (mg/L)`, color = "Chlore 2 (mg/L)"), linewidth = 1)
            if ("pression" %in% input$variables_kapta)
              p <- p + geom_line(aes(y = `Pression (Bar)` * 0.1, color = "Pression (Bar) × 0.1"), linetype = "dotted")
            if ("temperature" %in% input$variables_kapta)
              p <- p + geom_line(aes(y = `T° (°C)` / 100, color = "Température (°C) / 100"), linetype = "dashed")

            p <- p +
              scale_color_manual(values = c(
                "Chlore 1 (mg/L)" = "#007bff",
                "Chlore 2 (mg/L)" = "#74b9ff",
                "Pression (Bar) × 0.1" = "#55efc4",
                "Température (°C) / 100" = "#e17055"
              )) +
              labs(
                title = paste("Sonde Kapta", my_id),
                y = "Valeurs",
                x = "Date",
                color = ""
              ) +
              theme_minimal() +
              theme(plot.title = element_text(face = "bold"))

            return(ggplotly(p))
          }

          # PSV
          if (type == "P") {
            df <- psv_data %>%
              filter(Numero == as.numeric(num)) %>%
              filter(
                as.Date(Date.de.prelevement) >= as.Date(input$date_start_global),
                as.Date(Date.de.prelevement) <= as.Date(input$date_end_global)
              )

            if (nrow(df) == 0) {
              return(
                plotly_empty(type = "scatter") %>%
                  layout(title = paste("Sonde", my_id, "- aucune donnée"))
              )
            }

            df_filtered <- df %>%
              filter(Parametre %in% c("Conductiv. (1303)", "C Orga (1841)")) %>%
              select(Date.de.prelevement, Parametre, Resultat) %>%
              tidyr::pivot_wider(names_from = Parametre, values_from = Resultat)

            names(df_filtered) <- gsub("Conductiv\\. \\(1303\\)", "Conductivité (µS/cm)", names(df_filtered))
            names(df_filtered) <- gsub("C Orga \\(1841\\)", "COT (mg/L)", names(df_filtered))

            if (!("Conductivité (µS/cm)" %in% names(df_filtered)) &&
                !("COT (mg/L)" %in% names(df_filtered))) {
              return(
                plotly_empty(type = "scatter") %>%
                  layout(title = paste("Sonde", my_id, "- Conductivité / COT absentes"))
              )
            }

            p <- ggplot(df_filtered, aes(x = Date.de.prelevement))

            if ("conductivite" %in% input$variables_psv &&
                "Conductivité (µS/cm)" %in% names(df_filtered)) {
              p <- p + geom_line(aes(y = `Conductivité (µS/cm)`, color = "Conductivité (µS/cm)"), linewidth = 1)
            }
            if ("cot" %in% input$variables_psv &&
                "COT (mg/L)" %in% names(df_filtered)) {
              p <- p + geom_line(aes(y = `COT (mg/L)`, color = "COT (mg/L)"), linewidth = 1)
            }

            available <- names(df_filtered)[names(df_filtered) != "Date.de.prelevement"]

            p <- p +
              scale_color_manual(values = c(
                "Conductivité (µS/cm)" = "#0984e3",
                "COT (mg/L)" = "#d63031"
              )) +
              labs(
                title = paste("Sonde", my_id),
                subtitle = paste("Paramètres disponibles :", paste(available, collapse = ", ")),
                y = "Valeurs",
                x = "Date",
                color = ""
              ) +
              theme_minimal() +
              theme(plot.title = element_text(face = "bold"))

            tryCatch({
              ggplotly(p) %>%
                layout(
                  title = list(
                    text = paste0(
                      "<b>", paste("Sonde", my_id), "</b><br>",
                      "<span style='font-size:12px; color:gray;'>Paramètres disponibles : ",
                      paste(available, collapse = ", "), "</span>"
                    )
                  )
                )
            }, error = function(e) {
              plotly_empty(type = "scatter") %>%
                layout(title = paste("Sonde", my_id, "- erreur d'affichage Plotly"))
            })
          }
        })
      })
    }
  })

 
}

# Lancer l'application
shinyApp(ui, server)
