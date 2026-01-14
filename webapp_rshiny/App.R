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



# Appel des scripts R, pour definir les graphiques

source("./scripts/Carto_2.R")        # Carte leaflet
source("./scripts/Graphes.R")        # Graphiques de répartition
source("./scripts/Kapta-PSV.R")      # Graphiques généraux KAPTA / PSV
source("./scripts/Seuil_chlore.R")   # Graphiques et tableau etude de seuil
source("./scripts/Meteo.R")          # Graphique pluviométrique
source("./scripts/Seuil_chlore2.R")
source("./scripts/COT_Analysis.R")   # analise du carbone organique
source("./scripts/Sulfate_Prediction.R") # script de prediction sulfate

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

# Initialistion de la structuer du dashboard

ui <- fluidPage(
  
  # Définition d'éléments CSS
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
    tags$style(
      HTML("#well-panel-1 { min-height: 400px; }",
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
  
  # Initialisation du titre et logo
  
  splitLayout(
    cellWidths = c("86%", "14%"),
    titlePanel("DATO dashboard"),
    img(src = "Image/logo.jpg",
        width = "auto",
        height = "70px"),
  ),
  
  # Barre de navigation principale
  navbarPage("Menu", 
             
             # Premiere tab
             tabPanel(
               "Page principale",
               
               # Définition des tabs secondaire
               tabsetPanel(
                 id = "tabs",
                 br(),
                 tabPanel("Contextualisation & objectifs",
                          h2("Contextualisation & objectifs", style = "color: #337ab7;font-weight:bold;"),
                          
                          # Définition d'une sidebar
                          sidebarLayout(
                            sidebarPanel(
                              id = "sidebar1",
                              includeHTML("./www/HTML/Intro.html")

                              
                            ),
                            
                            # Définition du main panel
                            mainPanel(
                              
                              # Tableau
                              tableOutput("tableau"),
                              
                              # Image centrée
                              img(src ="Image/Nice_zones_al.png", width = "auto", height = "600px")
                            ),
                          )
                          ),                       
      # --------visualisation globale-venuja----------
                 tabPanel("Visualisation Globale",
                    fluidPage(
                      h3("Visualisation Globale", style = "font-weight:bold; margin-bottom:20px;"),

                      # --- Section: Sélection période ---
                      wellPanel(
                        h4("Sélection des sondes et période", style = "color:#337ab7;"),
                        fluidRow(
                          column(4, dateInput("date_start_global", "Date début", value = "2019-01-01")),
                          column(4, dateInput("date_end_global", "Date fin", value = "2021-06-16")),
                          column(4, h5(textOutput("selected_sondes_count"), style = "margin-top:35px;"))
                        )
                      ),

                      # --- Section: Variables ---
                      wellPanel(
                        h4("Variables à afficher", style = "color:#337ab7;"),
                        checkboxGroupInput("variables_kapta", "Kapta sondes :", 
                                          choices = c("Chlore 1 (mg/L)" = "chlore1",
                                                      "Chlore 2 (mg/L)" = "chlore2",
                                                      "Température (°C)" = "temperature"),
                                          selected = c("chlore1", "chlore2", "temperature")),
                        checkboxGroupInput("variables_psv", "PSV sondes :",
                                          choices = c("Conductivité (µS/cm)" = "conductivite",
                                                      "COT (mg/L)" = "cot"),
                                          selected = c("conductivite"))
                      ),

                       # --- Section: Carte + Menu déroulant ---
                      wellPanel(
                        h4("Localisation et sélection des sondes", style = "color:#337ab7;"),

                        selectizeInput(
                          "sensor_selector",
                          "Sélectionnez des sondes (✕ pour retirer une sonde) :",
                          choices = NULL,
                          multiple = TRUE,
                          options = list(
                            plugins = list("remove_button"),
                            placeholder = "Choisissez une ou plusieurs sondes"
                          )
                        ),

                        fluidRow(
                          column(
                            6,
                            actionButton(
                              "show_selected_sensors",
                              "Afficher les sondes sélectionnées",
                              icon = icon("chart-line"),
                              class = "btn-primary"
                            )
                          ),
                          column(
                            6,
                            actionButton(
                              "show_all_sensors",
                              "Afficher toutes les sondes",
                              icon = icon("layer-group"),
                              class = "btn-warning"
                            )
                          ),

                          column(
                              4,
                              actionButton(
                                "clear_selected_sensors",
                                "Tout désélectionner",
                                icon = icon("trash"),
                                class = "btn-danger"
                              )
                            ),

                        ),

                        br(),

                        leafletOutput("global_map", height = 400),

                        tags$div(
                          "Zone de Nice – Carte interactive des sondes",
                          style = "text-align:center; margin-top:10px;"
                        )
                      ),


                      # --- Section: Graphiques ---
                      wellPanel(
                        h5("Sélectionnez des sondes puis cliquez sur « Afficher les sondes sélectionnées » ou affichez toutes les sondes.",
                          style = "text-align:center; margin-bottom:20px;"),
                        uiOutput("global_graphs")
                      )
                    )
                  ),

      # --------Statistiques Chlore-----------
                 tabPanel("Statistiques Chlore",
                            
                            fluidPage(
                              h3("Monitoring Qualité de l'Eau"),
                              p("Gestion des sondes Kapta et PSV"),
                              
                              # ---- Parameters ----
                              fluidRow(
                                column(3, numericInput("chlore_threshold", "Seuil de chlore (mg/L):", value = 0.3, min = 0, step = 0.01)),
                                column(3, dateInput("date_start", "Date début")),
                                column(3, dateInput("date_end", "Date fin")),
                                column(
                                    3,
                                    radioButtons(
                                      "exceed_direction",
                                      "Type de dépassement :",
                                      choices = c(
                                        "Supérieur au seuil" = "above",
                                        "Inférieur au seuil" = "below"
                                      ),
                                      selected = "above"
                                    )
                                  ),

                                  column(
                                      3,
                                      radioButtons(
                                        "metric_type",
                                        "Critère d'affichage :",
                                        choices = c(
                                          "Nombre de dépassements" = "count",
                                          "Pourcentage de dépassements" = "percent"
                                        ),
                                        selected = "percent"
                                      )
                                    )

                              ),
                              
                              # ---- Map section ----
                              h4("Localisation des dépassements de seuil"),
                              leafletOutput("chlore_map", height = 400),
                              
                              br(),
                              uiOutput("top5_charts")
                            )
                          ), # Fin de page Statictiques chlore


                  # --------Analyse COT-----------
                 tabPanel("Carbone Organique Total",
                  
                  fluidPage(
                    h2("Analyse du Carbone Organique Total (COT)", 
                      style = "color: #337ab7; font-weight: bold;"),
                    
                    p("Observer l'évolution du chlore en fonction de la ressource et de la météo qui affecte la quantité de matière organique."),
                    
                    # --- Filter Panel ---
                    wellPanel(
                      h4("Sélection de la période", style = "color: #337ab7;"),
                      fluidRow(
                        column(4, 
                              dateInput("cot_date_start", "Date début:", 
                                        value = as.Date("2021-03-01"))
                        ),
                        column(4, 
                              dateInput("cot_date_end", "Date fin:", 
                                        value = as.Date("2021-07-01"))
                        ),
                        column(4, 
                              br(),
                              actionButton("cot_go_button", "Actualiser", 
                                          class = "my-button",
                                          style = "margin-top: 5px;")
                        )
                      ),
                      div(style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
                          tags$b("Seuil d'alerte COT:"), 
                          tags$span("2 mg/L", style = "color: red; font-weight: bold; margin-left: 10px;"),
                      )
                    ),
                    
                    # --- Alert Panel (dynamic) ---
                    uiOutput("cot_alert_ui"),
                    
                    # --- Synoptic Diagram ---
                    wellPanel(
                      h4("Synoptique des points de mesure", style = "color: #337ab7;"),
                      p("Emplacement des capteurs sur le réseau", style = "color: gray;"),
                      div(style = "text-align: center; padding: 20px;",
                          # Si vous avez l'image prête, décommentez la ligne ci-dessous et commentez le div suivant
                          # imageOutput("cot_synoptic_image", height = "400px")
                          
                          # Espace réservé en attendant que l'image soit prête
                          div(style = "border: 2px dashed #ccc; padding: 40px; background-color: #f8f9fa;",
                              icon("map-marked-alt", class = "fa-3x", style = "color: #337ab7;"),
                              h4("Synoptique à fournir", style = "color: gray; margin-top: 20px;")
                          )
                      )
                    ),
                    
                    # --- Time Series Graphs ---
                    wellPanel(
                      h4("Évolution temporelle des paramètres", style = "color: #337ab7;"),
                      p("Les graphiques suivants permettent d'observer les corrélations entre COT, météo et qualité de l'eau."),
                      
                      # Graph de carbone organique total (COT)
                      div(style = "margin-bottom: 30px;",
                          h5("1. Carbone Organique Total (COT)", 
                            style = "font-weight: bold; color: #337ab7;"),
                          plotlyOutput("cot_graph", height = "350px"),
                          hr()
                      ),
                      
                      # Graph de précipitations de météo
                      div(style = "margin-bottom: 30px;",
                          h5("2. Précipitations", 
                            style = "font-weight: bold; color: #337ab7;"),
                          p("Impact de la météo sur la matière organique", style = "color: gray; font-size: 13px;"),
                          plotlyOutput("cot_meteo_graph", height = "300px"),
                          hr()
                      ),
                      
                
                    ),
                    
                    # --- Data Export ---
                    wellPanel(
                      h4("Export des données", style = "color: #337ab7;"),
                      downloadButton("cot_download_data", "Télécharger les données COT (CSV)", 
                                    class = "btn-success")
                    )
                  )
                ),# Fin de tab Page carbone organique total
                # --------Prédiction Sulfates-----------  
                 tabPanel("Prédictions taux de sulfates",
                            fluidPage(
                              h2("Prédictions du taux de sulfates", style = "color:#337ab7; font-weight:bold;"),

                              fileInput(
                                "sulfate_file",
                                "Glisser un fichier Excel ou CSV",
                                accept = c(".xlsx", ".xls",".csv")
                              ),

                              tabsetPanel(
                                id = "sulfate_tabs",

                                tabPanel(
                                  "Vésubie",
                                  br(),
                                  uiOutput("vesubie_results")
                                ),

                                tabPanel(
                                  "Joseph Raybaud",
                                  br(),
                                  uiOutput("raybaud_results")
                                )
                              )
                            )
                          )

                 ) # fin de tabset
               ), # fin de tab panel page principale
             ) # fin de navabar
  
  
)  # Fin de ui

# Definition serveur logique
server <- function(input, output, session) {
  # Tableau resume
  output$tableau <- renderTable({
    data.frame(
      "Nombre de Kaptas" = nombre_kaptas,
      "Plage temporelle pour les donnees Kapta" = paste(date_min_kapta, "  -  ", date_max_kapta),
      "Nombre de PSV" = nombre_numeros_uniques,
      "Plage temporelle pour les donnees PSV" = paste(date_min_psv, "  -  ", date_max_psv)
    )
  })

  # -------- Bouton 1 : seuil --------
  observeEvent(input$goButton1, {
    tryCatch({
      selected_input_dDebut3 <- input$date_debut_3
      selected_input_dFin3 <- input$date_fin_3
      selected_input_seuil <- input$seuil_select
      selected_input_upDown <- input$up_down
      selected_input_mesure <- input$choix_mesure

      if (input$date_debut_3 >= input$date_fin_3) {
        stop("La date debut doit etre avant la date fin")
      }

      output$selectInputUI1 <- renderUI({
        div(
          style = "justify-content: center;align-items: center",
          selectInput(
            inputId = "zone_select_psv2",
            label = " Selectionnez un point de surveillance:",
            choices = NULL,
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
            choices = NULL,
            width = "50%"
          )
        )
      })

      output_seuil <- create_datatable(selected_input_dDebut3, selected_input_dFin3, selected_input_seuil, selected_input_upDown, selected_input_mesure)
      output_stat <- create_stats_table(selected_input_dDebut3, selected_input_dFin3, selected_input_seuil, selected_input_upDown, selected_input_mesure)

      req(output_seuil)

      output$data_table <- DT::renderDataTable(output_seuil$table)
      output$stats_table <- DT::renderDataTable(output_stat$table)
      output$graphique_pluie2 <- renderPlotly(create_plot_meteo(selected_input_dDebut3, selected_input_dFin3))

      updateSelectInput(session, "zone_select_psv2", choices = unique(output_seuil$set$Sectorisat))
      updateSelectInput(session, "zone_select_kapta2", choices = unique(output_seuil$set$ENDPOINTREF.x))

      output$kapta_plot_output2 <- renderPlotly(create_plot(output_seuil$set, "KAPTA", input$zone_select_kapta2))
      output$PSV_plot_output2 <- renderPlotly(create_plot(output_seuil$set, "PSV", input$zone_select_psv2))
    }, error = function(err) {
      shinyalert(text = "La date debut doit etre avant la date fin")
    })
  })

  # -------- Bouton 2 : visualisation globale --------
  observeEvent(input$goButton2, {
    tryCatch({
      selected_input_dDebut <- input$Date_Debut
      selected_input_dFin <- input$Date_Fin

      if (input$Date_Debut >= input$Date_Fin) {
        stop("La date debut doit etre avant la date fin")
      }

      output$histo_chlore_output <- renderPlotly(create_histo(selected_input_dDebut, selected_input_dFin))
      output$box_chlore_output <- renderPlotly(create_box(selected_input_dDebut, selected_input_dFin))
      output$carte_output <- renderLeaflet({ create_leafMap(selected_input_dDebut, selected_input_dFin) })
    }, error = function(err) {
      shinyalert(text = "La date debut doit etre avant la date fin")
    })
  })

  # -------- Bouton 3 : Kapta / PSV --------
  observeEvent(input$goButton3, {
    tryCatch({
      selected_input_dDebut2 <- input$Date_Debut_2
      selected_input_dFin2 <- input$Date_Fin_2
      selected_input_zone_kapta <- input$zone_select_kapta
      selected_input_zone_psv <- input$zone_select_psv

      if (input$Date_Debut_2 >= input$Date_Fin_2) {
        stop("La date debut doit etre avant la date fin")
      }

      output$graphique_pluie1 <- renderPlotly(create_plot_meteo(selected_input_dDebut2, selected_input_dFin2))
      output$kapta_plot_output <- renderPlotly(create_kapta_plot(selected_input_dDebut2, selected_input_dFin2, selected_input_zone_kapta))
      output$PSV_plot_output <- renderPlotly(create_PSV_plot(selected_input_dDebut2, selected_input_dFin2, selected_input_zone_psv))
    }, error = function(err) {
      shinyalert(text = "La date debut doit etre avant la date fin")
    })
  })

  # -------- Tables seuil flexible --------
  resultats <- reactive({
    seuil <- input$seuil
    mois <- input$mois
    annees <- input$annees
    secteur <- input$secteur
    recherche <- input$recherche
    up_down <- input$up_down2
    create_seuil_table(seuil, mois, annees, secteur, recherche, up_down)
  })

  output$resultats <- renderTable({ resultats() })

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
    "Pas de donnees pour ces parametres"
  })


  # -------- Statistiques chlore --------
  # ---- Base data (ALWAYS full data) ----
  # Filter PSV data to chlore units within selected date range
  base_chlore_data <- reactive({
    df <- psv_data %>%
      filter(Unite %in% c("mg(Cl2)/L (165)"))

    req(input$date_start, input$date_end)

    df %>%
      filter(
        Date.de.prelevement >= input$date_start,
        Date.de.prelevement <= input$date_end
      )
  })

  # ---- Logical condition for exceedance ----
  is_exceedance <- reactive({
  # Build a comparator based on user direction/threshold
  if (input$exceed_direction == "above") {
      function(x) !is.na(x) & x >= input$chlore_threshold
    } else {
      function(x) !is.na(x) & x <= input$chlore_threshold
    }
  })

  # ---- Per-sensor statistics (central table) ----
  sensor_stats <- reactive({
    df <- base_chlore_data()
    cond <- is_exceedance()

    # Aggregate total measures and exceedance rates per sensor
    df %>%
      group_by(Numero) %>%
      summarise(
        total_mesures = n(),
        nb_depassements = sum(cond(Resultat), na.rm = TRUE),
        pct_depassements = round(nb_depassements / total_mesures * 100, 2),
        .groups = "drop"
      ) %>%
      filter(total_mesures > 0)
  })

    sorted_sensors <- reactive({
    stats <- sensor_stats()
    req(nrow(stats) > 0)

    # Order by selected metric and keep top 100
    stats <- if (input$metric_type == "count") {
      stats %>% arrange(desc(nb_depassements))
    } else {
      stats %>% arrange(desc(pct_depassements))
    }

    stats %>% slice_head(n = 100)
  })


  output$top5_charts <- renderUI({
  stats <- sorted_sensors()

  if (nrow(stats) == 0)
    return(h4("Aucune donnée disponible."))

  # Build per-sensor plot containers + download buttons
  tagList(
    lapply(seq_len(nrow(stats)), function(i) {
      tagList(
        fluidRow(
          column(10, h4(paste("Sonde", stats$Numero[i]))),
          column(
            2,
            downloadButton(
              paste0("download_sensor_", i),
              label = "",
              icon = icon("download")
            )
          )
        ),
        plotlyOutput(paste0("plot_", i))
      )
    })
  )
})



  output$chlore_map <- renderLeaflet({
    stats <- sensor_stats()
    if (nrow(stats) == 0) return(NULL)

    # ---- Join with positions ----
    stats <- stats %>%
      left_join(
        position_PSV %>%
          st_drop_geometry() %>%
          select(Numero, XWGS84, YWGS84),
        by = "Numero"
      ) %>%
      filter(!is.na(XWGS84), !is.na(YWGS84))

    # ---- Choose metric ----
    # Switch between counts and percentages for display
    value_col <- if (input$metric_type == "count") {
      stats$nb_depassements
    } else {
      stats$pct_depassements
    }

    # ---- Color scale ----
    # Use fixed bins for each metric type
    pal <- colorBin(
      palette = c("green", "yellow", "orange", "red"),
      bins = if (input$metric_type == "count") {
        c(0, 5, 10, 20, Inf)
      } else {
        c(0, 20, 40, 60, 100)
      },
      domain = value_col
    )

    leaflet(stats) %>%
      addTiles() %>%
      addCircleMarkers(
        ~XWGS84, ~YWGS84,
        color = ~pal(value_col),
        radius = 8,
        fillOpacity = 0.8,
        label = ~paste0(
          "Sonde ", Numero,
          " | Nombre de dépassements : ", nb_depassements,
          " | Pourcentage : ", pct_depassements, " %"
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = value_col,
        title = ifelse(
          input$metric_type == "count",
          "Nombre de dépassements",
          "Pourcentage de dépassements"
        )
      )
  })


  observeEvent(
    sorted_sensors(), {

    df <- base_chlore_data()
    if (nrow(df) == 0) return()

    # ---- Clear previous plots & downloads (avoid leftovers) ----
  for (i in 1:100) {
    output[[paste0("plot_", i)]] <- renderPlotly(NULL)
    output[[paste0("download_sensor_", i)]] <- downloadHandler(
      filename = function() "",
      content = function(file) {}
    )
  }


    stats <- sorted_sensors()

    for (i in seq_len(nrow(stats))) {
      local({
        idx <- i
        sensor_id <- stats$Numero[idx]

        # Label for the metric shown above each chart
        metric_label <- if (input$metric_type == "count") {
        paste0("Nombre de dépassements : ", stats$nb_depassements[idx])
      } else {
        paste0("Pourcentage de dépassements : ", stats$pct_depassements[idx], " %")
      }


        # ---- GRAPH (ALL VALUES, NO FILTERING) ----
        output[[paste0("plot_", idx)]] <- renderPlotly({
          # Aggregate time series per day for this sensor
          df_sensor <- df %>%
          filter(Numero == sensor_id) %>%
          group_by(Date.de.prelevement) %>%
          summarise(
            Resultat = mean(Resultat, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          arrange(Date.de.prelevement)

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

            geom_line(color = "#007bff", linewidth = 1) +
            geom_point(color = "#007bff", size = 2) +
            geom_hline(
              yintercept = input$chlore_threshold,
              color = "red",
              linetype = "dashed"
            ) +
            labs(
              x = "Date",
              y = "Chlore (mg/L)"
            ) +
            theme_minimal() +
            theme(plot.title = element_text(face = "bold"))

          ggplotly(p, tooltip = "text") %>%
            layout(
              annotations = list(
                list(
                  x = 0,
                  y = 1.05,
                  xref = "paper",
                  yref = "paper",
                  text = metric_label,
                  showarrow = FALSE,
                  xanchor = "left",
                  font = list(size = 12, color = "black")
                )
              )
            )

        })

        # ---- CSV EXPORT ----
        # Export raw records for the selected sensor
        output[[paste0("download_sensor_", idx)]] <- downloadHandler(
          filename = function() {
            paste0(
              "sonde_",
              sensor_id,
              "_",
              input$exceed_direction,
              "_chlore.csv"
            )
          },
          content = function(file) {
            write.csv(
              df %>%
                filter(Numero == sensor_id) %>%
                select(
                  sensor_id = Numero,
                  date = Date.de.prelevement,
                  chlore = Resultat
                ),
              file,
              row.names = FALSE
            )
          }
        )
      })
    }
  })



  # -------- Visualisation globale (Venuja) --------
  # Helper to find available temperature column in Kapta data
  detect_temp_column <- function(df) {
    possible <- c("T° (°C)", "Temperature", "Temp")
    intersect(possible, names(df))[1]
  }

  # Track selected sensors and those currently displayed in graphs
  selected_sondes <- reactiveVal(character(0))

  displayed_sondes <- reactiveVal(character(0))

  # ---- CLEAR ALL SELECTED SENSORS ----
  observeEvent(input$clear_selected_sensors, {

    # Clear internal selections
    selected_sondes(character(0))
    displayed_sondes(character(0))

    # Clear dropdown UI
    updateSelectizeInput(
      session,
      "sensor_selector",
      selected = character(0)
    )
 })


  observeEvent(input$show_selected_sensors, {
  req(length(selected_sondes()) > 0)
  # Show only the sensors chosen via map or dropdown
  displayed_sondes(selected_sondes())
  })

  observeEvent(input$show_all_sensors, {

  # Build the list of ALL sensor IDs (Kapta + PSV)
  all_kapta_ids <- position_sondes %>%
    mutate(ID = paste0("K_", ENDPOINTREF)) %>%
    pull(ID)

  all_psv_ids <- position_PSV %>%
    st_drop_geometry() %>%
    mutate(ID = paste0("P_", Numero)) %>%
    pull(ID)

  displayed_sondes(c(all_kapta_ids, all_psv_ids))
 })




  observe({
    req(position_sondes, position_PSV)

    # Build dropdown choices with readable labels
    kapta_choices <- position_sondes %>%
      mutate(ID = paste0("K_", ENDPOINTREF), Label = paste0("Kapta ", ENDPOINTREF, " - ", Site)) %>%
      select(ID, Label)

    psv_choices <- position_PSV %>%
      st_drop_geometry() %>%
      mutate(ID = paste0("P_", Numero), Label = paste0("PSV ", Numero, " - ", Sectorisat)) %>%
      select(ID, Label)

    all_choices <- bind_rows(kapta_choices, psv_choices)

    updateSelectizeInput(session, "sensor_selector", choices = setNames(all_choices$ID, all_choices$Label), server = TRUE)
  })

  observeEvent(input$sensor_selector, {
    # Keep internal selection in sync with dropdown
    sel <- input$sensor_selector
    selected_sondes(sel)
  })

  output$global_map <- renderLeaflet({
    req(position_sondes, position_PSV)

    # Merge Kapta and PSV point data for a single map
    kapta <- position_sondes %>%
      mutate(ID = paste0("K_", ENDPOINTREF), Type = "Kapta", Label = paste0("Kapta ", ENDPOINTREF, " - ", Site))

    psv <- position_PSV %>%
      st_drop_geometry() %>%
      mutate(ID = paste0("P_", Numero), Type = "PSV", Label = paste0("PSV ", Numero, " - ", Sectorisat), Longitude = XWGS84, Latitude = YWGS84)

    all_pts <- bind_rows(kapta, psv)

    # Base map with colored markers by type
    leaflet(all_pts) %>%
      addTiles() %>%
      addCircleMarkers(
        ~Longitude, ~Latitude,
        layerId = ~ID,
        radius = 7,
        fillOpacity = 0.9,
        color = ~ifelse(Type == "Kapta", "#007bff", "#00b894"),
        label = ~Label,
        labelOptions = labelOptions(
          direction = "top",
          offset = c(0, -10),
          opacity = 0.95,
          style = list("background-color" = "white", "padding" = "6px", "border-radius" = "4px")
        )
      )
  })

  observeEvent(input$global_map_marker_click, {
    # Toggle a sensor when clicking its marker
    id <- input$global_map_marker_click$id
    cur <- selected_sondes()

    if (id %in% cur) {
      cur <- setdiff(cur, id)
    } else {
      cur <- c(cur, id)
    }

    selected_sondes(cur)
    updateSelectInput(session, "sensor_selector", selected = cur)
  })

  observe({
    req(position_sondes, position_PSV)

    # Refresh map marker colors based on current selection
    kapta <- position_sondes %>%
      mutate(ID = paste0("K_", ENDPOINTREF), Type = "Kapta", Label = paste0("Kapta ", ENDPOINTREF, " - ", Site))

    psv <- position_PSV %>%
      st_drop_geometry() %>%
      mutate(ID = paste0("P_", Numero), Type = "PSV", Label = paste0("PSV ", Numero, " - ", Sectorisat), Longitude = XWGS84, Latitude = YWGS84)

    all_pts <- bind_rows(kapta, psv)

    leafletProxy("global_map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = all_pts,
        ~Longitude, ~Latitude,
        layerId = ~ID,
        radius = 7,
        fillOpacity = 0.9,
        color = ~ifelse(ID %in% selected_sondes(), "orange", ifelse(Type == "Kapta", "#007bff", "#00b894")),
        label = ~Label
      )
  })

  output$selected_sondes_count <- renderText({
    # Display count of selected sensors
    paste(length(selected_sondes()), "sondes sélectionnées")
  })

  output$global_graphs <- renderUI({
    req(displayed_sondes())
    # One plot container per displayed sensor
    tagList(lapply(displayed_sondes(), function(id) plotlyOutput(paste0("plot_", id), height = 320)))
  })

  observe({
    req(displayed_sondes())

    for (sid in displayed_sondes()) {
      local({
        my_id <- sid

        output[[paste0("plot_", my_id)]] <- renderPlotly({
          # Build plots by sensor type (Kapta vs PSV)
          type <- substr(my_id, 1, 1)
          num <- as.numeric(sub("^[KP]_", "", my_id))

          if (type == "K") {
            raw_df <- donnees_sondes %>%
              filter(
                ENDPOINTREF == num,
                as.Date(DATEREF) >= input$date_start_global,
                as.Date(DATEREF) <= input$date_end_global
              )

            if (nrow(raw_df) == 0) return(plotly_empty())

            # Resolve temperature column name dynamically
            temp_col <- detect_temp_column(raw_df)

            df <- raw_df %>%
              group_by(DATEREF) %>%
              summarise(
                chlore1 = mean(`Concentration chlore 1 (mg/L)`, na.rm = TRUE),
                chlore2 = mean(`Concentration chlore 2 (mg/L)`, na.rm = TRUE),

               temperature_raw = if (!is.na(temp_col)) {
               suppressWarnings(
                mean(as.numeric(.data[[temp_col]]), na.rm = TRUE)
                  )
                } else {
                  NA_real_
                },


                temperature_scaled = temperature_raw / 100,

                .groups = "drop"
              ) %>%
              arrange(DATEREF)



            if (nrow(df) == 0) return(plotly_empty())

            # ---- SAFETY CHECK: nothing numeric to display ----
            has_chlore1 <- "chlore1" %in% input$variables_kapta &&
                          any(is.finite(df$chlore1))

            has_chlore2 <- "chlore2" %in% input$variables_kapta &&
                          any(is.finite(df$chlore2))

            has_temp <- "temperature" %in% input$variables_kapta &&
                        any(is.finite(df$temperature_scaled))

            if (!has_chlore1 && !has_chlore2 && !has_temp) {
              return(plotly_empty())
            }

            p <- ggplot(df, aes(x = DATEREF))

            # Kapta chlore series (optional by checkbox)
            if ("chlore1" %in% input$variables_kapta) {
              p <- p + geom_line(
                aes(
                  x = DATEREF,
                  y = chlore1,
                  group = 1,
                  color = "Chlore 1",
                  text = paste0(
                    "Date : ", DATEREF,
                    "<br>Chlore 1 : ", round(chlore1, 3), " mg/L",
                    "<br>Température réelle : ", round(temperature_raw, 1), " °C"
                  )
                ),
                linewidth = 0.8,
                na.rm = TRUE
              )



            }

            # Second chlore series (optional)
            if ("chlore2" %in% input$variables_kapta) {
              p <- p + geom_line(
                  aes(
                    x = DATEREF,
                    y = chlore2,
                    group = 1,
                    color = "Chlore 2",
                    text = paste0(
                      "Date : ", DATEREF,
                      "<br>Chlore 2 : ", round(chlore2, 3), " mg/L",
                      "<br>Température réelle : ", round(temperature_raw, 1), " °C"
                    )
                  ),
                  linewidth = 0.8,
                  na.rm = TRUE
                )

            }

              
              
              # ---- Optional temperature (same axis, dashed) ----
             if (has_temp) {
              p <- p + geom_line(
                aes(
                  x = DATEREF,
                  y = temperature_scaled,
                  group = 1,
                  color = "Température (÷100)",
                  text = paste0(
                    "Date : ", DATEREF,
                    "<br>Température (affichée) : ", round(temperature_scaled, 3),
                    "<br>Température réelle : ", round(temperature_raw, 1), " °C"
                  )
                ),
                linetype = "dashed",
                linewidth = 1.2,
                na.rm = TRUE
              )
            }


            p <- p +
              labs(
                title = paste("Sonde Kapta", num),
                x = "Date",
                color = "Afficher / masquer"
              ) +
              theme_minimal() +
              theme(plot.title = element_text(face = "bold"))

           # Return interactive plotly chart
           return(
              ggplotly(p, tooltip = "text") %>%
                layout(
                  yaxis = list(
                    tickmode = "auto",
                    nticks = 10
                  ),
                  legend = list(itemclick = "toggle")
                )
            )

          }

            if (type == "P") {

              # PSV: keep Conductivity and COT only
              df <- psv_data %>%
                filter(
                  Numero == num,
                  Date.de.prelevement >= input$date_start_global,
                  Date.de.prelevement <= input$date_end_global,
                  Parametre %in% c("Conductiv. (1303)", "COT (1305)")
                )

              if (nrow(df) == 0) return(plotly_empty())

              show_cond <- "conductivite" %in% input$variables_psv
              show_cot  <- "cot" %in% input$variables_psv

              # ---- Numeric conversion (VERY IMPORTANT) ----
              df_wide <- df %>%
              mutate(Resultat_num = suppressWarnings(as.numeric(Resultat))) %>%
              group_by(Date.de.prelevement, Parametre) %>%
              summarise(
                Resultat_num = mean(Resultat_num, na.rm = TRUE),
                .groups = "drop"
              ) %>%
              tidyr::pivot_wider(
                names_from = Parametre,
                values_from = Resultat_num
              )


              # ---- Availability checks ----
              has_cond <- show_cond &&
                "Conductiv. (1303)" %in% names(df_wide) &&
                any(is.finite(df_wide$`Conductiv. (1303)`))

              cot_exists <- show_cot && "COT (1305)" %in% names(df_wide)
              cot_has_values <- cot_exists && any(is.finite(df_wide$`COT (1305)`))

              # ---- Nothing to display at all ----
             if (!has_cond && !show_cot) {
                return(plotly_empty())
              }

              p <- ggplot(df_wide, aes(x = Date.de.prelevement))

              # ---- Conductivity (ALWAYS shown if available) ----
              if (has_cond) {
                p <- p + geom_line(
                  aes(
                    y = `Conductiv. (1303)`,
                    group = 1,
                    color = "Conductivité",
                    text = paste0(
                      "Date : ", as.character(Date.de.prelevement),
                      "<br>Conductivité : ",
                      round(`Conductiv. (1303)`, 1), " µS/cm"
                    )
                  ),
                  linewidth = 1.2,
                  na.rm = TRUE
                )
              }

              # ---- COT (only if available) ----
              if (cot_has_values) {
                p <- p + geom_line(
                  aes(
                    y = `COT (1305)`,
                    group = 1,
                    color = "COT",
                    text = paste0(
                      "Date : ", as.character(Date.de.prelevement),
                      "<br>COT : ",
                      round(`COT (1305)`, 2), " mg/L"
                    )
                  ),
                  linewidth = 1,
                  linetype = "dashed",
                  na.rm = TRUE
                ) +
                  geom_point(
                    aes(y = `COT (1305)`, color = "COT"),
                    size = 2,
                    na.rm = TRUE
                  )
              }

          # ---- Message ONLY if COT selected but unavailable ----
              if (cot_exists && !cot_has_values) {
                p <- p + annotate(
                  "label",
                  x = min(df_wide$Date.de.prelevement, na.rm = TRUE),
                  y = max(df_wide$`Conductiv. (1303)`, na.rm = TRUE),
                  label = "COT non disponible pour cette sonde",
                  hjust = 0,
                  vjust = 1.2,
                  size = 4,
                  label.size = 0,
                  fill = "gray90",
                  color = "gray30"
                )
              }


              p <- p +
                labs(
                  title = paste("Sonde PSV", num),
                  x = "Date",
                  y = "Valeur",
                  color = "Afficher / masquer"
                ) +
                theme_minimal() +
                theme(plot.title = element_text(face = "bold"))

             # Return interactive plotly chart
             return(
                ggplotly(p, tooltip = "text") %>%
                  layout(
                    yaxis = list(
                      tickmode = "auto",
                      nticks = 10
                    ),
                    legend = list(itemclick = "toggle")
                  )
              )

            }


        })
      })
    }
  })


 # -------- server de carbone organique total  --------
observeEvent(input$cot_go_button, {

  # ---- Date validation ----
  if (input$cot_date_start >= input$cot_date_end) {
    shinyalert(text = "La date début doit être avant la date fin")
    return()
  }

  # ---- Filtrer les données COT (NOM DE PARAMÈTRE CORRECT) ----
  cot_df <- psv_data %>%
    mutate(Date = as.Date(Date.de.prelevement)) %>%
    filter(
      Date >= input$cot_date_start,
      Date <= input$cot_date_end,
      Parametre == "C Orga (1841)"
    ) %>%
    mutate(Resultat = suppressWarnings(as.numeric(Resultat))) %>%
    arrange(Date)


  # ---- graph COT ----
  output$cot_graph <- renderPlotly({

    if (nrow(cot_df) == 0) {
      return(
        plotly_empty() %>%
          layout(
            annotations = list(
              list(
                text = "Aucune donnée COT disponible pour cette période",
                x = 0.5, y = 0.5,
                xref = "paper", yref = "paper",
                showarrow = FALSE,
                font = list(size = 16)
              )
            )
          )
      )
    }

    plot_ly(
  cot_df,
  x = ~Date,
  y = ~Resultat,
  type = "scatter",
  mode = "lines+markers",
  name = "COT (mg/L)",
  line = list(color = "darkorange")
) %>%
  layout(
    xaxis = list(title = "Date"),
    yaxis = list(title = "COT (mg/L)"),
    shapes = list(
      list(
        type = "line",
        x0 = min(cot_df$Date),
        x1 = max(cot_df$Date),
        y0 = 2,
        y1 = 2,
        line = list(color = "red", dash = "dash")
      )
    )
  )
    



  })

  # ---- ALERT ----
  output$cot_alert_ui <- renderUI({
    if (nrow(cot_df) > 0 && any(cot_df$Resultat > 2, na.rm = TRUE)) {
      div(
        class = "alert alert-danger",
        icon("exclamation-triangle"),
        strong("ALERTE : "),
        "COT > 2 mg/L détecté sur la période sélectionnée"
      )
    } else {
      div(
        class = "alert alert-success",
        icon("check-circle"),
        "Aucun dépassement du seuil COT détecté sur la période sélectionnée."
      )
    }
  })

  # ---- graph de meteo ----
  output$cot_meteo_graph <- renderPlotly({

  p <- create_plot_meteo(input$cot_date_start, input$cot_date_end)

  # Force line display
  p$x$data[[1]]$type <- "scatter"
  p$x$data[[1]]$mode <- "lines+markers"

  p
  
}) 

})

# Gestionnaire de téléchargement des données de COT filtrées
output$cot_download_data <- downloadHandler(
  filename = function() {
    paste0("donnees_cot_", 
           format(input$cot_date_start, "%Y%m%d"), "_",
           format(input$cot_date_end, "%Y%m%d"), ".csv")
  },
  content = function(file) {
    df <- psv_data %>%
      mutate(Date = as.Date(Date.de.prelevement)) %>%
      filter(
        Date >= input$cot_date_start,
        Date <= input$cot_date_end,
        Parametre == "C Orga (1841)"
      ) %>%
      arrange(Date)

    write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
  }
)## end of COT
# --------- Sulfates prédictions ---------
# PRÉDICTIONS SULFATES VÉSUBIE

uploaded_sulfate_data <- reactiveVal(NULL)

observeEvent(input$sulfate_file, {
  req(input$sulfate_file)
  uploaded_sulfate_data(input$sulfate_file$datapath)
})

output$vesubie_results <- renderUI({

  if (is.null(uploaded_sulfate_data())) {
    return(
      wellPanel(
        style = "text-align:center; padding:40px;",
        icon("upload", class = "fa-3x", style = "color:#337ab7;"),
        h4("Téléchargez un fichier Excel"),
        p("Colonnes requises : temperature, Conductivité.µS.cm, cumul_glissant_*")
      )
    )
  }

  result <- predict_sulfate_vesubie(uploaded_sulfate_data())

  if (!result$success) {
    return(div(class = "alert alert-danger", result$error))
  }

  summary <- result$summary

  risk_class <- if (summary$n_critical > 0) "danger"
  else if (summary$n_warning > 0) "warning"
  else "success"

  risk_text <- if (summary$n_critical > 0) "RISQUE CRITIQUE"
  else if (summary$n_warning > 0) "ATTENTION"
  else "PAS DE RISQUE"

  output$vesubie_prediction_table <- DT::renderDataTable({
    result$data %>%
      mutate(
        Alerte = case_when(
          alert_level == 2 ~ "Critique",
          alert_level == 1 ~ "Attention",
          TRUE ~ "Normal"
        )
      ) %>%
      select(jour, temperature, Conductivité.µS.cm, pred_sulfate, Alerte) %>%
      rename(
        Date = jour,
        `Température (°C)` = temperature,
        `Conductivité (µS/cm)` = Conductivité.µS.cm,
        `Sulfates prédits (mg/L)` = pred_sulfate
      )
  })

  tagList(
    div(class = paste("alert alert-", risk_class),
        style = "font-size:18px; font-weight:bold;",
        risk_text),

    fluidRow(
      column(4, wellPanel(h3(summary$n_critical), "Critiques")),
      column(4, wellPanel(h3(summary$n_warning), "Alertes")),
      column(4, wellPanel(h3(summary$n_safe), "Normales"))
    ),

    div(
      class = "alert alert-info",
      paste0("Max : ", round(summary$max_concentration,1), " mg/L | Moyenne : ",
             round(summary$avg_concentration,1), " mg/L")
    ),

    hr(),
    DT::dataTableOutput("vesubie_prediction_table")
  )
})



output$raybaud_results <- renderUI({
  wellPanel(
    icon("ban", class="fa-2x"),
    h4("Modèle Joseph Raybaud non disponible"),
    p("Aucun modèle entraîné pour ce site.")
  )
}) # end of sulfate

}# end server function

# Run the app
shinyApp(ui, server)
# runApp(list(ui=ui, server=server), host="10.165.8.60", port=5050)
