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
                              tabsetPanel(
                                tabPanel("Contextualisation",
                                         br(),
                                         # Texte de contextualisation
                                         includeHTML("./www/HTML/Contexte.html")
                                               
                                         ),
                                tabPanel("Objectif",
                                         br(),
                                         # Affichage du contenu du fichier texte
                                         includeHTML("./www/HTML/Objectifs.html")
                                )
                              )
                              
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
                 
                 tabPanel("Visualisation seuil",
                          h2("Visualisation seuil", style = "color: #337ab7;font-weight:bold;"),
                         tabsetPanel(
                           tabPanel("Periode simple",
                        
                            
                          br(),
                          sidebarLayout(
                          # Définition d'une sidebar
                            sidebarPanel(
                              id = "sidebar2",
                            
                            # Conteneur des filtres paramètres
                              wellPanel(
                              h3("Filtre paramètres", align = "center", style = "color: #337ab7;"),
                              br(),
                              sliderInput("seuil_select", "Seuil de concentration de chlore:", 0.08, min = 0, max = 0.2, step = 0.01),
                              
                              # Découpage en colonnes
                              splitLayout(
                                cellWidths = c("50%", "50%"),  # Largeur des cellules    
                                
                                radioButtons("up_down", "Option de dépassement:",         # Cellule 1
                                            choices = c("Inférieur", "Supérieur"),                
                                            selected = "Inférieur"),
                              
                                radioButtons("choix_mesure", "Type de sondes:",      # Cellule 2
                                            choices = c("KAPTA", "PSV"),
                                            selected = "KAPTA")
                                ),
                              
                              # Découpage en colonnes
                              splitLayout(
                                cellWidths = c("50%", "50%"),  # Largeur des cellules
                                
                                dateInput("date_debut_3", "Choisissez la date début :", value = "2021-03-01"),     # Cellule 1
                                dateInput("date_fin_3", "Choisissez la date fin :", value = "2021-07-01"),       # Cellule 2
                              ),
                              
                              # Bouton pour appliqur les filtres sur les graphiques
                              actionButton("goButton1", "Go", class = "my-button")
                              
                              ),  # Fin du conteneur filtre paramètres 
                              
                              # Ajout de données météo
                              h3("Graphique pluviométrique", align = "center", style = "color: #337ab7;"),
                              plotlyOutput("graphique_pluie2")
                            
                            ), #Fin du sidebar
                          
                            #Definition du main panel
                            mainPanel(
                              
                              # Définition de tab tertiaire
                              tabsetPanel(
                                tabPanel("Données dépassement",
                                         br(),
                                         DT::dataTableOutput("data_table") # Tableau de seuil
                                ),
                                tabPanel("Statistiques",
                                         br(),
                                         DT::dataTableOutput("stats_table") # Tableau statistiques
                                )
                              ), # Fin de tab
                              br(),
                              
                              # Panel si choix = PSV
                              conditionalPanel(condition = "input.choix_mesure == 'PSV'",
                                               
                                               # Block définie dans la partie server permet d'attendre le click du boutton
                                               uiOutput("selectInputUI1"),
                                               plotlyOutput("PSV_plot_output2")     # Graphiques des concentrations PSV
                              ),
                              # Panel si choix = Kapta
                              conditionalPanel(condition = "input.choix_mesure == 'KAPTA'",
                                               
                                               # Block définie dans la partie server permet d'attendre le click du boutton
                                               uiOutput("selectInputUI2"),
                                               plotlyOutput("kapta_plot_output2")   # Graphiques des concentrations Kapta
                              ),
                              
                            ),  # Fin de main panel
                          
                          ), # Fin de sidebarLayout 
                        
                        ), # Fin de tab Periode fixe
                        
                 tabPanel("Periode flexible",
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
                              selectInput("recherche", "Numéro de recherche:", choices = NULL, multiple = TRUE, selected = unique(donnees_avec_secteur$Numero), width = "50%")
                              
                              ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Résultats", tableOutput("resultats")),
                                tabPanel("Occurrences", 
                                       p("Certains numéros peuvent avoir plusieurs occurrences selon le mois et l'année. Pour plus de précision, veuillez choisir un numéro."),
                                       tableOutput("occurrences"))
                            )
                              
                            )
                          ))
                         )
                 ),
                 
                 tabPanel("Visualisation globale",
                          h2("Visualisation globale", style = "color: #337ab7;font-weight:bold;"),
                          sidebarLayout(
                            
                            # Définition sidebar
                            sidebarPanel(
                              id = "sidebar3",
                              
                              # Définition panel de filtre
                              wellPanel(
                                h3("Filtre paramètres",align = "center", style = "color: #337ab7;"),
                                br(),
                                
                                # Découpage en colonnes
                                splitLayout(
                                  cellWidths = c("50%", "50%"),
                                  dateInput("Date_Debut", "Choisissez la date début :", value = "2021-03-01"),    # Cellule 1 
                                  dateInput("Date_Fin", "Choisissez la date fin :", value = "2021-07-01")       # Cellule 2
                                  ),
                                
                                # Bouton de validation des filtres
                                actionButton("goButton2", "Go", class = "my-button")
                                ),
                            
                            h4("Graphes de Répartition" ,align = "center", style = "color: #337ab7;"),
                            
                            # Bouton déroulant
                            dropdownButton(
                              
                              # Paramètres
                              circle = TRUE, 
                              status = "primary",
                              width = "1000px",
                              tooltip = tooltipOptions(title = "Cliquez pour afficher les graphiques"),
                              
                              # Contenu
                              h4("Graphes de Répartition", style = "color: #337ab7;"),
                              p("Ces graphique permettent d'analyser la répartition de toutes les mesures effectuées (sondes et PSV) sur la période selectionné plus haut.\n Ils permettent aussi d'obtenir quelques données statistiques globales telles que les quartiles ou la médiane", align = "center"),
                              br(),
                              
                              # Découpage en colonnes
                              splitLayout(
                                cellWidths = c("50%", "50%"),
                                plotlyOutput("histo_chlore_output"),  # Cellule 1
                                plotlyOutput("box_chlore_output")     # Cellule 2
                              )
                              ) # Fin de bouton déroulant
                            ),
                            
                            # Définition main Panel
                            mainPanel(
                                  h3("Carte des canalisations", align = "center", style = "color: #337ab7;"),
                                  leafletOutput("carte_output", height = "800px")  # Carte leaflet
                                  
                                  ), #Fin de main panel
                            
                            ) # Fin de sidebar
                          
                          ), # Fin de tab Visualisation globale
                 
                 
                 tabPanel("Visualisation Kapta/PSV",
                          h2("Visualisation Kapta/PSV", style = "color: #337ab7;font-weight:bold;"),
                          sidebarLayout(
                            
                            # Définition sidebar
                            sidebarPanel(
                              id = "sidebar4",
                              
                              # Définition du panel de filtre 
                              wellPanel(
                                h3("Filtre paramètres", align = "center", style = "color: #337ab7;"),
                                br(),
                                
                                splitLayout(
                                  cellWidths = c("50%", "50%"),
                                    dateInput("Date_Debut_2", "Choisissez la date début :", value = "2021-03-01"),
                                    dateInput("Date_Fin_2", "Choisissez la date fin :", value = "2021-07-01")
                                  ),
                                
                                # Panel si tab1 = PSV    
                                conditionalPanel(condition = "input.tabs1 == 'PSV'",
                                                 selectInput(
                                                   inputId = "zone_select_psv",
                                                   label = " Selectionnez une zone:",
                                                   choices = choices_secteur_psv,
                                                   width = "100%"
                                                 )
                                                 ),
                                
                                # Panel si tab1 = KAPTA
                                conditionalPanel(condition = "input.tabs1 == 'KAPTA'",
                                                 selectInput(
                                                   inputId = "zone_select_kapta",
                                                   label = " Selectionnez une zone:",
                                                   choices = choices_secteur_kapta,
                                                   width = "50%"
                                                 )
                                                 ),
                                
                                #Bouton de validation des filtres
                                actionButton("goButton3", "Go", class = "my-button")
                                ),
                                
                                # Afichage des données météo
                                h3("Graphique pluviométrique", align = "center", style = "color: #337ab7;"),
                                plotlyOutput("graphique_pluie1")
                              
                              ), # Fin de sidebar
                            
                            #Définition du main panel
                            mainPanel(
                              
                              # Définition de tabs tertiaire
                              tabsetPanel(id = "tabs1",
                                          tabPanel("PSV",
                                                   br(),
                                                   plotlyOutput("PSV_plot_output")
                                                   ),
                                          tabPanel("KAPTA",
                                                   br(),
                                                   plotlyOutput("kapta_plot_output")
                                                   ),
                                          )
                              
                              ), # Fin de main panel
                            
                            ) # Fin de sidebarLayout
                          
                          ), # Fin de tab KAPTA/PSV
                          
                          
                                          # --------Venuja-----------
                          tabPanel(
                    "Visualisation Globale",
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
                        selectInput("sensor_selector", "Sélectionnez jusqu'à 4 sondes :", choices = NULL, multiple = TRUE),
                        leafletOutput("global_map", height = 400),
                        tags$div("Zone de Nice – Carte interactive des sondes", 
                                style = "text-align:center; margin-top:10px;")
                      ),

                      # --- Section: Graphiques ---
                      wellPanel(
                        h5("Sélectionnez jusqu'à 4 sondes sur la carte ou le menu ci-dessus pour afficher les graphiques.",
                          style = "text-align:center; margin-bottom:20px;"),
                        uiOutput("global_graphs")
                      )
                    )
                  ),

                          # --------Venuja-----------
                          tabPanel(
                            "Statistiques Chlore",
                            
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
                                  )

                              ),
                              
                              # ---- Map section ----
                              h4("Localisation des dépassements de seuil"),
                              leafletOutput("chlore_map", height = 400),
                              
                              br(),
                              h4("Top 5 des points avec le plus de dépassements"),
                              uiOutput("top5_charts")
                            )
                          )



                 )
               ),
             )
  
  
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

  # -------- Venuja --------
  # -------- Statistiques chlore --------
  # ---- Base data (ALWAYS full data) ----
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
    if (input$exceed_direction == "above") {
      function(x) !is.na(x) & x > input$chlore_threshold
    } else {
      function(x) !is.na(x) & x < input$chlore_threshold
    }
  })


  output$top5_charts <- renderUI({
    df <- base_chlore_data()
    if (nrow(df) == 0) return(h4("Aucune donnée disponible."))

    cond <- is_exceedance()

    top_sensors <- df %>%
      group_by(Numero) %>%
      summarise(
        depassements = sum(cond(Resultat), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(depassements)) %>%
      slice_head(n = 5)

    if (nrow(top_sensors) == 0)
      return(h4("Aucun dépassement pour ces paramètres."))

    tagList(
      lapply(seq_len(nrow(top_sensors)), function(i) {
        tagList(
          fluidRow(
            column(
              10,
              h4(paste("Sonde", top_sensors$Numero[i]))
            ),
            column(
              2,
              downloadButton(
                outputId = paste0("download_sensor_", i),
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
    df <- base_chlore_data()
    if (nrow(df) == 0) return(NULL)

    cond <- is_exceedance()

    df_summary <- df %>%
      group_by(Numero) %>%
      summarise(
        depassements = sum(cond(Resultat), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(depassements > 0)

    df_summary <- df_summary %>%
      left_join(
        position_PSV %>%
          st_drop_geometry() %>%
          select(Numero, XWGS84, YWGS84),
        by = "Numero"
      ) %>%
      filter(!is.na(XWGS84), !is.na(YWGS84))

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
        radius = 8,
        fillOpacity = 0.8
      ) %>%
      addLegend(
        "bottomright",
        colors = c("green", "yellow", "orange", "red"),
        labels = c("<= 5", "6–10", "11–20", "> 20"),
        title = "Dépassements"
      )
  })


  observeEvent(
  list(
    base_chlore_data(),
    input$chlore_threshold,
    input$exceed_direction
  ), {
    df <- base_chlore_data()
    if (nrow(df) == 0) return()

    # ---- Clear previous plots & downloads (avoid leftovers) ----
  for (i in 1:5) {
    output[[paste0("plot_", i)]] <- renderPlotly(NULL)
    output[[paste0("download_sensor_", i)]] <- downloadHandler(
      filename = function() "",
      content = function(file) {}
    )
  }

    cond <- is_exceedance()

    top_sensors <- df %>%
      group_by(Numero) %>%
      summarise(
        depassements = sum(cond(Resultat), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(depassements)) %>%
      slice_head(n = 5)

    for (i in seq_len(nrow(top_sensors))) {
      local({
        idx <- i
        sensor_id <- top_sensors$Numero[idx]

        # ---- GRAPH (ALL VALUES, NO FILTERING) ----
        output[[paste0("plot_", idx)]] <- renderPlotly({
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
              title = paste("Sonde", sensor_id),
              x = "Date",
              y = "Chlore (mg/L)"
            ) +
            theme_minimal() +
            theme(plot.title = element_text(face = "bold"))

          ggplotly(p, tooltip = "text")
        })

        # ---- CSV EXPORT ----
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
  detect_temp_column <- function(df) {
    possible <- c("T° (°C)", "Temperature", "Temp")
    intersect(possible, names(df))[1]
  }

  selected_sondes <- reactiveVal(character(0))

  observe({
    req(position_sondes, position_PSV)

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
    sel <- input$sensor_selector
    if (length(sel) > 4) {
      sel <- tail(sel, 4)
      updateSelectInput(session, "sensor_selector", selected = sel)
    }
    selected_sondes(sel)
  })

  output$global_map <- renderLeaflet({
    req(position_sondes, position_PSV)

    kapta <- position_sondes %>%
      mutate(ID = paste0("K_", ENDPOINTREF), Type = "Kapta", Label = paste0("Kapta ", ENDPOINTREF, " - ", Site))

    psv <- position_PSV %>%
      st_drop_geometry() %>%
      mutate(ID = paste0("P_", Numero), Type = "PSV", Label = paste0("PSV ", Numero, " - ", Sectorisat), Longitude = XWGS84, Latitude = YWGS84)

    all_pts <- bind_rows(kapta, psv)

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
    id <- input$global_map_marker_click$id
    cur <- selected_sondes()

    if (id %in% cur) {
      cur <- setdiff(cur, id)
    } else {
      cur <- c(cur, id)
      if (length(cur) > 4) cur <- tail(cur, 4)
    }

    selected_sondes(cur)
    updateSelectInput(session, "sensor_selector", selected = cur)
  })

  observe({
    req(position_sondes, position_PSV)

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
    paste(length(selected_sondes()), "/ 4 sondes selectionnees")
  })

  output$global_graphs <- renderUI({
    req(selected_sondes())
    tagList(lapply(selected_sondes(), function(id) plotlyOutput(paste0("plot_", id), height = 320)))
  })

  observe({
    req(selected_sondes())

    for (sid in selected_sondes()) {
      local({
        my_id <- sid

        output[[paste0("plot_", my_id)]] <- renderPlotly({
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

            temp_col <- detect_temp_column(raw_df)

            df <- raw_df %>%
              group_by(DATEREF) %>%
              summarise(
                chlore1 = mean(`Concentration chlore 1 (mg/L)`, na.rm = TRUE),
                chlore2 = mean(`Concentration chlore 2 (mg/L)`, na.rm = TRUE),
                temperature = if (!is.na(temp_col))
                  mean(.data[[temp_col]], na.rm = TRUE)
                else NA_real_,
                .groups = "drop"
              ) %>%
              arrange(DATEREF)


            if (nrow(df) == 0) return(plotly_empty())


            p <- ggplot(df, aes(x = DATEREF))

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
                    "<br>Température : ", round(temperature, 1), " °C"
                  )
                ),
                linewidth = 1.2,
                na.rm = TRUE
              )



            }

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
                      "<br>Température : ", round(temperature, 1), " °C"
                    )
                  ),
                  linewidth = 1.2,
                  na.rm = TRUE
                )

            }

      # ---- Always add primary Y axis ----
              p <- p + scale_y_continuous(name = "Chlore (mg/L)")

              # ---- Optional temperature (same axis, dashed) ----
              if ("temperature" %in% input$variables_kapta &&
                  any(!is.na(df$temperature))) {

                p <- p + geom_line(
                  aes(
                    x = DATEREF,
                    y = temperature,
                    group = 1,
                    color = "Température (°C)",
                    text = paste0(
                      "Date : ", DATEREF,
                      "<br>Température : ", round(temperature, 1), " °C"
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

            return(
              ggplotly(p, tooltip = "text") %>%
                layout(legend = list(itemclick = "toggle"))
            )
          }

            if (type == "P") {

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

        # ---- Build wide table once ----
        df_wide <- df %>%
          select(Date.de.prelevement, Parametre, Resultat) %>%
          tidyr::pivot_wider(names_from = Parametre, values_from = Resultat)

        # ---- If COT is selected but not available ----
        if (show_cot && !"COT (1305)" %in% names(df_wide)) {
          p <- ggplot() +
            annotate(
              "text",
              x = 0.5,
              y = 0.5,
              label = "Paramètre COT non disponible pour cette sonde",
              size = 5
            ) +
            theme_void() +
            labs(title = paste("Sonde PSV", num))

          return(ggplotly(p))
        }


            p <- ggplot(df_wide, aes(x = Date.de.prelevement))

            # ---- Conductivité ----
            if (show_cond && "Conductiv. (1303)" %in% names(df_wide)) {
              p <- p + geom_line(
                aes(
                  x = Date.de.prelevement,
                  y = `Conductiv. (1303)`,
                  group = 1,                     # ← THIS IS THE KEY FIX
                  color = "Conductivité",
                  text = paste0(
                    "Date : ", Date.de.prelevement,
                    "<br>Conductivité : ",
                    round(`Conductiv. (1303)`, 1), " µS/cm"
                  )
                ),
                linewidth = 1.2,                 # slightly thicker so it’s visible
                na.rm = TRUE
              )


            }

            # ---- COT ----
            if (show_cot && "COT (1305)" %in% names(df_wide)) {
              p <- p + geom_line(
                aes(
                  y = `COT (1305)`,
                  color = "COT",
                  text = paste0(
                    "Date : ", Date.de.prelevement,
                    "<br>COT : ", round(`COT (1305)`, 2), " mg/L"
                  )
                ),
                linewidth = 1,
                linetype = "dashed",
                na.rm = TRUE
              )
              p <- p + geom_point(
                aes(
                  y = `COT (1305)`,
                  color = "COT"
                ),
                size = 2,
                na.rm = TRUE
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

            return(
              ggplotly(p, tooltip = "text") %>%
                layout(legend = list(itemclick = "toggle"))
            )
          }

        })
      })
    }
  })
}

# Run the app
shinyApp(ui, server)
# runApp(list(ui=ui, server=server), host="10.165.8.60", port=5050)
