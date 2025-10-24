# Script principal: Définition de l'architecture et logique du dashboard
options(download.file.method = "wininet")
setwd("S:/DPH/07 - DATASCIENCE/2-visualisation_valorisation_donnees/Qualite/projet_dato/P4A_DATO-main/DATO_dashboard_app")
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

source("./Script/Carto_2.R")        # Carte leaflet
source("./Script/Graphes.R")        # Graphiques de répartition
source("./Script/Kapta-PSV.R")      # Graphiques généraux KAPTA / PSV
source("./Script/Seuil_chlore.R")   # Graphiques et tableau etude de seuil
source("./Script/Meteo.R")          # Graphique pluviométrique
source("./Script/Seuil_chlore2.R")

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
                                  dateInput("Date_Début", "Choisissez la date début :", value = "2021-03-01"),    # Cellule 1 
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
                                    dateInput("Date_Début_2", "Choisissez la date début :", value = "2021-03-01"),
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
                          
                          ) # Fin de tab KAPTA/PSV
                 )
               ),
             )
  
  )  # Fin de ui
  
  # Definition serveur logique
  server <- function(input, output, session) {
    
    #Tableau
    output$tableau <- renderTable({
      data <- data.frame(
        "Nombre de Kaptas" = nombre_kaptas,
        "Plage temporelle pour les données Kapta" = paste(date_min_kapta, "  -  ", date_max_kapta),
        "Nombre de PSV" = nombre_numeros_uniques,
        "Plage temporelle pour les données PSV" = paste(date_min_psv, "  -  ", date_max_psv)
      )
      data
    })
    
    # # Lecture du contenu du fichier texte
    # objectifText <- readLines("./www/objectifs Text")
    
    # # Affichage du contenu du fichier texte
    # output$objectifText <- renderText({objectifText
    #   
    # })

    # Evennemment observable: Click button 1 
    observeEvent(input$goButton1, {
      
      tryCatch({
        
        # Récupère les inputs de filtres
        selected_input_dDébut3 <- input$date_debut_3
        selected_input_dFin3 <- input$date_fin_3
        selected_input_seuil <- input$seuil_select
        selected_input_upDown <- input$up_down
        selected_input_mesure <- input$choix_mesure
        
        # Test de cohérences des dates 
        if(input$date_debut_3 >= input$date_fin_3){
          stop("La date début doit être avant la date fin")  # Arret si les dates ne sont pas cohérentes
        }
        
        # Définition du block ui 1
        output$selectInputUI1 <- renderUI({
          div(style = "justify-content: center;align-items: center",
              selectInput(
                inputId = "zone_select_psv2",
                label = " Selectionnez un point de surveillance:",
                choices =  NULL,
                width = "50%"
                )
              )
          }) # Fin de block ui 1
        
        # Définition du block ui 2
        output$selectInputUI2 <- renderUI({
          div(style = "justify-content: center;align-items: center",
            selectInput(
              inputId = "zone_select_kapta2",
              label = " Selectionnez une sonde kapta:",
              choices =  NULL,
              width = "50%"
              )
            )
          }) # Fin de block ui 2
      
      # Appel des fonction de creation de table et graphes (fichier : Seuil_chlore.R)
      output_seuil <- create_datatable(selected_input_dDébut3, selected_input_dFin3, selected_input_seuil, selected_input_upDown, selected_input_mesure)
      output_stat <- create_stats_table(selected_input_dDébut3, selected_input_dFin3, selected_input_seuil, selected_input_upDown, selected_input_mesure)
      
      req(output_seuil) # Attente de la variable output_seuil
      
      # Render des graphiques et tableaux
      output$data_table <- 
        DT::renderDataTable(output_seuil$table)
      output$stats_table <- 
        DT::renderDataTable(output_stat$table)
      output$graphique_pluie2 <-
        renderPlotly(create_plot_meteo(selected_input_dDébut3,selected_input_dFin3))  # (Fichier: Météo.R)

      # Maj des choix des filtres suivant le dataset
      updateSelectInput(session, "zone_select_psv2", choices = unique(output_seuil$set$Sectorisat))
      updateSelectInput(session, "zone_select_kapta2", choices = unique(output_seuil$set$ENDPOINTREF.x))
      
      # Render des graphiques et tableaux
      output$kapta_plot_output2 <-
        renderPlotly(create_plot(output_seuil$set, "KAPTA", input$zone_select_kapta2))
      output$PSV_plot_output2 <-
        renderPlotly(create_plot(output_seuil$set, "PSV", input$zone_select_psv2))
      
      }, error = function(err) {  # Catch error
        shinyalert(text = "La date début doit être avant la date fin")
        
      }) # Fin try catch
      
      }) # Fin du observed event
    
    # Evennemment observable: Click button 2
    observeEvent(input$goButton2, {
      tryCatch({
        
        # Récupère les inputs de filtres
        selected_input_dDébut <- input$Date_Début
        selected_input_dFin <- input$Date_Fin
        
        # Test de cohérences des dates 
        if(input$Date_Début >= input$Date_Fin){
          stop("La date début doit être avant la date fin")
        }
        
        # Render des carte et graphiques  
        output$histo_chlore_output <-
          renderPlotly(create_histo(selected_input_dDébut , selected_input_dFin)) # Fonction dans fichier Graphes.R
        output$box_chlore_output <-
          renderPlotly(create_box(selected_input_dDébut , selected_input_dFin))
        output$carte_output <- 
          renderLeaflet({create_leafMap(selected_input_dDébut , selected_input_dFin)}) # Fonction dans Carto_2.R
        
        }, error = function(err){ # Catch error
          shinyalert(text = "La date début doit être avant la date fin")
          
        })# Fin trycatch
      
      }) # Fin du observed event
    
    # Evennemment observable: Click button 3
    observeEvent(input$goButton3, {
      
      tryCatch({
        
        # Récupère les inputs de filtre
        selected_input_dDébut2 <- input$Date_Début_2
        selected_input_dFin2 <- input$Date_Fin_2
        selected_input_zone_kapta <- input$zone_select_kapta
        selected_input_zone_psv <- input$zone_select_psv
      
        #Test de la cohérence des dates
        if(input$Date_Début_2 >= input$Date_Fin_2){
          stop("La date début doit être avant la date fin")
        }
      
        # Render des graphiques (fichier: Kapta-PSV.R)
        output$graphique_pluie1 <-
          renderPlotly(create_plot_meteo(selected_input_dDébut2,selected_input_dFin2))  # (Fichier: Météo.R)
        output$kapta_plot_output <-
          renderPlotly(create_kapta_plot(selected_input_dDébut2 , selected_input_dFin2, selected_input_zone_kapta))
        output$PSV_plot_output <-
          renderPlotly(create_PSV_plot(selected_input_dDébut2 , selected_input_dFin2, selected_input_zone_psv))
        
        }, error = function(err){ # Catch error
        shinyalert(text = "La date début doit être avant la date fin")
        
          }) # Fin du try catch
      
      }) # Fin du observed event
    
    resultats <- reactive({
      seuil <- input$seuil
      mois <- input$mois
      annees <- input$annees
      secteur <- input$secteur
      recherche <- input$recherche
      up_down <- input$up_down2
      subset_data <- create_seuil_table(seuil, mois, annees, secteur, recherche, up_down)
      return(subset_data)
    })
    
    # Affichage des résultats filtrés
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
      return(create_occurence_table(new_data))
      })
    
    # Affichage des occurrences
    output$occurrences <- renderTable({
      occurrences()[[1]]
    })
    }, error = function(err) {
      "Pas de données pour ces paramètres"
    }
    )
 
  } # Fin de serveur logique
  
  # Run the app
  shinyApp(ui, server)
  # runApp(list(ui=ui, server=server), host="10.165.8.60", port=5050)
