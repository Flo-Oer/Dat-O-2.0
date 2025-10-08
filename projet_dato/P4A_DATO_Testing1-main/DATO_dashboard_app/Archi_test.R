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
library(raster)
# library(lintr)
# library(semantic.dashboard)

source("./Carto_2.R")
source("./Graphes.R")
source("./Kapta-PSV.R")
source("./Seuil_chlore.R")

ui <- fluidPage(
  # Page title
  tags$head(tags$style(
    HTML(
      "
      #sidebar {
        position: relative;
        top: 0px;
        overflow-y: auto;
        width: 200px;
        padding: 10;
        background-color: #f8f8f8;
        border-right: 1px solid #ccc;
      }
    "
    )
  ),
  tags$script(
    HTML(
      '$(document).ready(function() {
               var height = $("#histo_chlore_output").height() + $("#box_chlore_output").height();
               $("#carte_output").height(height);
            });'
    ),
    HTML(
      '
  $(document).ready(function() {
        var sidebarTop = $("#sidebar").offset().top;
        $(window).scroll(function() {
          var scrollTop = $(window).scrollTop();
          if (scrollTop >= sidebarTop) {
            $("#sidebar").css({
              position: "fixed",
              top: "10px"
            });
          } else {
            $("#sidebar").css({
              position: "relative",
              top: "60x"
            });
          }
        });
      });
    '
    )
  )),
  titlePanel("My Dashboard"),
  
  #Navigation bar
  navbarPage("DaTo",
             
             #Second tab
             tabPanel(
               "Main Page",
               
               # Sidebar layout
               sidebarLayout(
                 # Sidebar panel
                 sidebarPanel(
                   width = 2,
                   id = "sidebar",
                   
                   
                   #Vertical tabs layout
                   tabsetPanel(
                     id = "tabs",
                     #Vertical tabs panel
                     
                     tabPanel("Contextualisation & objectifs"),
                     
                     tabPanel("Data Visualisation"),
                     
                     tabPanel("KAPTA/PSV")
                     
                   ),
                   
                   # Main panel
                   
                   
                   
                 ),
                 mainPanel(
                   width = 10,
                   conditionalPanel(condition = "input.tabs == 'Contextualisation & objectifs'",
                                    numericInput("seuil_select", "Seuil de concentration de chlore:", 0.05, min = 0, max = 1, step = 0.01),
                                    DT::dataTableOutput("data_table")
                                    ),
                   conditionalPanel(condition = "input.tabs == 'Data Visualisation'",
                                    #tab layout
                                    tabsetPanel(
                                      tabPanel(
                                        "PSV",
                                        #Drop down select
                                        br(),
                                        
                                        dropdownButton(
                                          tags$h3("Liste des paramètres"),
                                          
                                          selectInput(
                                            inputId = "Date_Début",
                                            label = " Selectionnez une date de début:",
                                            choices = NULL ,
                                            width = "100%"
                                          ),
                                          selectInput(
                                            inputId = "Date_Fin",
                                            label = " Selectionnez une date de fin:",
                                            choices = NULL,
                                            width = "100%"
                                          ),
                                          awesomeCheckboxGroup(
                                            "Secteur",
                                            "Selectionnez un secteur:",
                                            choices = unique(cd_bar2$SECTORISAT[3]),
                                            inline = TRUE
                                          ),
                                          
                                          circle = TRUE,
                                          status = "info",
                                          icon = icon("gear"),
                                          width = "300px",
                                          
                                          tooltip = tooltipOptions(title = "Cliquez pour modifier les paramètres!")
                                        ),
                                        br(),
                                        fluidRow(
                                          column(
                                            4,
                                            br(),
                                            #h3("plot1(PSV)", align="center", style = "color:teal")
                                            plotOutput("histo_chlore_output"),
                                            br(),
                                            plotOutput("box_chlore_output")
                                          ),
                                          column(
                                            7,
                                            h3("Carte des canalisations", align =
                                                 "center", style = "color:teal"),
                                            leafletOutput("carte_output")
                                            
                                          )
                                          
                                          
                                          # dropdownButton(
                                          #
                                          #   tags$h3("List of Inputs"),
                                          #
                                          #   selectInput(inputId = 'xcol',
                                          #               label = 'X Variable',
                                          #               choices = names(iris)),
                                          #
                                          #   circle = TRUE, status = "danger",
                                          #   icon = icon("gear"), width = "300px",
                                          #
                                          #   tooltip = tooltipOptions(title = "Click to see inputs !")
                                          # ),
                                          
                                          
                                          
                                        )
                                        
                                      ),
                                      
                                      tabPanel("KAPTA"
                                               # img(
                                               #   src = "image_1.jpg",
                                               #   height = 200,
                                               #   width = 300)
                                               ),
                                               tabPanel("DASHBOARD SEUIL"),
                                               
                                      )),
                                    conditionalPanel(
                                      condition = "input.tabs == 'KAPTA/PSV'",
                                      fluidRow(column(
                                        4,
                                        selectInput(
                                          inputId = "Date_Début_2",
                                          label = " Selectionnez une date de début:",
                                          choices = NULL ,
                                          width = "100%"
                                        ),
                                        selectInput(
                                          inputId = "Date_Fin_2",
                                          label = " Selectionnez une date de fin:",
                                          choices = NULL,
                                          width = "100%"
                                        )
                                      ),
                                      column(8,
                                             selectInput(
                                               inputId = "zone_select",
                                               label = " Selectionnez une zone:",
                                               choices = NULL,
                                               width = "100%"
                                             )
                                             )
                                      ),
                                      
                                      tabsetPanel(tabPanel("PSV_1",
                                                           plotOutput("PSV_plot_output")
                                                           ),
                                                  tabPanel("KAPTA_1",
                                                           plotOutput("kapta_plot_output"))
                                                  )
                                      
                                      
                                      
                                      
                                    )
                   )
                 )
                 
                 
                 
                 
               ))
  )
  
  # Define server logic
  server <- function(input, output, session) {
    output$selected_variable <- renderText({
      paste("Vous avez sélectionné la ", input$variable)
    })
    
    selected_input_dDébut <- reactive({
      return(input$Date_Début)
    })
    selected_input_dFin <- reactive({
      return(input$Date_Fin)
    })
    
    selected_input_dDébut2 <- reactive({
      return(input$Date_Début_2)
    })
    selected_input_dFin2 <- reactive({
      return(input$Date_Fin_2)
    })
    selected_input_zone <- reactive({
      return(input$zone_select)
    })
    selected_input_seuil <- reactive({
      return(input$seuil_select)
    })
    data <- reactive({
    df_filtered <- df[df$`Concentration chlore 2 (mg/L)` < selected_input_seuil(), c("DATE", "HEURE", "RFID", "Concentration chlore 1 (mg/L)", "Concentration chlore 2 (mg/L)", "Latitude", "Longitude")]
    
    # Calculer la moyenne des concentrations de chlore 1 et 2
    df_filtered$Moyenne_chlore <- (df_filtered$`Concentration chlore 1 (mg/L)` + df_filtered$`Concentration chlore 2 (mg/L)`) / 2
    
    # Réorganiser les colonnes
    df_filtered <- df_filtered %>% dplyr::select(DATE, HEURE, RFID, `Concentration chlore 1 (mg/L)`, `Concentration chlore 2 (mg/L)`, Moyenne_chlore, Latitude, Longitude)
    return(df_filtered)
    })
    
    output$histo_chlore_output <-
      renderPlot(create_histo(selected_input_dDébut() , selected_input_dFin()))
    
    output$box_chlore_output <-
      renderPlot(create_box(selected_input_dDébut() , selected_input_dFin()))
    
    output$carte_output <- renderLeaflet({
      create_leafMap(selected_input_dDébut() , selected_input_dFin())
    })
    
    output$kapta_plot_output <-
      renderPlot(create_kapta_plot(selected_input_dDébut2() , selected_input_dFin2()))
    
    output$PSV_plot_output <-
      renderPlot(create_PSV_plot(selected_input_dDébut2() , selected_input_dFin2(), selected_input_zone()))
    
    output$data_table <- 
      DT::renderDataTable(create_datatable(data()))
    
    choices_donnees <<- seq(as.Date("2018-01-01"), Sys.Date(), by = "day")
    choices_secteur <<- unique(position_PSV$Sectorisat)
    
    # Update choices for selectInput
    updateSelectInput(session,
                      "Date_Fin",
                      choices = choices_donnees ,
                      selected = "2021-06-15")
    updateSelectInput(session,
                      "Date_Début",
                      choices = choices_donnees ,
                      selected = "2021-03-01")
    
    updateSelectInput(session,
                      "Date_Fin_2",
                      choices = choices_donnees ,
                      selected = "2021-06-15")
    updateSelectInput(session,
                      "Date_Début_2",
                      choices = choices_donnees ,
                      selected = "2021-03-01")
    updateSelectInput(session,
                      "zone_select",
                      choices = choices_secteur ,
                    )
    
  }
  # Run the app
  shinyApp(ui, server)
  