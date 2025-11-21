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

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Seuil", tabName = "seuil", icon = icon("dashboard")),
      menuItem("Statistiques", tabName = "stats", icon = icon("bar-chart-o"))  # Ajout du nouvel onglet de statistiques
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "seuil",
              fluidRow(
                column(width = 8,
                       box(
                         title = "Sélection du seuil", status = "primary", solidHeader = TRUE,
                         numericInput("seuil", "Seuil de concentration de chlore:", 0.05, min = 0, max = 1, step = 0.01),
                         selectInput("selected_month", "Choisissez le mois :",
                                     choices = NULL, selected = NULL), # sera mis à jour dans server
                         radioButtons("choice", "Choisissez une option:",
                                      choices = list("Inférieur" = "Inférieur", "Supérieur" = "Supérieur"),
                                      selected = "Inférieur")
                       )),
                column(width = 12,
                       box(
                         title = "Données", status = "primary", solidHeader = TRUE,
                         DT::dataTableOutput("table")  # Change to DT::dataTableOutput
                       ))
              )),
      # Ajout du nouvel onglet de statistiques
      tabItem(tabName = "stats",
              fluidRow(
                column(width = 12,
                       box(
                         title = "Statistiques", status = "primary", solidHeader = TRUE,
                         DT::dataTableOutput("stats_table")  
                       ))
              ))
    )
  )
)

server <- function(input, output, session) {
  
  options(digits = 10)
  
  df1 <- read_delim("C:/Users/osouidi/Desktop/4A/DATO/MasterKaptaV2.csv", delim = ";")
  df2 <- read_delim("C:/Users/osouidi/Desktop/4A/DATO/position-sondes-global.csv", delim = ";")
  
  df1$`Concentration chlore 2 (mg/L)` <- gsub(",", ".", df1$`Concentration chlore 2 (mg/L)`)
  df1$`Concentration chlore 1 (mg/L)` <- gsub(",", ".", df1$`Concentration chlore 1 (mg/L)`)
  
  df2$Latitude <- as.numeric(paste0(substr(df2$Latitude, 1, nchar(df2$Latitude)-6), ".", substr(df2$Latitude, nchar(df2$Latitude)-5, nchar(df2$Latitude))))
  df2$Longitude <- as.numeric(paste0(substr(df2$Longitude, 1, nchar(df2$Longitude)-5), ".", substr(df2$Longitude, nchar(df2$Longitude)-4, nchar(df2$Longitude))))
  
  df1 <- df1 %>% mutate(`Concentration chlore 2 (mg/L)` = as.numeric(`Concentration chlore 2 (mg/L)`), `Concentration chlore 1 (mg/L)` = as.numeric(`Concentration chlore 1 (mg/L)`))
  
  df <- merge(df1, df2, by = "RFID")
  
  df$DATE <- as.Date(df$DATE, format = "%d/%m/%Y")
  df$YearMonth <- format(df$DATE, "%Y-%m")
  
  observe({
    updateSelectInput(session, "selected_month", choices = sort(unique(df$YearMonth), decreasing = TRUE), selected = sort(unique(df$YearMonth), decreasing = TRUE)[1])
  })
  
  output$table <- DT::renderDataTable({
    df_selected <- df[format(df$DATE, "%Y-%m") == input$selected_month,]
    if (input$choice == "Inférieur") {
      df_selected <- df_selected[df_selected$`Concentration chlore 2 (mg/L)` < input$seuil,]
    } else {
      df_selected <- df_selected[df_selected$`Concentration chlore 2 (mg/L)` > input$seuil,]
    }
    df_selected$Moyenne_chlore <- (df_selected$`Concentration chlore 1 (mg/L)` + df_selected$`Concentration chlore 2 (mg/L)`) / 2
    datatable(df_selected,
              options = list(
                columnDefs = list(
                  list(width = '100px', targets = c(1,2,3)),
                  list(width = '200px', targets = c(4,5,6))
                ),
                scrollX = TRUE)
    ) %>% formatStyle(
      'Moyenne_chlore',
      target = 'row',
      color = styleEqual(c(0), c('red'))
    )
  })
  
  output$stats_table <- DT::renderDataTable({
    df_selected <- df[format(df$DATE, "%Y-%m") == input$selected_month,]
    if (input$choice == "Inférieur") {
      df_selected <- df_selected[df_selected$`Concentration chlore 2 (mg/L)` < input$seuil,]
    } else {
      df_selected <- df_selected[df_selected$`Concentration chlore 2 (mg/L)` > input$seuil,]
    }
    df_stats <- df_selected %>% group_by(RFID, YearMonth) %>% summarise(n = n())
    datatable(df_stats, options = list(scrollX = TRUE))
  })
}

shinyApp(ui = ui, server = server)
