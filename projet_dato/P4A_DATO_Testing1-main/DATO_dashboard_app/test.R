library(shiny)
library(semantic.dashboard)
library(DT)
library(lintr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(sf)
library(readxl)

position_PSV <- st_read("./www/Donnees_Dato/Loc_PSV_V4/Loc_PSV_V4.shx")
donnees_PSV <- read_excel("./www/Dato donnees clean/donnees_PSV.xlsx")
donnees_sondes <- read_excel("./www/Dato donnees clean/MasterKaptaV2.xlsx")
PSV_pivoted <- pivot_wider(donnees_PSV, names_from = Parametre, values_from = Resultat)
merged_data <- merge(position_PSV, PSV_pivoted, by = "Numero")

ui <- dashboardPage(theme = "slate",
                    dashboardHeader(title = "My dashboard"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Dat'O", tabName = "dato"),
                        menuItem("Sensor2", tabName = "sensor2")
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem("dato",
                                dateRangeInput("date_range", "Selectionner la plage de dates",
                                               start = "2018-01-01", end = Sys.Date(),
                                               min = "2018-01-01", max = Sys.Date(),
                                               format = "yyyy/mm/dd"),
                                box(plotOutput("dato_data"), width = 8)
                                
                        ),
                        tabItem("sensor2",
                                dateRangeInput("date_range_psv", "Selectionner la plage de dates",
                                               start = "2018-01-01", end = Sys.Date(),
                                               min = "2018-01-01", max = Sys.Date(),
                                               format = "yyyy/mm/dd"),
                                uiOutput("zone_select"),
                                box(plotOutput("sensor2_data"), width = 8))
                      )
                    )
)

server <- function(input, output) {
  
  #Onglet Dat'O
  output$dato_data <- renderPlot({
    if (!is.null(input$date_range)) {
      filtered_data <- subset(donnees_sondes, DATE >= as.POSIXct(input$date_range[1], format = "%Y-%m-%d") & DATE <= as.POSIXct(input$date_range[2], format = "%Y-%m-%d"))
    } else {
      filtered_data <- donnees_sondes
    }
    ggplot(filtered_data, aes(x = as.POSIXct(filtered_data$DATEREF), y = filtered_data$`Concentration chlore 2 (mg/L)`)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = "Date", y = "Concentration en chlore (mg/L)", title = "Concentration en chlore (mg/L)") +
      theme_minimal()
  })
  
  
  # Onglet Sensor2
  output$sensor2_data <- renderPlot({
    debut <- as.POSIXct(input$date_range_psv[1])
    fin <- as.POSIXct(input$date_range_psv[2])
    choix_zone <- input$zone 
    
    filtered_data_PSV <- merged_data %>%
      filter(Date.de.prelevement >= debut & Date.de.prelevement <= fin, !is.na(`Cl2 libre (1398)`), Sectorisat == choix_zone) %>%
      dplyr::select(Date.de.prelevement, `Cl2 libre (1398)`)
    
    ggplot(filtered_data_PSV, aes(x = Date.de.prelevement, y = `Cl2 libre (1398)`)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = "Date", y = "Concentration en chlore (mg/L)", title = "Concentration en chlore (mg/L)") +
      theme_minimal()
  })
  
  output$zone_select <- renderUI({
    selectInput("zone", "Selectionner la zone:", choices = unique(merged_data$Sectorisat))
  })
}

shinyApp(ui, server)