library(randomForest)
library(nnet)
library(caret)
library(dplyr)
library(readxl)

# ============================================================================
# PREDICTION FUNCTION FOR VÉSUBIE
# ============================================================================
predict_sulfate_vesubie <- function(uploaded_file) {

  data_new <- readxl::read_excel(uploaded_file)

  rf_model <- readRDS("models/vesubie_rf_cluster.rds")
  nn_model <- readRDS("models/vesubie_nnet.rds")
  scaler   <- readRDS("models/vesubie_scaler.rds")

  # SAME columns as training
  cols <- c(
    "temperature",
    "Conductivité.µS.cm",
    grep("^cumul_glissant_", names(data_new), value = TRUE)
  )

  # cluster prediction
  data_new$groupe <- predict(rf_model, data_new[, cols])

  # scaling
  X_new <- scale(
    data_new[, cols],
    center = scaler$means,
    scale  = scaler$sds
  )

  # sulfate prediction
  data_new$pred_sulfate <- predict(nn_model, X_new)

  # alert logic (SAME as training)
  data_new$alert_level <- ifelse(
    data_new$groupe == 1,
    ifelse(data_new$pred_sulfate > 200, 2,
           ifelse(data_new$pred_sulfate >= 180, 1, 0)),
    0
  )

  return(list(
  success = TRUE,
  data = data_new,
  summary = list(
    n_critical = sum(data_new$alert_level == 2, na.rm = TRUE),
    n_warning  = sum(data_new$alert_level == 1, na.rm = TRUE),
    n_safe     = sum(data_new$alert_level == 0, na.rm = TRUE),
    max_concentration = max(data_new$pred_sulfate, na.rm = TRUE)
  )
))

}

# ============================================================================
# PREDICTION FUNCTION FOR JOSEPH RAYBAUD
# ============================================================================
# predict_sulfate_raybaud <- function(uploaded_file) {
#   tryCatch({
#     # Similar structure to Vésubie
#     # Load Raybaud-specific models
#     rf_model <- readRDS("models/raybaud_rf_cluster.rds")
#     nn_model <- readRDS("models/raybaud_nnet.rds")
#     scaler <- readRDS("models/raybaud_scaler.rds")
    
#     # ... (same prediction logic adapted for Raybaud)
    
#     return(list(
#       success = TRUE,
#       data = data_new,
#       summary = summary_stats
#     ))
    
#   }, error = function(e) {
#     return(list(
#       success = FALSE,
#       error = paste("Erreur Raybaud:", e$message)
#     ))
#   })
# }

# ============================================================================
# FORMAT OUTPUT FOR UI
# ============================================================================
format_prediction_output <- function(prediction_result) {
  if (!prediction_result$success) {
    return(tags$div(
      class = "alert alert-danger",
      icon("times-circle"),
      " ", prediction_result$error
    ))
  }
  
  summary <- prediction_result$summary
  
  # Risk level indicator
  risk_class <- if (summary$n_critical > 0) {
    "danger"
  } else if (summary$n_warning > 0) {
    "warning"
  } else {
    "success"
  }
  
  risk_text <- if (summary$n_critical > 0) {
    "RISQUE CRITIQUE"
  } else if (summary$n_warning > 0) {
    "ATTENTION"
  } else {
    "PAS DE RISQUE"
  }
  
  tagList(
    # Risk banner
    tags$div(
      class = paste("alert alert-", risk_class),
      style = "padding: 20px; margin-bottom: 20px; font-size: 18px;",
      tags$strong(risk_text)
    ),
    
    # Statistics cards
    fluidRow(
      column(4,
             wellPanel(
               style = "background-color: #d9534f; color: white; text-align: center;",
               h3(summary$n_critical, style = "margin: 0;"),
               p("Dépassements critiques", style = "margin: 5px 0 0 0;"),
               p("> 200 mg/L", style = "font-size: 12px; margin: 0;")
             )
      ),
      column(4,
             wellPanel(
               style = "background-color: #f0ad4e; color: white; text-align: center;",
               h3(summary$n_warning, style = "margin: 0;"),
               p("Avertissements", style = "margin: 5px 0 0 0;"),
               p("180-200 mg/L", style = "font-size: 12px; margin: 0;")
             )
      ),
      column(4,
             wellPanel(
               style = "background-color: #5cb85c; color: white; text-align: center;",
               h3(summary$n_safe, style = "margin: 0;"),
               p("Mesures normales", style = "margin: 5px 0 0 0;"),
               p("< 180 mg/L", style = "font-size: 12px; margin: 0;")
             )
      )
    ),
    
    # Maximum concentration
    if (summary$max_concentration >= 180) {
      tags$div(
        class = "alert alert-info",
        style = "margin-top: 15px;",
        tags$strong("Concentration maximale prédite: "),
        tags$span(
          paste0(round(summary$max_concentration, 1), " mg/L"),
          style = "font-size: 20px; color: #d9534f;"
        )
      )
    },
    
    # Data table
    h4("Détail des prédictions", style = "margin-top: 30px; color: #337ab7;"),
    DT::dataTableOutput("prediction_table")
  )
}