# Function to filter COT data
filter_cot_data <- function(start_date, end_date) {
  df <- psv_data %>%
    filter(
      Parametre == "COT (1305)",
      Date.de.prelevement >= start_date,
      Date.de.prelevement <= end_date
    ) %>%
    arrange(Date.de.prelevement)
  
  return(df)
}

# Function to create COT plot with alert threshold
create_cot_plot <- function(start_date, end_date) {
  df <- filter_cot_data(start_date, end_date)
  
  if (nrow(df) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, 
               label = "Aucune donnée COT disponible pour cette période", 
               size = 6, color = "gray50") +
      theme_void()
    return(ggplotly(p))
  }
  
  # Calculate daily averages
  df_agg <- df %>%
    group_by(Date.de.prelevement, Sectorisat) %>%
    summarise(
      COT_mean = mean(Resultat, na.rm = TRUE),
      COT_max = max(Resultat, na.rm = TRUE),
      COT_min = min(Resultat, na.rm = TRUE),
      n_mesures = n(),
      .groups = "drop"
    )
  
  # Create plot
  p <- ggplot(df_agg, aes(x = Date.de.prelevement, y = COT_mean)) +
    geom_line(aes(color = Sectorisat), linewidth = 1.2) +
    geom_point(aes(color = Sectorisat), size = 2) +
    geom_hline(yintercept = 2, color = "red", linetype = "dashed", linewidth = 1) +
    annotate("text", 
             x = min(df_agg$Date.de.prelevement) + 
                 (max(df_agg$Date.de.prelevement) - min(df_agg$Date.de.prelevement)) * 0.02,
             y = 2.15, 
             label = "Seuil d'alerte (2 mg/L)", 
             color = "red", 
             hjust = 0,
             size = 3.5) +
    labs(
      title = "Évolution du Carbone Organique Total (COT)",
      x = "Date",
      y = "COT (mg/L)",
      color = "Secteur"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  ggplotly(p, tooltip = c("x", "y", "color")) %>%
    layout(
      hovermode = "x unified",
      hoverlabel = list(bgcolor = "white")
    )
}

# Function to check for COT alerts
check_cot_alerts <- function(start_date, end_date, threshold = 2) {
  df <- filter_cot_data(start_date, end_date)
  
  if (nrow(df) == 0) {
    return(NULL)
  }
  
  alerts <- df %>%
    filter(Resultat > threshold) %>%
    arrange(desc(Date.de.prelevement))
  
  if (nrow(alerts) > 0) {
    return(list(
      has_alert = TRUE,
      n_alerts = nrow(alerts),
      max_value = max(alerts$Resultat, na.rm = TRUE),
      last_alert_date = max(alerts$Date.de.prelevement),
      last_alert_sector = alerts$Sectorisat[which.max(alerts$Date.de.prelevement)],
      alert_data = alerts
    ))
  } else {
    return(list(has_alert = FALSE))
  }
}

# Function to create chlorine plot from Kapta data for Super Rimiez
create_chlore_super_rimiez_plot <- function(start_date, end_date) {
  # Filter for Super Rimiez site - adjust ENDPOINTREF based on your data
  # You may need to check: unique(donnees_sondes$Site) or unique(donnees_sondes$ENDPOINTREF)
  
  df <- donnees_sondes %>%
    filter(
      grepl("Super Rimiez|SUPER RIMIEZ|Rimiez", Site, ignore.case = TRUE) |
      ENDPOINTREF %in% c(1, 2, 3), # Adjust these IDs based on your Super Rimiez sensors
      DATE >= start_date,
      DATE <= end_date
    )
  
  if (nrow(df) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, 
               label = "Données de chlore Super Rimiez non disponibles\n(Vérifier le nom du site ou ENDPOINTREF)", 
               size = 5, color = "gray50") +
      theme_void()
    return(ggplotly(p))
  }
  
  # Aggregate by date
  df_agg <- df %>%
    group_by(DATE, ENDPOINTREF) %>%
    summarise(
      Chlore1 = mean(`Concentration chlore 1 (mg/L)`, na.rm = TRUE),
      Chlore2 = mean(`Concentration chlore 2 (mg/L)`, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = c(Chlore1, Chlore2),
      names_to = "Capteur",
      values_to = "Concentration"
    ) %>%
    mutate(
      Capteur_Label = paste0(Capteur, " (Sonde ", ENDPOINTREF, ")")
    )
  
  p <- ggplot(df_agg, aes(x = DATE, y = Concentration, color = Capteur_Label)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = "Chlore en sortie d'usine de Super Rimiez",
      x = "Date",
      y = "Chlore (mg/L)",
      color = "Capteur"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  ggplotly(p, tooltip = c("x", "y", "color")) %>%
    layout(hovermode = "x unified")
}

# Function to create turbidity plot
create_turbidity_plot <- function(start_date, end_date) {
  # Check if turbidity data exists in PSV data
  df <- psv_data %>%
    filter(
      grepl("Turbidité|Turbidite|TURBIDITE", Parametre, ignore.case = TRUE),
      grepl("Saint-Jean|St Jean|SAINT JEAN", Sectorisat, ignore.case = TRUE),
      Date.de.prelevement >= start_date,
      Date.de.prelevement <= end_date
    )
  
  if (nrow(df) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, 
               label = "Données de turbidité non disponibles\npour Saint-Jean-la-Rivière", 
               size = 5, color = "gray50") +
      theme_void()
    return(ggplotly(p))
  }
  
  df_agg <- df %>%
    group_by(Date.de.prelevement) %>%
    summarise(
      Turbidite = mean(Resultat, na.rm = TRUE),
      .groups = "drop"
    )
  
  p <- ggplot(df_agg, aes(x = Date.de.prelevement, y = Turbidite)) +
    geom_line(color = "#8B4513", linewidth = 1.2) +
    geom_point(color = "#8B4513", size = 2) +
    labs(
      title = "Turbidité à la prise d'eau de Saint-Jean-la-Rivière",
      x = "Date",
      y = "Turbidité (NTU)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  ggplotly(p, tooltip = c("x", "y")) %>%
    layout(hovermode = "x unified")
}
