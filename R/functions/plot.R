plot_temp <- function(data, niveau_geo, input_geo, granularite_temps){
  
  # 1. Agrégation des données
  data_mieux <- aggregate_meteo(data, granularite_temps, niveau_geo, input_geo)
  
  # 2. Validation
  shiny::validate(
    shiny::need(
      nrow(data_mieux) > 0, 
      "Aucune donnée météo ne correspond à votre sélection géographique et temporelle."
    )
  )
  
  # 3. Gestion du Titre (si input_geo est NA, on met "France")
  titre_lieu <- if (is.null(input_geo) || is.na(input_geo)) "France entière" else input_geo
  
  # 4. Création du graphique
  ggplot(data_mieux, aes(x = periode)) +
    
    # A. Zone d'amplitude (Min - Max) en arrière-plan
    geom_ribbon(aes(ymin = Temperature_min, ymax = Temperature_max), 
                fill = "#3498db", alpha = 0.2) +
    
    NULL +
    
    # B. Ligne de Moyenne
    geom_line(aes(y = Temperature_moyenne), 
              color = "#2980b9", linewidth = 1.2) +
    
    # C. Thème et Esthétique
    theme_minimal(base_size = 14) +
    labs(
      title = paste("🌡️ Évolution des températures :", titre_lieu),
      subtitle = paste("Vue :", granularite_temps, "| La zone colorée représente l'amplitude Min/Max"),
      y = "Température (°C)",
      x = NULL,
      caption = "Source: Météo-France"
    ) +
    theme(
      plot.title = element_text(face = "bold", color = "#2c3e50"),
      plot.subtitle = element_text(size = 10, color = "#7f8c8d"),
      axis.text = element_text(color = "#2c3e50"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(linetype = "dotted", color = "gray80")
    )
}

plot_prec <- function(data,  niveau_geo, input_geo, granularite_temps){
  # 1. Agrégation des données
  data_mieux <- aggregate_meteo(data, granularite_temps, niveau_geo, input_geo)
  
  # 2. Validation
  shiny::validate(
    shiny::need(
      nrow(data_mieux) > 0, 
      "Aucune donnée météo ne correspond à votre sélection géographique et temporelle."
    )
  )
  
  # 3. Gestion du Titre (si input_geo est NA, on met "France")
  titre_lieu <- if (is.null(input_geo) || is.na(input_geo)) "France entière" else input_geo
  
  # 4. Création du graphique
  ggplot(data_mieux, aes(x = periode, y=Precipitation_mm_moy)) +
    geom_col(fill = "steelblue") +
    theme_minimal(base_size = 14) +
    labs(
      title = paste("🌧 Évolution des précipitation :", titre_lieu),
      subtitle = paste("Vue :", granularite_temps),
      y = "Précipitation (mm)",
      x = NULL,
      caption = "Source: Météo-France"
    ) +
    theme(
      plot.title = element_text(face = "bold", color = "#2c3e50"),
      plot.subtitle = element_text(size = 10, color = "#7f8c8d"),
      axis.text = element_text(color = "#2c3e50"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(linetype = "dotted", color = "gray80")
    )
}