plot_temp <- function(data,
                      titre_geo = "France",
                      granularite_temps,
                      temp_choix) {
  
  colonne_y <- switch(
    temp_choix,
    "Temperature max" = "Temperature_max",
    "Temperature moy" = "Temperature_moyenne",
    "Temperature min" = "Temperature_min",
    "Tout"            = "Temperature_moyenne"
  )
  
  p <- ggplot(data, aes(x = periode, y = .data[[colonne_y]]))
  
  if (temp_choix == "Tout") {
    p <- p + geom_ribbon(
      aes(ymin = Temperature_min, ymax = Temperature_max),
      fill = "#3498db",
      alpha = 0.2
    )
  } else if (granularite_temps == "annee") {
    # Lissage seulement si on est en vue annuelle
    p <- p + geom_smooth(
      color = "#e74c3c",
      fill  = "#f58f3c",
      alpha = 0.1,
      linewidth = 1,
      linetype = "dotted",
      se = TRUE
    )
  }
  
  p + geom_line(color = "#2980b9", linewidth = 1.2) +
    theme_minimal(base_size = 14) +
    labs(
      title = paste("🌡️ Évolution :", titre_geo),
      subtitle = paste("Vue :", granularite_temps),
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


plot_prec <- function(data,
                      titre_geo = "France",
                      granularite_temps) {
  
  ggplot(data, aes(x = periode, y = Precipitation_mm_moy)) +
    geom_col(fill = "steelblue") +
    theme_minimal(base_size = 14) +
    labs(
      title = paste("🌧 Précipitations :", titre_geo),
      subtitle = paste("Vue :", granularite_temps),
      y = "Cumul (mm)",
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