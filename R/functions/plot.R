plot_temp <- function(data,
                      niveau_geo,
                      input_geo,
                      granularite_temps,
                      temp_choix) {
  data_mieux <- aggregate_meteo(data, granularite_temps, niveau_geo, input_geo)
  
  shiny::validate(
    shiny::need(
      nrow(data_mieux) > 0,
      "Aucune donnée météo ne correspond à votre sélection géographique et temporelle."
    )
  )
  
  titre_lieu <- if (is.null(input_geo) ||
                    is.na(input_geo))
    "France entière"
  else
    input_geo
  
  colonne_y <- switch(
    temp_choix,
    "Temperature max" = "Temperature_max",
    "Temperature moy" = "Temperature_moyenne",
    "Temperature min" = "Temperature_min",
    "Tout"            = "Temperature_moyenne"
  )
  
  p <- ggplot(data_mieux, aes(x = periode))
  
  if (temp_choix == "Tout") {
    p <- p + geom_ribbon(
      aes(ymin = Temperature_min, ymax = Temperature_max),
      fill = "#3498db",
      alpha = 0.2
    )
  } else if (granularite_temps == "annee") {
    p <- p + geom_smooth(
      aes(y = .data[[colonne_y]]),
      color = "#e74c3c",
      fill  = "#f58f3c",
      alpha = 0.1,
      linewidth = 1,
      linetype = "dotted",
      method = mgcv::gam(y ~ s(x, bs="cs")),
      se = TRUE
    )
  }
  p <- p + geom_line(aes(y = .data[[colonne_y]]),
                     color = "#2980b9",
                     linewidth = 1.2)
  
  p <- p +  theme_minimal(base_size = 14) +
    labs(
      title = paste("🌡️ Évolution des températures :", titre_lieu),
      subtitle = paste(
        "Vue :",
        granularite_temps,
        "| La zone colorée représente l'amplitude Min/Max"
      ),
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
  
  return(p)
}


plot_prec <- function(data,
                      niveau_geo,
                      input_geo,
                      granularite_temps) {
  data_mieux <- aggregate_meteo(data, granularite_temps, niveau_geo, input_geo)
  
  shiny::validate(
    shiny::need(
      nrow(data_mieux) > 0,
      "Aucune donnée météo ne correspond à votre sélection géographique et temporelle."
    )
  )
  
  titre_lieu <- if (is.null(input_geo) ||
                    is.na(input_geo))
    "France entière"
  else
    input_geo
  
  ggplot(data_mieux, aes(x = periode, y = Precipitation_mm_moy)) +
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