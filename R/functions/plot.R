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

#' Génère la carte Leaflet interactive
plot_map_leaflet <- function(data_map, 
                             var_type = "Temperature", 
                             temp_type = "Temperature moy", 
                             col_name_region) {
  
  # 1. Configuration selon la variable (Température ou Pluie)
  if (var_type == "Temperature") {
    # Choix de la colonne spécifique
    col_val <- switch(temp_type,
                      "Temperature max" = "Temperature_max",
                      "Temperature min" = "Temperature_min",
                      "Temperature moy" = "Temperature_moyenne",
                      "Temperature_moyenne") # Valeur par défaut
    
    palette_name <- "RdYlBu"
    is_reverse   <- TRUE   # Rouge = Chaud, Bleu = Froid
    unit_label   <- "°C"
    title_legend <- paste0(temp_type, " (°C)")
    
  } else {
    # Cas Précipitations
    col_val      <- "Precipitation_mm_moy"
    palette_name <- "Blues"
    is_reverse   <- FALSE  # Bleu clair = sec, Bleu foncé = humide
    unit_label   <- "mm"
    title_legend <- "Précipitations (mm)"
  }
  
  # 2. Création de la palette de couleurs
  # On extrait les valeurs pour définir le domaine (min/max)
  vals <- data_map[[col_val]]
  
  pal <- colorNumeric(
    palette  = palette_name, 
    domain   = vals, 
    reverse  = is_reverse, 
    na.color = "#808080"
  )
  
  # 3. Construction de la carte
  leaflet(data_map) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      fillColor   = ~pal(get(col_val)),
      color       = "#2c3e50", 
      weight      = 1, 
      opacity     = 1, 
      fillOpacity = 0.6,
      # Tooltip dynamique : Nom + Valeur + Unité
      label       = ~paste0(get(col_name_region), ": ", round(get(col_val), 1), " ", unit_label),
      highlightOptions = highlightOptions(
        weight = 3, 
        color = "#e74c3c", 
        bringToFront = TRUE
      )
    ) %>%
    addLegend(
      pal      = pal, 
      values   = vals, 
      title    = title_legend, 
      position = "bottomright", 
      opacity  = 0.7
    ) %>% 
    setView(lng = 2.2137, lat = 46.2276, zoom = 6)
}

#' Trace le graphique des projections climatiques
plot_projection_graph <- function(data_hist, 
                                  data_proj, 
                                  scenario_choisi, 
                                  titre, 
                                  offset_val) {
  
  # Séparation : le scénario choisi vs les autres (pour le fond)
  data_proj_selected <- data_proj %>% filter(Contexte == scenario_choisi)
  data_proj_back     <- data_proj 
  
  ggplot() +
    # 1. Tous les scénarios en arrière-plan (pointillés gris)
    geom_line(data = data_proj_back, 
              aes(x = annee, y = Temperature_moyenne, group = Contexte), 
              color = "grey60", linetype = "dashed", alpha = 0.5) +
    
    # 2. L'historique (Trait plein sombre)
    geom_line(data = data_hist, 
              aes(x = annee, y = Temperature_moyenne, color = "Historique"), 
              linewidth = 1) +
    
    # 3. Le Scénario choisi (Trait coloré épais)
    geom_line(data = data_proj_selected,
              aes(x = annee, y = Temperature_moyenne, color = Contexte),
              linewidth = 1.5) +
    
    # 4. Esthétique et Couleurs
    scale_color_manual(values = c(
      "Historique" = "#2c3e50", 
      "rcp26"      = "#2ecc71", 
      "rcp45"      = "#f39c12", 
      "rcp85"      = "#e74c3c"
    )) +
    
    geom_vline(xintercept = 2025, linetype = "dotted") +
    
    theme_minimal(base_size = 14) +
    
    labs(
      title    = paste("Trajectoire :", titre),
      subtitle = paste("Ajustement (biais) appliqué :", round(offset_val, 1), "°C"),
      y        = "Température (°C)", 
      x        = NULL, 
      color    = "Scénario",
      caption  = "Source: Météo-France & DRIAS"
    ) +
    theme(
      plot.title    = element_text(face = "bold", color = "#2c3e50"),
      plot.subtitle = element_text(size = 10, color = "#7f8c8d"),
      legend.position = "bottom"
    )
}