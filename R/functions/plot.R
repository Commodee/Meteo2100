plotplot <- function(data, niveau_geo, input_geo, granularite_temps){
  
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
  
  # 4. Création du graphique Design
  ggplot(data_mieux, aes(x = periode)) +
    
    # A. Zone d'amplitude (Min - Max) en arrière-plan
    # Cela donne un contexte visuel immédiat sur les écarts de température
    #geom_ribbon(aes(ymin = Temperature_min, ymax = Temperature_max), 
    #            fill = "#3498db", alpha = 0.2) +
    
    # B. Ligne de Moyenne (Plus épaisse et jolie couleur)
    geom_line(aes(y = Temperature_moyenne), 
              color = "#2980b9", linewidth = 1.2) +
    
    # C. Ligne de référence 0°C (discrète)
    #geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", size = 0.5) +
    
    # D. Thème et Esthétique
    theme_minimal(base_size = 14) + # Police un peu plus grande
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
      panel.grid.minor = element_blank(), # On retire les petites lignes de grille pour alléger
      panel.grid.major.x = element_line(linetype = "dotted", color = "gray80")
    )
  
  
}

plot_spiral <- function(data, niveau_geo, input_geo) {
  
  require(ggplot2)
  require(dplyr)
  require(lubridate)
  
  # 1. On force l'agrégation mensuelle (c'est le seul mode qui a du sens pour une spirale)
  data_agg <- aggregate_meteo(data, granularite_temps = "mois", niveau_geo, input_geo)
  
  # Validation
  shiny::validate(shiny::need(nrow(data_agg) > 0, "Pas de données."))
  
  # 2. Préparation des données pour la spirale
  data_spiral <- data_agg %>%
    mutate(
      mois_num = month(periode),
      mois_label = month(periode, label = TRUE, abbr = TRUE), # Jan, Fév...
      annee = year(periode),
      # Pour que la spirale soit continue, on a besoin d'un groupe ou d'un path
      # Ici on va utiliser l'année pour la couleur
    ) %>% 
    filter(annee < year(Sys.Date())) # On retire l'année incomplète (2025)
  
  titre_lieu <- if (is.null(input_geo) || is.na(input_geo)) "France entière" else input_geo
  
  # 3. Construction du graphique
  ggplot(data_spiral, aes(x = mois_num, y = Temperature_moyenne, group = annee, color = annee)) +
    
    # Le tracé
    geom_line(size = 0.5, alpha = 0.7) +
    
    # L'échelle de couleur (Du bleu pour le passé au rouge pour le présent)
    scale_color_gradientn(colors = c("#3498db", "#f1c40f", "#e74c3c"), 
                          name = "Année") +
    
    # La magie : Coordonnées Polaires
    coord_polar() +
    
    # Gestion des axes (Mois autour du cercle)
    scale_x_continuous(breaks = 1:12, labels = c("Jan", "Fév", "Mar", "Avr", "Mai", "Juin", 
                                                 "Juil", "Août", "Sep", "Oct", "Nov", "Déc")) +
    
    # Thème sombre ou épuré (souvent plus joli pour les spirales)
    theme_minimal() +
    labs(
      title = paste("🌀 Spirale Climatique :", titre_lieu),
      subtitle = "Chaque ligne est une année. Plus on s'éloigne du centre, plus il fait chaud.",
      y = NULL,
      x = NULL
    ) +
    theme(
      axis.text.y = element_blank(), # On cache les températures sur les cercles concentriques pour la clarté
      panel.grid.major.y = element_line(color = "gray90", linetype = "dotted"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray50")
    )
}