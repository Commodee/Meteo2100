#' Génère une carte HTML stylisée pour afficher un indicateur clé (Max, Min, Moyenne)
#' avec un code couleur adapté au type de variable (Température/Précipitation) et à la valeur.
#'
#' @param data Dataframe contenant les données à analyser.
#' @param col_var Chaîne de caractères. Nom de la colonne contenant la variable d'intérêt.
#' @param type Chaîne de caractères. Type de statistique à calculer : "max", "min" ou "mean".
#' @param var_type Chaîne de caractères. Type de variable météo : "Temperature" ou "Precipitation".
#' @param temporal_scale (Optionnel) Chaîne de caractères. Échelle temporelle ("jour", "mois", "annee") pour le formatage de la date.
#' @param key_col (Optionnel) Chaîne de caractères. Nom de la colonne identifiant l'entité géographique (ex: "NOM_DEPT") pour l'affichage.
#'
#' @return Un objet HTML (tagList/div) représentant la carte d'information.
#' @export
create_info_card <- function(data, col_var, type, var_type, temporal_scale = NULL, key_col = NULL) {
  # type: "max", "min", "mean"
  
  is_temp <- var_type == "Temperature"
  unit <- if (is_temp) "°C" else "mm"
  
  if (type == "mean") {
    val <- if (is_temp) mean(data[[col_var]], na.rm = TRUE) else sum(data[[col_var]], na.rm = TRUE)
    
    if (!is.null(key_col)) {
      title <- "Moyenne Nationale"
      desc <- "France Métropolitaine"
    } else {
      title <- if (is_temp) "Moyenne Période" else "Cumul Total"
      desc <- "Sur la période sélectionnée"
    }
    
    bg_color <- "linear-gradient(135deg, #2ecc71 0%, #1abc9c 100%)" # Vert
    
  } else {
    # Min or Max
    if (type == "max") {
      row <- data %>% filter(!!sym(col_var) == max(!!sym(col_var), na.rm = TRUE)) %>% slice(1)
      
      if (!is.null(key_col)) {
        title <- if (is_temp) "Le plus chaud" else "Le plus pluvieux"
      } else {
        title <- if (is_temp) "Record de Chaleur" else "Record de Pluie"
      }
      
      bg_color <- if (is_temp) {
        "linear-gradient(135deg, #e74c3c 0%, #c0392b 100%)" # Rouge
      } else {
        "linear-gradient(135deg, #3498db 0%, #2980b9 100%)" # Bleu
      }
      
    } else {
      # Min
      row <- data %>% filter(!!sym(col_var) == min(!!sym(col_var), na.rm = TRUE)) %>% slice(1)
      
      if (!is.null(key_col)) {
        title <- if (is_temp) "Le plus froid" else "Le plus sec"
      } else {
        title <- if (is_temp) "Record de Froid" else "Record de Sécheresse"
      }
      
      bg_color <- if (is_temp) {
        "linear-gradient(135deg, #3498db 0%, #2980b9 100%)" # Bleu
      } else {
        "linear-gradient(135deg, #f39c12 0%, #d35400 100%)" # Orange/Sec
      }
    }
    
    val <- row[[col_var]]
    
    if (!is.null(key_col)) {
      desc <- row[[key_col]]
    } else {
      date_val <- row$periode
      desc <- switch(temporal_scale,
        "jour" = format(date_val, "%d/%m/%Y"),
        "mois" = format(date_val, "%m/%Y"),
        "annee" = format(date_val, "%Y")
      )
    }
  }
  
  div(
    class = "info-card",
    style = paste0("background: ", bg_color, ";"),
    div(class = "info-card-title", title),
    div(class = "info-card-value", paste0(round(val, 1), " ", unit)),
    div(class = "info-card-desc", desc)
  )
}
