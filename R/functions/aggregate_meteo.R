#' Agrégation spatiale et temporelle des données météo
#'
#' Agrège les données météorologiques brutes (niveau station/jour) vers un niveau géographique
#' et une granularité temporelle cibles. Gère la moyenne pour les températures et la somme pour les précipitations.
#'
#' @param data Un objet Dataset Arrow ou un dataframe contenant les données brutes (TM, TN, TX, RR).
#' @param granularite_temps Chaîne de caractères. "jour", "mois" ou "annee".
#' @param niveau_geo Chaîne de caractères. "France", "Régionale", "Départementale" ou "Communale".
#' @param choix_geo (Optionnel) Chaîne de caractères pour filtrer une zone précise (ex: "Bretagne").
#'
#' @return Un dataframe agrégé avec les colonnes \code{Temperature_moyenne}, \code{Temperature_min},
#' \code{Temperature_max}, et \code{Precipitation_mm_moy}.
aggregate_meteo <- function(data,
                            granularite_temps = "mois",
                            niveau_geo = "france",
                            choix_geo = NULL) {
  col_geo <- switch(
    niveau_geo,
    "Communale"      = "NOM_USUEL",
    "Départementale" = "NOM_DEPT",
    "Régionale"      = "NOM_REGION",
    "Nationale"      = NA_character_
  )
  
  # 1. Filtrage initial & Conversion
  if (!is.na(col_geo) && !is.null(choix_geo) && choix_geo != "") {
    data <- data %>% filter(.data[[col_geo]] == choix_geo)
  }
  
  data <- data %>%
    mutate(
      TM = as.numeric(TM),
      TN = as.numeric(TN),
      TX = as.numeric(TX),
      RR = as.numeric(RR),
      DATE = as.Date(DATE),
      NOM_REGION = if_else(CODE_DEPT == "20", "Corse", NOM_REGION),
      NOM_DEPT = if_else(CODE_DEPT == "20", "Corse", NOM_DEPT)
    )
  
  # --- ÉTAPE 1 : AGRÉGATION SPATIALE (Moyenne par Jour et par Zone) ---
  # Ici, pour la pluie (RR), on fait la MOYENNE spatiale.
  
  group_vars_spatial <- "DATE"
  if (!is.na(col_geo))
    group_vars_spatial <- c(group_vars_spatial, col_geo)
  
  data_spatial <- data %>%
    group_by(across(all_of(group_vars_spatial))) %>%
    summarise(
      TM = mean(TM, na.rm = TRUE),
      TN = mean(TN, na.rm = TRUE),
      TX = mean(TX, na.rm = TRUE),
      RR = mean(RR, na.rm = TRUE),
      .groups = "drop"
    )
  
  # --- ÉTAPE 2 : AGRÉGATION TEMPORELLE (Sur la période choisie) ---
  # Ici on SOMME.
  if (granularite_temps == "mois") {
    data_spatial <- data_spatial %>% mutate(periode = floor_date(DATE, "month"))
  } else if (granularite_temps == "annee") {
    data_spatial <- data_spatial %>% mutate(periode = floor_date(DATE, "year"))
  } else {
    data_spatial <- data_spatial %>% mutate(periode = DATE)
  }
  
  group_vars_final <- "periode"
  if (!is.na(col_geo))
    group_vars_final <- c(group_vars_final, col_geo)
  
  result <- data_spatial %>%
    group_by(across(all_of(group_vars_final))) %>%
    summarise(
      Temperature_moyenne  = mean(TM, na.rm = TRUE),
      Temperature_min      = mean(TN, na.rm = TRUE),
      Temperature_max      = mean(TX, na.rm = TRUE),
      Precipitation_mm_moy = sum(RR, na.rm = TRUE),
      nb_observations      = n(),
      .groups = "drop"
    ) %>%
    arrange(periode) %>%
    collect()
  
  return(result)
}

#' Ré-agrégation temporelle de données déjà traitées
#'
#' Permet de changer la granularité temporelle (ex: passer de jour à mois/année)
#' sur un dataset déjà filtré ou partiellement agrégé.
#'
#' @param data_jour Dataframe contenant des données journalières.
#' @param tempo Chaîne de caractères. La granularité cible : "jour", "mois" ou "annee".
#'
#' @return Un dataframe ré-agrégé par la nouvelle période temporelle.
reaggregate_tempo <- function(data_jour, tempo) {
  if (tempo == "jour")
    return(data_jour)
  
  # Définition de la période
  if (tempo == "mois") {
    data_jour <- data_jour %>% mutate(periode = floor_date(periode, "month"))
  } else {
    data_jour <- data_jour %>% mutate(periode = floor_date(periode, "year"))
  }
  
  # On groupe par la nouvelle période (et par zone géo si présente)
  cols_geo <- intersect(names(data_jour), c("NOM_DEPT", "NOM_REGION"))
  vars_grp <- c("periode", cols_geo)
  
  data_jour %>%
    group_by(across(all_of(vars_grp))) %>%
    summarise(
      Temperature_moyenne = mean(Temperature_moyenne, na.rm = TRUE),
      Temperature_min     = mean(Temperature_min, na.rm = TRUE),
      Temperature_max     = mean(Temperature_max, na.rm = TRUE),
      Precipitation_mm_moy = sum(Precipitation_mm_moy, na.rm = TRUE),
      .groups = "drop"
    )
}