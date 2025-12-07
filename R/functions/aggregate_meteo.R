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
      DATE = as.Date(DATE)
    )
  
  # --- ÉTAPE 1 : AGRÉGATION SPATIALE (Moyenne par Jour et par Zone) ---
  # On réduit d'abord les stations météo en un seul point par zone géographique et par jour.
  # Ici, pour la pluie (RR), on fait la MOYENNE spatiale (s'il pleut sur la moitié du département, la moyenne reflète ça).
  
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
  # Maintenant qu'on a une valeur par jour, on agrège par Mois ou Année.
  # Ici, pour la pluie, on fait la SOMME temporelle (cumul du mois/année).
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