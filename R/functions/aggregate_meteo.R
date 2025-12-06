aggregate_meteo <- function(data,
                            granularite_temps = "mois",
                            niveau_geo = "france",
                            choix_geo = NULL) {
  # Mapping simple
  col_geo <- switch(
    niveau_geo,
    "Communale"      = "NOM_USUEL",
    "Départementale" = "NOM_DEPT",
    "Régionale"      = "NOM_REGION",
    "Nationale"      = NA_character_
  )
  
  # 1. Filtrage (Avant le collect)
  if (!is.na(col_geo) && !is.null(choix_geo) && choix_geo != "") {
    data <- data %>% filter(.data[[col_geo]] == choix_geo)
  }
  
  # 2. Calcul Période (Logique R standard, Arrow gère bien floor_date maintenant)
  data <- data %>%
    mutate(
      TM = as.numeric(TM),
      TN = as.numeric(TN),
      TX = as.numeric(TX),
      RR = as.numeric(RR)
    )
  
  # arrow ne supporte pas le case when
  if (granularite_temps == "semaine") {
    data <- data %>% mutate(periode = floor_date(DATE, "week"))
  } else if (granularite_temps == "mois") {
    data <- data %>% mutate(periode = floor_date(DATE, "month"))
  } else if (granularite_temps == "annee") {
    data <- data %>% mutate(periode = floor_date(DATE, "year"))
  } else {
    data <- data %>% mutate(periode = DATE)
  }
  
  # 3. Group by & Summarise
  group_vars <- "periode"
  if (!is.na(col_geo))
    group_vars <- c(group_vars, col_geo)
  
  result <- data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      Temperature_moyenne = mean(TM, na.rm = TRUE),
      Temperature_min     = mean(TN, na.rm = TRUE),
      Temperature_max     = mean(TX, na.rm = TRUE),
      Precipitation_mm_moy = mean(RR, na.rm = TRUE),
      nb_observations      = n(),
      .groups = "drop"
    ) %>%
    arrange(periode) %>%
    collect()
  
  return(result)
}