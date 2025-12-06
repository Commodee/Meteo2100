library(readr)
library(dplyr)
library(stringr)
library(purrr)

load_drias_projections <- function(dossier_data = NULL) {
  
  # 1. DÉTECTION DU DOSSIER
  chemins_possibles <- c("../data", "data", "Meteo2100/data")
  dossier_valide <- NULL
  for (ch in chemins_possibles) {
    if (dir.exists(ch) && length(list.files(ch, pattern = "\\.txt$")) > 0) {
      dossier_valide <- ch
      break
    }
  }
  
  if (is.null(dossier_valide)) {
    warning("⚠️ ERREUR : Aucun fichier .txt trouvé.")
    return(NULL)
  }
  
  fichiers <- list.files(dossier_valide, pattern = "\\.txt$", full.names = TRUE)
  
  # 2. LECTURE SPÉCIALE "HORIZONS"
  lire_fichier_horizon <- function(chemin) {
    
    # On lit sans en-tête (col_names = FALSE) pour éviter le problème des #
    df <- suppressMessages(read_delim(chemin, delim = ";", comment = "#", col_names = FALSE, 
                                      show_col_types = FALSE, col_types = cols(.default = "c")))
    
    if (ncol(df) <= 1) {
      df <- suppressMessages(read_delim(chemin, delim = ",", comment = "#", col_names = FALSE, 
                                        show_col_types = FALSE, col_types = cols(.default = "c")))
    }
    
    # D'après ton erreur précédente, la structure est :
    # ... | Col 4 (Scénario) | Col 5 (Horizon: H1, H2...) | Col 6 (Valeur) ...
    if (ncol(df) < 6) return(NULL)
    
    df_clean <- df %>%
      select(X4, X5, X6) %>%
      rename(scenario_raw = X4, horizon = X5, valeur = X6) %>%
      mutate(
        valeur = as.numeric(valeur),
        # --- C'EST ICI QU'ON CONVERTIT LES HORIZONS EN ANNÉES ---
        annee = case_when(
          str_detect(horizon, "Ref") ~ 1990,       # Passé (Reference)
          str_detect(horizon, "H1") ~ 2035,        # Futur proche
          str_detect(horizon, "H2") ~ 2055,        # Futur moyen
          str_detect(horizon, "H3") ~ 2085,        # Fin de siècle
          TRUE ~ NA_real_
        )
      ) %>%
      filter(!is.na(annee), !is.na(valeur))
    
    # Correction du nom du scénario via le nom du fichier (plus sûr)
    nom_fichier <- basename(chemin)
    scen_fixe <- case_when(
      str_detect(nom_fichier, "2.6") ~ "rcp26",
      str_detect(nom_fichier, "4.5") ~ "rcp45",
      str_detect(nom_fichier, "8.5") ~ "rcp85",
      TRUE ~ "historique"
    )
    df_clean$scenario <- scen_fixe
    
    return(df_clean)
  }
  
  # 3. COMBINAISON
  donnees_brutes <- map_df(fichiers, lire_fichier_horizon)
  
  if (is.null(donnees_brutes) || nrow(donnees_brutes) == 0) return(NULL)
  
  # 4. AGREGATION
  resultat <- donnees_brutes %>%
    group_by(annee, scenario) %>%
    summarise(
      temp_moy = mean(valeur, na.rm = TRUE),
      # On simule un intervalle car les horizons donnent souvent juste la moyenne
      temp_min = temp_moy - 0.5, 
      temp_max = temp_moy + 0.5,
      .groups = "drop"
    )
  
  return(resultat)
}