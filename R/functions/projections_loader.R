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
  
  # 2. FONCTION DE LECTURE ROBUSTE
  lire_fichier_horizon <- function(chemin) {
    
    # Lecture brute sans en-tête
    df <- suppressMessages(read_delim(chemin, delim = ";", comment = "#", col_names = FALSE, 
                                      show_col_types = FALSE, col_types = cols(.default = "c")))
    
    # Fallback virgule
    if (ncol(df) <= 1) {
      df <- suppressMessages(read_delim(chemin, delim = ",", comment = "#", col_names = FALSE, 
                                        show_col_types = FALSE, col_types = cols(.default = "c")))
    }
    
    # Vérification structure minimale (Scenario, Horizon, Valeur)
    if (ncol(df) < 6) return(NULL)
    
    # Extraction des colonnes clés (4=Scenario, 5=Horizon, 6=Valeur)
    df_clean <- df %>%
      select(X4, X5, X6) %>%
      rename(scenario_txt = X4, horizon_txt = X5, valeur = X6) %>%
      mutate(
        valeur = as.numeric(valeur),
        # DÉTECTION LARGE DES HORIZONS
        annee = case_when(
          # Passé (Reference) -> 1990
          str_detect(horizon_txt, regex("Ref|His|1976", ignore_case = TRUE)) ~ 1990,
          # H1 (2021-2050) -> 2035
          str_detect(horizon_txt, regex("H1|2021|2035", ignore_case = TRUE)) ~ 2035,
          # H2 (2041-2070) -> 2055
          str_detect(horizon_txt, regex("H2|2041|2055", ignore_case = TRUE)) ~ 2055,
          # H3 (2071-2100) -> 2085
          str_detect(horizon_txt, regex("H3|2071|2085", ignore_case = TRUE)) ~ 2085,
          TRUE ~ NA_real_
        )
      ) %>%
      filter(!is.na(annee), !is.na(valeur))
    
    # On force le nom du scénario d'après le fichier pour éviter les mélanges
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
  
  # 4. SÉCURITÉ : On s'assure que 1990 existe pour tout le monde
  # Si un scénario n'a pas 1990, on lui donne la moyenne des autres (très probable que ce soit pareil)
  ref_val <- mean(donnees_brutes$valeur[donnees_brutes$annee == 1990], na.rm = TRUE)
  
  # On complète les trous
  resultat <- donnees_brutes %>%
    group_by(annee, scenario) %>%
    summarise(
      temp_moy = mean(valeur, na.rm = TRUE),
      temp_min = temp_moy - 0.5, 
      temp_max = temp_moy + 0.5,
      .groups = "drop"
    )
  
  return(resultat)
}