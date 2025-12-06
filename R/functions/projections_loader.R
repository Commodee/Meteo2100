library(readr)
library(dplyr)
library(stringr)
library(purrr)

load_drias_projections <- function(dossier_data = "../data") {
  
  # 1. On cherche tous les fichiers texte dans le dossier data
  fichiers <- list.files(dossier_data, pattern = "\\.txt$", full.names = TRUE)
  
  if (length(fichiers) == 0) {
    warning("Aucun fichier .txt trouvé dans data/")
    return(NULL)
  }
  
  # 2. Fonction pour lire un seul fichier proprement
  lire_fichier_drias <- function(chemin) {
    
    # --- A. DÉTECTION DU SCÉNARIO VIA LE NOM DU FICHIER ---
    # C'est la nouveauté : on regarde le nom (ex: s2.6.txt) pour savoir ce que c'est
    nom_fichier <- basename(chemin)
    scenario_fixe <- case_when(
      str_detect(nom_fichier, "2.6") ~ "rcp26",
      str_detect(nom_fichier, "4.5") ~ "rcp45",
      str_detect(nom_fichier, "8.5") ~ "rcp85",
      str_detect(nom_fichier, "hist") ~ "historique",
      TRUE ~ "autre"
    )
    
    # --- B. LECTURE DU FICHIER ---
    # On lit le fichier
    df <- read_delim(chemin, delim = ";", comment = "#", show_col_types = FALSE)
    
    # --- C. CORRECTION DES COLONNES ---
    # Si la colonne "Contexte" n'existe pas (ton erreur actuelle), on la crée !
    if (!"Contexte" %in% names(df)) {
      df$Contexte <- scenario_fixe
    }
    
    # On sélectionne et renomme
    # On vérifie aussi si la colonne s'appelle TAV (Temp Moyenne)
    if ("TAV" %in% names(df)) {
      df_clean <- df %>%
        select(Contexte, Annee, TAV) %>%
        rename(
          scenario = Contexte,
          annee = Annee,
          valeur = TAV
        )
    } else {
      # Sécurité si le fichier est vide ou mal formé
      return(NULL)
    }
    
    return(df_clean)
  }
  
  # 3. On charge et combine tous les fichiers
  donnees_brutes <- map_df(fichiers, lire_fichier_drias)
  
  if (is.null(donnees_brutes) || nrow(donnees_brutes) == 0) return(NULL)
  
  # 4. Nettoyage final
  resultat <- donnees_brutes %>%
    # On nettoie les noms de scénario une dernière fois pour être sûr
    mutate(
      scenario = case_when(
        str_detect(scenario, "2.6") ~ "rcp26",
        str_detect(scenario, "4.5") ~ "rcp45",
        str_detect(scenario, "8.5") ~ "rcp85",
        TRUE ~ "historique"
      )
    ) %>%
    group_by(annee, scenario) %>%
    summarise(
      temp_moy = mean(valeur, na.rm = TRUE),
      # Simulation des intervalles (car on a que la moyenne)
      temp_min = temp_moy - 0.5, 
      temp_max = temp_moy + 0.5,
      .groups = "drop"
    )
  
  return(resultat)
}