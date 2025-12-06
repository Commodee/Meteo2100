library(readr)
library(dplyr)
library(stringr)
library(purrr)

load_drias_projections <- function(dossier_data = "../data") {
  
  # 1. On cherche tous les fichiers texte dans le dossier data
  # (Il va trouver s2.6.txt, s4.5.txt et s8.5.txt)
  fichiers <- list.files(dossier_data, pattern = "\\.txt$", full.names = TRUE)
  
  if (length(fichiers) == 0) {
    warning("Aucun fichier .txt trouvé dans data/")
    return(NULL)
  }
  
  # 2. Fonction pour lire un seul fichier proprement
  lire_fichier_drias <- function(chemin) {
    # On lit en ignorant les lignes qui commencent par # (commentaires)
    # On spécifie le délimiteur ";"
    df <- read_delim(chemin, delim = ";", comment = "#", show_col_types = FALSE)
    
    # On garde juste ce qui nous intéresse
    # TAV = Température Moyenne
    df_clean <- df %>%
      select(any_of(c("Contexte", "Annee", "TAV"))) %>%
      rename(
        scenario = Contexte,
        annee = Annee,
        valeur = TAV
      )
    return(df_clean)
  }
  
  # 3. On charge et combine tous les fichiers trouvés
  donnees_brutes <- map_df(fichiers, lire_fichier_drias)
  
  # 4. Nettoyage final et calcul des stats
  resultat <- donnees_brutes %>%
    # Harmonisation des noms de scénarios pour qu'ils correspondent à ton UI
    mutate(
      scenario_clean = case_when(
        str_detect(scenario, "RCP2.6") ~ "rcp26",
        str_detect(scenario, "RCP4.5") ~ "rcp45",
        str_detect(scenario, "RCP8.5") ~ "rcp85",
        str_detect(scenario, "Historique") ~ "historique",
        TRUE ~ "autre"
      )
    ) %>%
    # On fait une moyenne globale par année et scénario
    group_by(annee, scenario_clean) %>%
    summarise(
      temp_moy = mean(valeur, na.rm = TRUE),
      # Création d'un intervalle de confiance simulé (car tes fichiers n'ont que la médiane)
      temp_min = temp_moy - 0.5, 
      temp_max = temp_moy + 0.5,
      .groups = "drop"
    ) %>%
    rename(scenario = scenario_clean)
  
  return(resultat)
}