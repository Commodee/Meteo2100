# data_loader.R
# Script pour charger toutes les données nécessaires à l'application Shiny

#' Charger toutes les données brutes nécessaires à l'application
#'
#' Cette fonction charge :
#' - Les données météo quotidiennes (format Arrow/Parquet)
#' - Les agrégations pré-calculées (nationale, régionale, départementale)
#' - Les fonds de carte (départements, régions)
#' - Les projections climatiques DRIAS
#'
#' @param dossier_parquet Chemin vers le dossier contenant les fichiers parquet météo
#' @param dossier_geo Chemin vers le dossier contenant les shapefiles
#' @param dossier_drias Chemin vers le dossier contenant les données DRIAS
#' @param verbose Afficher les messages de progression
#'
#' @return Une liste nommée contenant tous les objets de données
#' @export
load_raw_data <- function(dossier_parquet = "../data/meteo_parquet",
                          dossier_geo = "../data/geo",
                          dossier_drias = "../data/drias",
                          verbose = TRUE) {
  
  if (verbose) {
    cat("═══════════════════════════════════════════════════════════════\n")
    cat("  CHARGEMENT DES DONNÉES\n")
    cat("═══════════════════════════════════════════════════════════════\n\n")
  }
  
  # Liste pour stocker tous les objets
  data_list <- list()
  
  # 1. DONNÉES MÉTÉO BRUTES (Arrow Dataset) --------------------------------
  if (verbose) cat("→ Chargement des données météo quotidiennes...\n")
  
  if (!dir.exists(dossier_parquet)) {
    stop(paste("Le dossier", dossier_parquet, "n'existe pas. Veuillez d'abord télécharger les données."))
  }
  
  data_list$meteo <- arrow::open_dataset(dossier_parquet)
  
  # Vérification
  n_rows <- data_list$meteo %>% 
    dplyr::count() %>% 
    dplyr::collect() %>% 
    dplyr::pull(n)
  
  if (verbose) {
    cat(sprintf("  ✓ %s observations chargées\n", format(n_rows, big.mark = " ")))
  }
  
  
  # 2. AGRÉGATIONS PRÉ-CALCULÉES -------------------------------------------
  if (verbose) cat("\n→ Calcul des agrégations temporelles...\n")
  
  # 2.1 Agrégation NATIONALE (Jour, Mois, Année)
  if (verbose) cat("  • Agrégation nationale...\n")
  
  data_list$meteo_nationale <- data_list$meteo %>%
    dplyr::group_by(DATE) %>%
    dplyr::summarise(
      Temperature_moyenne = mean(TM, na.rm = TRUE),
      Temperature_min = mean(TN, na.rm = TRUE),
      Temperature_max = mean(TX, na.rm = TRUE),
      Precipitation_mm_moy = mean(RR, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::collect() %>%
    dplyr::rename(periode = DATE) %>%
    dplyr::arrange(periode)
  
  if (verbose) cat(sprintf("    ✓ %s jours (national)\n", nrow(data_list$meteo_nationale)))
  
  # 2.2 Agrégation RÉGIONALE
  if (verbose) cat("  • Agrégation régionale...\n")
  
  data_list$meteo_regionale <- data_list$meteo %>%
    dplyr::group_by(NOM_REGION, DATE) %>%
    dplyr::summarise(
      Temperature_moyenne = mean(TM, na.rm = TRUE),
      Temperature_min = mean(TN, na.rm = TRUE),
      Temperature_max = mean(TX, na.rm = TRUE),
      Precipitation_mm_moy = mean(RR, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::collect() %>%
    dplyr::rename(periode = DATE) %>%
    dplyr::arrange(NOM_REGION, periode)
  
  if (verbose) {
    n_regions <- data_list$meteo_regionale %>% 
      dplyr::distinct(NOM_REGION) %>% 
      nrow()
    cat(sprintf("    ✓ %s régions × jours\n", n_regions))
  }
  
  # 2.3 Agrégation DÉPARTEMENTALE
  if (verbose) cat("  • Agrégation départementale...\n")
  
  data_list$meteo_departementale <- data_list$meteo %>%
    dplyr::group_by(NOM_DEPT, CODE_DEPT, NOM_REGION, CODE_REGION, DATE) %>%
    dplyr::summarise(
      Temperature_moyenne = mean(TM, na.rm = TRUE),
      Temperature_min = mean(TN, na.rm = TRUE),
      Temperature_max = mean(TX, na.rm = TRUE),
      Precipitation_mm_moy = mean(RR, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::collect() %>%
    dplyr::rename(periode = DATE) %>%
    dplyr::arrange(NOM_DEPT, periode)
  
  if (verbose) {
    n_depts <- data_list$meteo_departementale %>% 
      dplyr::distinct(CODE_DEPT) %>% 
      nrow()
    cat(sprintf("    ✓ %s départements × jours\n", n_depts))
  }
  
  
  # 3. FONDS DE CARTE (GÉOMÉTRIES) -----------------------------------------
  if (verbose) cat("\n→ Chargement des fonds de carte...\n")
  
  # 3.1 Départements
  chemin_dept <- file.path(dossier_geo, "departements.shp")
  if (file.exists(chemin_dept)) {
    data_list$departements <- sf::st_read(chemin_dept, quiet = !verbose)
    if (verbose) cat(sprintf("  ✓ %s départements\n", nrow(data_list$departements)))
  } else {
    warning(paste("Fichier", chemin_dept, "introuvable. Carte départementale non disponible."))
    data_list$departements <- NULL
  }
  
  # 3.2 Régions
  chemin_reg <- file.path(dossier_geo, "regions.shp")
  if (file.exists(chemin_reg)) {
    data_list$regions <- sf::st_read(chemin_reg, quiet = !verbose)
    if (verbose) cat(sprintf("  ✓ %s régions\n", nrow(data_list$regions)))
  } else {
    warning(paste("Fichier", chemin_reg, "introuvable. Carte régionale non disponible."))
    data_list$regions <- NULL
  }
  
  
  # 4. PROJECTIONS CLIMATIQUES DRIAS ---------------------------------------
  if (verbose) cat("\n→ Chargement des projections DRIAS...\n")
  
  chemin_drias <- file.path(dossier_drias, "drias_projections.csv")
  if (file.exists(chemin_drias)) {
    data_list$drias <- readr::read_csv(chemin_drias, show_col_types = FALSE)
    if (verbose) {
      cat(sprintf("  ✓ %s lignes de projections\n", nrow(data_list$drias)))
      scenarios <- unique(data_list$drias$scenario)
      cat(sprintf("  ✓ Scénarios : %s\n", paste(scenarios, collapse = ", ")))
    }
  } else {
    warning(paste("Fichier", chemin_drias, "introuvable. Onglet 'Et demain ?' non disponible."))
    # Créer un tibble vide pour éviter les erreurs
    data_list$drias <- tibble::tibble(
      annee = integer(),
      scenario = character(),
      Temp_moy = numeric(),
      Temp_min = numeric(),
      Temp_max = numeric()
    )
  }
  
  
  # 5. RÉSUMÉ FINAL --------------------------------------------------------
  if (verbose) {
    cat("\n═══════════════════════════════════════════════════════════════\n")
    cat("✓ Toutes les données ont été chargées avec succès\n")
    cat("═══════════════════════════════════════════════════════════════\n")
    cat("\nContenu de global_data :\n")
    cat(sprintf("  • meteo                 : Arrow Dataset (%s obs)\n", format(n_rows, big.mark = " ")))
    cat(sprintf("  • meteo_nationale       : %s lignes\n", nrow(data_list$meteo_nationale)))
    cat(sprintf("  • meteo_regionale       : %s lignes\n", nrow(data_list$meteo_regionale)))
    cat(sprintf("  • meteo_departementale  : %s lignes\n", nrow(data_list$meteo_departementale)))
    cat(sprintf("  • departements          : %s géométries\n", ifelse(is.null(data_list$departements), "Non chargé", nrow(data_list$departements))))
    cat(sprintf("  • regions               : %s géométries\n", ifelse(is.null(data_list$regions), "Non chargé", nrow(data_list$regions))))
    cat(sprintf("  • drias                 : %s lignes\n", nrow(data_list$drias)))
    cat("\n")
  }
  
  return(data_list)
}


#' Recharger uniquement les agrégations (sans re-télécharger les données)
#'
#' Utile si vous avez modifié les calculs d'agrégation
#'
#' @param global_data Liste retournée par load_raw_data()
#' @param verbose Afficher les messages
#'
#' @return Liste mise à jour
#' @export
reload_aggregations <- function(global_data, verbose = TRUE) {
  
  if (verbose) cat("→ Recalcul des agrégations...\n")
  
  # Nationale
  global_data$meteo_nationale <- global_data$meteo %>%
    dplyr::group_by(DATE) %>%
    dplyr::summarise(
      Temperature_moyenne = mean(TM, na.rm = TRUE),
      Temperature_min = mean(TN, na.rm = TRUE),
      Temperature_max = mean(TX, na.rm = TRUE),
      Precipitation_mm_moy = mean(RR, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::collect() %>%
    dplyr::rename(periode = DATE)
  
  # Régionale
  global_data$meteo_regionale <- global_data$meteo %>%
    dplyr::group_by(NOM_REGION, DATE) %>%
    dplyr::summarise(
      Temperature_moyenne = mean(TM, na.rm = TRUE),
      Temperature_min = mean(TN, na.rm = TRUE),
      Temperature_max = mean(TX, na.rm = TRUE),
      Precipitation_mm_moy = mean(RR, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::collect() %>%
    dplyr::rename(periode = DATE)
  
  # Départementale
  global_data$meteo_departementale <- global_data$meteo %>%
    dplyr::group_by(NOM_DEPT, CODE_DEPT, NOM_REGION, CODE_REGION, DATE) %>%
    dplyr::summarise(
      Temperature_moyenne = mean(TM, na.rm = TRUE),
      Temperature_min = mean(TN, na.rm = TRUE),
      Temperature_max = mean(TX, na.rm = TRUE),
      Precipitation_mm_moy = mean(RR, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::collect() %>%
    dplyr::rename(periode = DATE)
  
  if (verbose) cat("✓ Agrégations mises à jour\n")
  
  return(global_data)
}