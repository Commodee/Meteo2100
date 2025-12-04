log_msg <- function(...,verbose=FALSE){
  if (verbose){
    cat(...)
  }
}

#' Fonction Principale Optimisée RAM avec Mode Light
download_meteo_multi_parquet <- function(departements,
                                         mode = "full",
                                         annee = NULL,
                                         output_dir = "../data/meteo_parquet",
                                         verbose = TRUE) {
  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE)
  log_msg(paste0("=== Sync Météo (Mode : ", mode, ") ==="), verbose = verbose)
  
  ref_geo <- get_referentiel_geo(verbose = FALSE)
  dataset_id <- "donnees-climatologiques-de-base-quotidiennes"
  
  all_resources <- list_dataset_resources(dataset_id, verbose = verbose)
  if (is.null(all_resources))
    return(NULL)
  
  for (dept in departements) {
    log_msg(paste0("\nTraite Dept: ", dept), verbose = verbose)
    
    # 1. Filtrage basique
    pattern_dept <- paste0("departement_", dept, "_")
    dept_res <- all_resources %>%
      filter(str_detect(titre, fixed(pattern_dept, ignore_case = TRUE)), str_detect(titre, "RR-T-Vent"))
    
    if (nrow(dept_res) == 0) {
      log_msg("   Aucune donnée trouvée.", verbose = verbose)
      next
    }
    
    # 2. Analyse des périodes
    dept_res <- dept_res %>%
      mutate(start_year = as.numeric(str_extract(periode_str, "\\d{4}")),
             end_year   = as.numeric(str_extract(periode_str, "(?<=-\\d{0,3})\\d{4}"))) %>%
      filter(end_year >= 1950) # On garde toujours post-1950
    
    # --- LOGIQUE MODE LIGHT ---
    if (mode == "light") {
      # On ne garde que ce qui commence après 2023 (donc le fichier 2024-2025)
      dept_res <- dept_res %>% filter(start_year >= 2024)
      
      if (nrow(dept_res) == 0) {
        log_msg("   [Info] Mode Light : Aucun fichier récent (2024+) pour ce département.",
                verbose = verbose)
        next
      }
    }
    # ---------------------------
    
    # 3. Traitement
    for (i in seq_len(nrow(dept_res))) {
      res <- dept_res[i, ]
      
      # Filtre année spécifique (si précisée)
      if (!is.null(annee)) {
        if (annee < res$start_year || annee > res$end_year)
          next
      }
      
      process_one_resource(res, dept, output_dir, ref_geo, verbose = verbose)
      gc(verbose = FALSE)
    }
  }
  log_msg("\nTerminé !", verbose = verbose)
}


#' Récupérer le référentiel Géographique (Départements et Régions)
#' Source: API Géo (geo.api.gouv.fr)
#' @return Un dataframe de référence
get_referentiel_geo <- function() {
  cat("Récupération du référentiel géographique (API Géo)...\n")
  url <- "https://geo.api.gouv.fr/departements?fields=nom,code,region"
  
  
  tryCatch({
    data_geo <- jsonlite::fromJSON(url)
    referentiel <- data.frame(
      CODE_DEPT = data_geo$code,
      NOM_DEPT = data_geo$nom,
      CODE_REGION = data_geo$region$code,
      NOM_REGION = data_geo$region$nom,
      stringsAsFactors = FALSE
    )
    
    cat("Référentiel chargé :",
        nrow(referentiel),
        "départements trouvés.\n")
    return(referentiel)
  }, error = function(e) {
    cat("Erreur lors de la récupération du référentiel géo :",
        conditionMessage(e),
        "\n")
    return(NULL)
  })
}



#' Ajouter les noms de départements et régions aux données météo
#' @param data_meteo Le dataframe issu de download_meteo_multi (doit avoir CODE_DEPT)
#' @return Le dataframe enrichi

enrichir_donnees_geo <- function(data_meteo) {
  if (is.null(data_meteo))
    return(NULL)
  
  # 1. On récupère le référentiel
  ref_geo <- get_referentiel_geo()
  
  if (is.null(ref_geo)) {
    warning("Impossible d'enrichir les données (référentiel manquant).")
    return(data_meteo)
  }
  
  cat("Enrichissement des données avec les noms et régions...\n")
  
  data_enrichie <- data_meteo %>%
    left_join(ref_geo, by = "CODE_DEPT")
  cat("Données enrichies !\n\n")
  
  return(data_enrichie)
}