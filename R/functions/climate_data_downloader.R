#' Petit helper pour les messages de log
log_msg <- function(..., verbose = TRUE) {
  if (verbose) cat(..., "\n")
}

#' Lister les ressources disponibles sur data.gouv
list_dataset_resources <- function(dataset_id, verbose = TRUE) {
  url <- paste0("https://www.data.gouv.fr/api/1/datasets/", dataset_id, "/")
  
  tryCatch({
    response <- GET(url)
    if (status_code(response) != 200) return(NULL)
    
    content <- content(response, "parsed", encoding = "UTF-8")
    
    map_df(content$resources, function(r) {
      tibble(
        titre = r$title,
        id = r$id,
        url = r$url,
        # Extraction de la période pour filtrage futur
        periode_str = str_extract(r$title, "periode_(\\d{4})-(\\d{4})")
      )
    })
  }, error = function(e) {
    warning("Erreur API data.gouv: ", conditionMessage(e))
    return(NULL)
  })
}

#' Récupérer le référentiel Géographique (Départements et Régions)
get_referentiel_geo <- function(verbose = TRUE) {
  log_msg("Récupération du référentiel géographique (API Géo)...", verbose = verbose)
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
    log_msg(paste("Référentiel chargé :", nrow(referentiel), "départements."), verbose = verbose)
    return(referentiel)
  }, error = function(e) {
    warning("Erreur API Géo : ", conditionMessage(e))
    return(NULL)
  })
}

#' Traite UNE SEULE ressource : Télécharge -> Nettoie -> Parquet -> Supprime CSV
process_one_resource <- function(resource, dept_code, output_dir, ref_geo, verbose = TRUE) {
  
  suffixe <- if(is.na(resource$periode_str)) "autres" else gsub("periode_", "", resource$periode_str)
  output_file <- file.path(output_dir, paste0("meteo_", dept_code, "_", suffixe, ".parquet"))
  
  if (file.exists(output_file)) {
    log_msg(paste("   [OK] Déjà présent :", basename(output_file)), verbose = verbose)
    return(TRUE)
  }
  
  log_msg(paste("   -> Téléchargement :", resource$titre), verbose = verbose)
  
  tmp_csv <- tempfile(fileext = ".csv.gz")
  
  tryCatch({
    response <- GET(resource$url, write_disk(tmp_csv, overwrite = TRUE), timeout(600))
    
    if (status_code(response) == 200) {
      df <- read_csv2(
        tmp_csv, 
        locale = locale(encoding = "UTF-8"),
        col_types = cols(.default = col_character()), 
        show_col_types = FALSE,
        progress = FALSE
      )
      
      col_date <- intersect(names(df), c("AAAAMMJJ", "DATE"))[1]
      
      if (!is.na(col_date)) {
        df_clean <- df %>%
          rename(DATE = all_of(col_date)) %>%
          mutate(
            CODE_DEPT = dept_code,
            DATE = ymd(DATE),
            TM = as.numeric(TM),
            TN = as.numeric(TN),
            TX = as.numeric(TX),
            RR = as.numeric(RR)
          ) %>%
          mutate(
            ANNEE = year(DATE),
            MOIS = month(DATE)
          )
        
        if (!is.null(ref_geo) & dept_code!=20) {
          df_clean <- left_join(df_clean, ref_geo, by = "CODE_DEPT")
        } else { # Probleme avec la corse, on tekecharge avec 20, mais ref geo attend 2a et 2b
          ref_geo_corse <- data.frame(
            CODE_DEPT = "20",
            NOM_DEPT = "Corse",
            CODE_REGION = "94",
            NOM_REGION = "Corse",
            stringsAsFactors = FALSE
          )
          df_clean <- left_join(df_clean, ref_geo_corse, by = "CODE_DEPT")
        }
        
        write_parquet(df_clean, output_file)
        log_msg(paste("      Sauvegardé :", basename(output_file)), verbose = verbose)
        
        rm(df, df_clean)
      }
    }
  }, error = function(e) {
    warning("Echec sur ", resource$titre, ": ", conditionMessage(e))
  }, finally = {
    if (file.exists(tmp_csv)) unlink(tmp_csv)
  })
  
  return(TRUE)
}

#' Fonction Principale : Orchestre le téléchargement (Mode Full ou Light)
download_meteo_multi_parquet <- function(departements, 
                                         mode = "full", 
                                         annee = NULL, 
                                         output_dir = "../data/meteo_parquet",
                                         verbose = TRUE) {
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  log_msg(paste0("=== Sync Météo (Mode : ", mode, ") ==="), verbose = verbose)
  
  # Chargement unique du référentiel
  ref_geo <- get_referentiel_geo(verbose = FALSE)
  dataset_id <- "donnees-climatologiques-de-base-quotidiennes"
  
  all_resources <- list_dataset_resources(dataset_id, verbose = verbose)
  if (is.null(all_resources)) return(NULL)
  
  for (dept in departements) {
    log_msg(paste0("\nTraite Dept: ", dept), verbose = verbose)
    
    # 1. Identifier les ressources du département
    pattern_dept <- paste0("departement_", dept, "_")
    dept_res <- all_resources %>%
      filter(str_detect(titre, fixed(pattern_dept, ignore_case = TRUE)), 
             str_detect(titre, "RR-T-Vent"))
    
    if (nrow(dept_res) == 0) {
      log_msg("   Aucune donnée trouvée.", verbose = verbose)
      next
    }
    
    # 2. Analyser les années
    dept_res <- dept_res %>%
      mutate(
        start_year = as.numeric(str_extract(periode_str, "\\d{4}")),
        end_year   = as.numeric(str_extract(periode_str, "(?<=-\\d{0,3})\\d{4}"))
      ) %>%
      filter(end_year >= 1950) # On ignore l'avant-1950
    
    # 3. Filtrage "Mode Light" (Uniquement fichiers récents > 2024)
    if (mode == "light") {
      dept_res <- dept_res %>% filter(start_year >= 2024)
      if (nrow(dept_res) == 0) {
        log_msg("   [Info] Mode Light : Aucun fichier récent (2024+) trouvé.", verbose = verbose)
        next
      }
    }
    
    # 4. Lancer le traitement fichier par fichier
    for (i in seq_len(nrow(dept_res))) {
      res <- dept_res[i, ]
      
      # Filtre année spécifique optionnel
      if (!is.null(annee)) {
        if (annee < res$start_year || annee > res$end_year) next
      }
      
      process_one_resource(res, dept, output_dir, ref_geo, verbose = verbose)
      gc(verbose = FALSE) # Force le nettoyage RAM
    }
  }
  log_msg("\nTerminé !", verbose = verbose)
}