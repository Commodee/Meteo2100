# helper functions --------------------------------------------------------

#' Message de log conditionnel
#'
#' @param ... Éléments à concaténer dans le message.
#' @param verbose Logique. Si TRUE, affiche le message.
#'
#' @return Aucune valeur retournée (effet de bord).
log_msg <- function(..., verbose = TRUE) {
  if (verbose) cat(..., "\n")
}


# list dataset resources --------------------------------------------------

#' Lister les ressources d'un jeu de données data.gouv
#'
#' @param dataset_id Chaîne de caractères. L'identifiant du dataset sur data.gouv.fr.
#' @param verbose Logique. Affiche les logs si TRUE.
#'
#' @return Un tibble contenant le titre, l'ID, l'URL et la période extraite de chaque ressource, ou NULL en cas d'erreur.
list_dataset_resources <- function(dataset_id, verbose = TRUE) {
  url <- paste0("https://www.data.gouv.fr/api/1/datasets/", dataset_id, "/")
  
  tryCatch({
    response <- httr::GET(url)
    if (httr::status_code(response) != 200) return(NULL)
    
    content <- httr::content(response, "parsed", encoding = "UTF-8")
    
    purrr::map_df(content$resources, function(r) {
      tibble::tibble(
        titre = r$title,
        id = r$id,
        url = r$url,
        periode_str = stringr::str_extract(r$title, "periode_(\\d{4})-(\\d{4})")
      )
    })
  }, error = function(e) {
    warning("Erreur API data.gouv: ", conditionMessage(e))
    return(NULL)
  })
}


# get referentiel geo -----------------------------------------------------

#' Récupérer le référentiel géographique officiel
#'
#' @param verbose Logique. Affiche les logs si TRUE.
#'
#' @return Un dataframe avec les codes et noms des départements et régions.
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


# process one resource ----------------------------------------------------

#' Traitement d'une ressource météo unique
#'
#' Télécharge un fichier CSV (compressé ou non) depuis une URL, le nettoie,
#' ajoute les informations géographiques et le sauvegarde au format Parquet.
#'
#' @param resource Liste ou ligne de dataframe contenant l'url et le titre de la ressource.
#' @param dept_code Chaîne de caractères. Code du département.
#' @param output_dir Chaîne de caractères. Dossier de sortie.
#' @param ref_geo Dataframe. Le référentiel géographique pour la jointure.
#' @param verbose Logique. Affiche les logs si TRUE.
#'
#' @return Logique. TRUE si succès ou si le fichier existe déjà.
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
    response <- httr::GET(resource$url, httr::write_disk(tmp_csv, overwrite = TRUE), httr::timeout(600))
    
    if (httr::status_code(response) == 200) {
      # Utilisation de readr::read_delim au lieu de data.table::fread
      df <- readr::read_delim(
        tmp_csv,
        delim = ";",
        locale = readr::locale(encoding = "UTF-8"),
        col_types = readr::cols(.default = readr::col_character()),
        show_col_types = FALSE,
        progress = FALSE
      )
      
      col_date <- intersect(names(df), c("AAAAMMJJ", "DATE"))[1]
      
      if (!is.na(col_date)) {
        df_clean <- df %>%
          dplyr::rename(DATE = dplyr::all_of(col_date)) %>%
          dplyr::mutate(
            CODE_DEPT = dept_code,
            DATE = lubridate::ymd(DATE),
            TM = as.numeric(TM),
            TN = as.numeric(TN),
            TX = as.numeric(TX),
            RR = as.numeric(RR),
            ANNEE = lubridate::year(DATE),
            MOIS = lubridate::month(DATE)
          )
        
        if (!is.null(ref_geo) & dept_code != "20") {
          df_clean <- dplyr::left_join(df_clean, ref_geo, by = "CODE_DEPT")
        } else {
          # Problème avec la Corse, on télécharge avec 20, mais ref geo attend 2A et 2B
          ref_geo_corse <- data.frame(
            CODE_DEPT = "20",
            NOM_DEPT = "Corse",
            CODE_REGION = "94",
            NOM_REGION = "Corse",
            stringsAsFactors = FALSE
          )
          df_clean <- dplyr::left_join(df_clean, ref_geo_corse, by = "CODE_DEPT")
        }
        
        arrow::write_parquet(df_clean, output_file)
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


# process department ------------------------------------------------------

#' Fonction helper pour traiter un département (utilisée par parallélisation)
#'
#' @param dept Chaîne de caractères. Code du département à traiter.
#' @param all_resources Dataframe. Toutes les ressources disponibles.
#' @param mode Chaîne de caractères. "full" ou "light".
#' @param annee (Optionnel) Numérique. Pour filtrer une année spécifique.
#' @param output_dir Chaîne de caractères. Dossier de destination.
#' @param ref_geo Dataframe. Le référentiel géographique.
#' @param verbose Logique. Affiche les logs si TRUE.
#'
#' @return Logique ou NULL.
process_department <- function(dept, all_resources, mode, annee, output_dir, ref_geo, verbose) {
  log_msg(paste0("\nTraite Dept: ", dept), verbose = verbose)
  
  pattern_dept <- paste0("departement_", dept, "_")
  dept_res <- all_resources %>%
    dplyr::filter(stringr::str_detect(titre, stringr::fixed(pattern_dept, ignore_case = TRUE)), 
                  stringr::str_detect(titre, "RR-T-Vent"))
  
  if (nrow(dept_res) == 0) {
    log_msg("   Aucune donnée trouvée.", verbose = verbose)
    return(NULL)
  }
  
  dept_res <- dept_res %>%
    dplyr::mutate(
      start_year = as.numeric(stringr::str_extract(periode_str, "\\d{4}")),
      end_year   = as.numeric(stringr::str_extract(periode_str, "(?<=-)\\d{4}"))
    ) %>%
    dplyr::filter(end_year >= 1950)
  
  if (mode == "light") {
    dept_res <- dept_res %>% dplyr::filter(start_year >= 2024)
    if (nrow(dept_res) == 0) {
      log_msg("   [Info] Mode Light : Aucun fichier récent (2024+) trouvé.", verbose = verbose)
      return(NULL)
    }
  }
  
  for (i in seq_len(nrow(dept_res))) {
    res <- dept_res[i, ]
    
    if (!is.null(annee)) {
      if (annee < res$start_year || annee > res$end_year) next
    }
    
    process_one_resource(res, dept, output_dir, ref_geo, verbose = verbose)
    gc(verbose = FALSE)
  }
  
  return(TRUE)
}


# download meteo multi parquet --------------------------------------------

#' Téléchargement des données météo
#'
#' Fonction principale pour télécharger et convertir en Parquet les données météo
#' pour une liste de départements. Supporte un mode "light" (uniquement données récentes)
#' ou "full" (historique complet depuis 1950).
#'
#' @param departements Vecteur de chaînes. Liste des codes départements à traiter.
#' @param mode Chaîne de caractères. "full" ou "light".
#' @param annee (Optionnel) Numérique. Pour filtrer une année spécifique.
#' @param output_dir Chaîne de caractères. Dossier de destination des fichiers Parquet.
#' @param parallel Logique. Active le traitement parallèle si TRUE.
#' @param n_cores Numérique. Nombre de cœurs à utiliser en mode parallèle.
#' @param verbose Logique. Affiche les logs si TRUE.
#'
#' @return NULL. Les fichiers sont sauvegardés sur le disque.
download_meteo_multi_parquet <- function(departements, 
                                         mode = "full", 
                                         annee = NULL, 
                                         output_dir = "../data/meteo_parquet",
                                         parallel = TRUE,
                                         n_cores = 4,
                                         verbose = TRUE) {
  
  # Chargement des packages AVANT la parallélisation
  suppressPackageStartupMessages({
    library(dplyr)
    library(httr)
    library(lubridate)
    library(readr)
    library(stringr)
    library(purrr)
    library(arrow)
    library(tibble)
  })
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  log_msg(paste0("=== Sync Météo (Mode : ", mode, ") ==="), verbose = verbose)
  
  ref_geo <- get_referentiel_geo(verbose = FALSE)
  dataset_id <- "donnees-climatologiques-de-base-quotidiennes"
  
  all_resources <- list_dataset_resources(dataset_id, verbose = verbose)
  if (is.null(all_resources)) return(NULL)
  
  if (parallel && length(departements) > 1) {
    if (!requireNamespace("future", quietly = TRUE) || !requireNamespace("furrr", quietly = TRUE)) {
      warning("Packages 'future' et 'furrr' requis pour la parallélisation. Mode séquentiel activé.")
      parallel <- FALSE
    }
  }
  
  if (parallel && length(departements) > 1) {
    library(future)
    library(furrr)
    
    # Configuration pour que les workers héritent des packages déjà chargés
    plan(multisession, workers = min(n_cores, length(departements)))
    log_msg(paste("Mode PARALLÈLE activé :", n_cores, "cœurs"), verbose = verbose)
    
    future_walk(departements, function(dept) {
      process_department(dept, all_resources, mode, annee, output_dir, ref_geo, verbose)
    }, .options = furrr_options(
      seed = TRUE,
      packages = c("dplyr", "httr", "lubridate", "readr", "stringr", "purrr", "arrow", "tibble")
    ))
    
    plan(sequential)
  } else {
    for (dept in departements) {
      process_department(dept, all_resources, mode, annee, output_dir, ref_geo, verbose)
    }
  }
  
  log_msg("\nTerminé !", verbose = verbose)
}