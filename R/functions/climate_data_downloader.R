install_packages <- function() {
  packages <- c("httr", "jsonlite", "dplyr", "lubridate", "tidyr", 
                "readr", "stringr", "purrr")
  
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    cat("Installation des packages:", paste(new_packages, collapse=", "), "\n")
    install.packages(new_packages)
  }
  
  invisible(lapply(packages, library, character.only = TRUE))
  cat("Packages chargés\n\n")
}




#' Lister les ressources disponibles pour un dataset
#'
#' @param dataset_id ID du dataset sur data.gouv.fr
#' @return Liste des ressources avec leurs métadonnées

list_dataset_resources <- function(dataset_id) {
  
  cat("Récupération des ressources du dataset...\n")
  
  url <- paste0("https://www.data.gouv.fr/api/1/datasets/", dataset_id, "/")
  
  tryCatch({
    response <- GET(url)
    
    if (status_code(response) != 200) {
      cat("Erreur lors de la récupération des métadonnées\n")
      return(NULL)
    }
    
    dataset_info <- content(response, "parsed")
    resources <- dataset_info$resources
    
    cat("Dataset:", dataset_info$title, "\n")
    cat("Nombre de ressources:", length(resources), "\n\n")
    
    resources_df <- map_df(resources, function(r) {
      data.frame(
        titre = r$title,
        id = r$id,
        format = r$format,
        url = r$url,
        taille = ifelse(is.null(r$filesize), NA, r$filesize),
        stringsAsFactors = FALSE
      )
    })
    
    return(resources_df)
    
  }, error = function(e) {
    cat("Erreur:", conditionMessage(e), "\n")
    return(NULL)
  })
}


#' Télécharger une ressource spécifique depuis data.gouv.fr
#'
#' @param resource_url URL de la ressource à télécharger
#' @param output_file Nom du fichier de sortie (optionnel)
#' @return Chemin du fichier téléchargé

download_resource <- function(resource_url, output_file = NULL) {
  
  cat("Téléchargement de la ressource...\n")
  cat("URL:", resource_url, "\n")
  
  if (is.null(output_file)) {
    output_file <- tempfile(fileext = ".csv.gz")
  }
  
  tryCatch({
    response <- GET(resource_url, write_disk(output_file, overwrite = TRUE), timeout(300))
    
    if (status_code(response) != 200) {
      cat("Erreur de téléchargement:", status_code(response), "\n")
      return(NULL)
    }
    
    file_size <- file.info(output_file)$size
    cat("Fichier téléchargé:", output_file, "\n")
    cat("Taille:", round(file_size / 1024 / 1024, 2), "Mo\n")
    
    return(output_file)
    
  }, error = function(e) {
    cat("Erreur:", conditionMessage(e), "\n")
    return(NULL)
  })
}


#' Télécharger les données météo historiques par département
#'
#' @param departement Code du département (ex: "75", "13", "69")
#' @param annee Année des données (ex: 2023)
#' @param save_dir Dossier de sauvegarde
#' @return Dataframe avec les données ou NULL

download_meteo_departement <- function(departement, 
                                       annee = 2023,
                                       save_dir = NULL) {
  
  cat("\nTéléchargement des données Météo-France (historique)\n")
  cat("Département:", departement, "\n")
  cat("Année:", annee, "\n\n")
  
  dataset_id <- "donnees-climatologiques-de-base-quotidiennes"
  
  resources <- list_dataset_resources(dataset_id)
  
  if (is.null(resources)) {
    cat("Impossible de récupérer les ressources\n")
    return(NULL)
  }
  
  dept_resources <- resources[grepl(paste0("departement_", departement, "_"), resources$titre, ignore.case = TRUE), ]
  
  if (nrow(dept_resources) == 0) {
    cat("Aucune ressource trouvée pour le département", departement, "\n")
    return(NULL)
  }
  
  matching <- NULL
  for (i in 1:nrow(dept_resources)) {
    titre <- dept_resources$titre[i]
    periode_match <- regmatches(titre, regexpr("periode_[0-9]{4}-[0-9]{4}", titre))
    
    if (length(periode_match) > 0) {
      annees <- as.numeric(unlist(strsplit(gsub("periode_", "", periode_match), "-")))
      annee_debut <- annees[1]
      annee_fin <- annees[2]
      
      if (annee >= annee_debut && annee <= annee_fin && grepl("RR-T-Vent", titre)) {
        matching <- dept_resources[i, ]
        break
      }
    }
  }
  
  if (is.null(matching) || nrow(matching) == 0) {
    cat("Aucune ressource trouvée pour le département", departement, "en", annee, "\n")
    return(NULL)
  }
  
  cat("Ressource trouvée:", matching$titre[1], "\n\n")
  
  file_path <- download_resource(matching$url[1])
  
  if (is.null(file_path)) {
    return(NULL)
  }
  
  cat("\nLecture des données...\n")
  
  tryCatch({
    data <- read_csv2(file_path, 
                      locale = locale(encoding = "UTF-8"),
                      col_types = cols(.default = "c"))
    
    cat("Données chargées:", nrow(data), "lignes\n")
    
    if (grepl("^/tmp", file_path)) {
      unlink(file_path)
    }
    
    if ("AAAAMMJJ" %in% names(data)) {
      data <- data %>%
        mutate(DATE = ymd(AAAAMMJJ))
    } else if ("DATE" %in% names(data)) {
      data <- data %>%
        mutate(DATE = ymd(DATE))
    } else {
      cat("Colonne de date non trouvée\n")
      return(NULL)
    }
    
    data <- data %>%
      filter(year(DATE) == annee) %>%
      mutate(
        ANNEE = year(DATE),
        MOIS = month(DATE)
      )
    
    if (nrow(data) == 0) {
      cat("Aucune donnée pour l'année", annee, "\n")
      return(NULL)
    }
    
    cat("Données pour", annee, ":", nrow(data), "lignes\n")
    
    if (!is.null(save_dir)) {
      if (!dir.exists(save_dir)) {
        dir.create(save_dir, recursive = TRUE)
      }
      
      output_path <- file.path(save_dir, paste0("meteo_", departement, "_", annee, ".csv"))
      write_csv(data, output_path)
      cat("Sauvegardé:", output_path, "\n")
    }
    
    return(data)
    
  }, error = function(e) {
    cat("Erreur lors de la lecture:", conditionMessage(e), "\n")
    return(NULL)
  })
}


#' Télécharger les données météo pour plusieurs départements (Format Stacké)
#'
#' @param departements Vecteur de codes départements
#' @param annee Année
#' @param save_dir Dossier de sauvegarde
#' @return Un unique Dataframe contenant tous les départements

download_meteo_multi <- function(departements, 
                                 annee = 2023,
                                 save_dir = NULL) {
  
  cat("\nTéléchargement multiple (Stacké + Enrichi)\n")
  cat("Départements:", paste(departements, collapse=", "), "\n")
  cat("Année:", annee, "\n\n")
  
  liste_temporaire <- list()
  
  # --- PHASE 1 : TÉLÉCHARGEMENT ---
  for (dept in departements) {
    cat("--- Traitement du département", dept, "---\n")
    data <- download_meteo_departement(dept, annee, save_dir)
    
    if (!is.null(data)) {
      data <- data %>% mutate(CODE_DEPT = dept)
      liste_temporaire[[dept]] <- data
    }
    Sys.sleep(1)
  }
  
  # --- PHASE 2 : ASSEMBLAGE ---
  if (length(liste_temporaire) > 0) {
    cat("\nAssemblage des données...\n")
    dataframe_final <- bind_rows(liste_temporaire)
    
    # --- PHASE 3 : ENRICHISSEMENT GÉOGRAPHIQUE ---
    dataframe_final <- enrichir_donnees_geo(dataframe_final) %>% 
      select(DATE, ANNEE, MOIS, AAAAMMJJ, NUM_POSTE, NOM_USUEL, CODE_DEPT, NOM_DEPT, NOM_REGION, CODE_REGION, everything())

    
    cat("Terminé ! Total lignes :", nrow(dataframe_final), "\n")
    return(dataframe_final)
    
  } else {
    cat("Aucune donnée récupérée.\n")
    return(NULL)
  }
}


#' Agréger les données météo par période et par 
#'
#' @param data Dataframe avec les données quotidiennes
#' @param granularite "jour", "semaine", "mois", "annee" 
#' @return Dataframe agrégé

aggregate_meteo <- function(data, granularite = "mois") {
  
  cat("Agrégation des données:", granularite, "\n")
  
  if (granularite == "jour") {
    return(data)
  }
  
  data <- data %>%
    mutate(
      TM = as.numeric(TM),
      TN = as.numeric(TN),
      TX = as.numeric(TX),
      RR = as.numeric(RR)
    )
  
  data <- data %>%
    mutate(
      periode = case_when(
        granularite == "semaine" ~ floor_date(DATE, "week"),
        granularite == "mois" ~ floor_date(DATE, "month"),
        granularite == "annee" ~ floor_date(DATE, "year"),
        TRUE ~ DATE
      )
    )
  
  result <- data %>%
    group_by(periode) %>%
    summarise(
      Temperature_moyenne = mean(TM, na.rm = TRUE),
      Temperature_min = mean(TN, na.rm = TRUE),
      Temperature_max = mean(TX, na.rm = TRUE),
      Precipitation_mm = sum(RR, na.rm = TRUE),
      nb_observations = n(),
      .groups = "drop"
    )
  
  cat("Agrégation réussie:", nrow(result), "périodes\n\n")
  
  return(result)
}






# ============================================================================
# 2. FONCTIONS UTILITAIRES
# ============================================================================

#' Récupérer le référentiel Géographique (Départements et Régions)
#' Source: API Géo (geo.api.gouv.fr)
#' @return Un dataframe de référence

get_referentiel_geo <- function() {
  cat("Récupération du référentiel géographique (API Géo)...\n")
  
  # On demande : nom du dept, code du dept, et infos de la région parente
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
    
    cat("Référentiel chargé :", nrow(referentiel), "départements trouvés.\n")
    return(referentiel)
    
  }, error = function(e) {
    cat("Erreur lors de la récupération du référentiel géo :", conditionMessage(e), "\n")
    return(NULL)
  })
}

#' Ajouter les noms de départements et régions aux données météo
#'
#' @param data_meteo Le dataframe issu de download_meteo_multi (doit avoir CODE_DEPT)
#' @return Le dataframe enrichi

enrichir_donnees_geo <- function(data_meteo) {
  
  if (is.null(data_meteo)) return(NULL)
  
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