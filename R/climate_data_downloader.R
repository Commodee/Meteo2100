# ============================================================================
# SCRIPT DE TÉLÉCHARGEMENT DE DONNÉES CLIMATIQUES - VERSION 2.1
# ============================================================================
# Description: Fonctions pour télécharger les données climatiques françaises
# Sources: Météo-France (données historiques, AROME, ARPEGE)
# Date: Novembre 2024
# ============================================================================

# ============================================================================
# INSTALLATION ET CHARGEMENT DES PACKAGES
# ============================================================================

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


# ============================================================================
# 1. DONNÉES HISTORIQUES MÉTÉO-FRANCE
# ============================================================================

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
                                       save_dir = "./data") {
  
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


#' Télécharger les données météo pour plusieurs départements
#'
#' @param departements Vecteur de codes départements
#' @param annee Année
#' @param save_dir Dossier de sauvegarde
#' @return Liste de dataframes

download_meteo_multi <- function(departements, 
                                 annee = 2023,
                                 save_dir = "./data") {
  
  cat("\nTéléchargement multiple\n")
  cat("Départements:", length(departements), "\n")
  cat("Année:", annee, "\n\n")
  
  resultats <- list()
  
  for (dept in departements) {
    cat("\n--- Département", dept, "---\n")
    data <- download_meteo_departement(dept, annee, save_dir)
    
    if (!is.null(data)) {
      resultats[[dept]] <- data
    }
    
    Sys.sleep(1)
  }
  
  cat("\nTéléchargement terminé\n")
  cat("Départements réussis:", length(resultats), "/", length(departements), "\n\n")
  
  return(resultats)
}


#' Agréger les données météo par période
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
# 2. API AROME (Prévisions haute résolution)
# ============================================================================

#' Télécharger les prévisions AROME via Open-Meteo API
#' 
#' Open-Meteo fournit un accès gratuit aux modèles AROME et ARPEGE de Météo-France
#' Documentation: https://open-meteo.com/en/docs/meteofrance-api
#'
#' @param lat Latitude (ex: 48.8566 pour Paris)
#' @param lon Longitude (ex: 2.3522 pour Paris)
#' @param variables Variables météo (temperature_2m, precipitation, wind_speed_10m, etc.)
#' @param forecast_days Nombre de jours de prévision (1 à 4)
#' @return Dataframe avec les prévisions

download_arome <- function(lat, 
                           lon, 
                           variables = c("temperature_2m", "precipitation", "wind_speed_10m"),
                           forecast_days = 2) {
  
  cat("\nTéléchargement des prévisions AROME via Open-Meteo\n")
  cat("Position: Lat", lat, "/ Lon", lon, "\n")
  cat("Variables:", paste(variables, collapse = ", "), "\n")
  cat("Prévisions:", forecast_days, "jours\n\n")
  
  # URL de l'API Open-Meteo pour Météo-France
  base_url <- "https://api.open-meteo.com/v1/meteofrance"
  
  # Construire les paramètres
  params <- list(
    latitude = lat,
    longitude = lon,
    hourly = paste(variables, collapse = ","),
    forecast_days = forecast_days,
    timezone = "Europe/Paris"
  )
  
  tryCatch({
    
    cat("Requête à Open-Meteo API...\n")
    
    response <- GET(
      base_url,
      query = params,
      timeout(30)
    )
    
    if (status_code(response) == 200) {
      
      data_json <- content(response, "parsed")
      
      # Extraire les données horaires
      if (!is.null(data_json$hourly)) {
        
        # Convertir en dataframe manuellement
        hourly <- data_json$hourly
        
        # Créer le dataframe de base avec time
        df <- data.frame(
          time = as.POSIXct(unlist(hourly$time), format = "%Y-%m-%dT%H:%M", tz = "Europe/Paris"),
          stringsAsFactors = FALSE
        )
        
        # Ajouter chaque variable
        for (var in names(hourly)) {
          if (var != "time") {
            df[[var]] <- unlist(hourly[[var]])
          }
        }
        
        cat("Données téléchargées:", nrow(df), "heures de prévisions\n")
        cat("Période:", min(df$time), "à", max(df$time), "\n\n")
        
        # Renommer les colonnes pour plus de clarté
        if ("temperature_2m" %in% names(df)) {
          names(df)[names(df) == "temperature_2m"] <- "Temperature_C"
        }
        if ("precipitation" %in% names(df)) {
          names(df)[names(df) == "precipitation"] <- "Precipitation_mm"
        }
        if ("wind_speed_10m" %in% names(df)) {
          names(df)[names(df) == "wind_speed_10m"] <- "Vent_kmh"
        }
        
        return(df)
        
      } else {
        cat("Aucune donnée horaire dans la réponse\n")
        return(NULL)
      }
      
    } else {
      cat("Erreur API:", status_code(response), "\n")
      cat("Message:", content(response, "text"), "\n")
      return(NULL)
    }
    
  }, error = function(e) {
    cat("Erreur:", conditionMessage(e), "\n")
    return(NULL)
  })
}


#' Informations sur l'API AROME
info_arome <- function() {
  
  cat("\n")
  cat("API AROME - Prévisions haute résolution\n")
  cat("========================================\n\n")
  
  cat("IMPLÉMENTATION UTILISÉE\n")
  cat("-----------------------\n")
  cat("Ce script utilise Open-Meteo API (gratuit, sans clé)\n")
  cat("Open-Meteo combine les modèles AROME et ARPEGE de Météo-France\n")
  cat("URL: https://open-meteo.com/en/docs/meteofrance-api\n\n")
  
  cat("CARACTÉRISTIQUES AROME\n")
  cat("----------------------\n")
  cat("Modèle: AROME (Application of Research to Operations at MEsoscale)\n")
  cat("Résolution: 1.3 km\n")
  cat("Couverture: France métropolitaine\n")
  cat("Échéance: 0 à 48 heures\n")
  cat("Fréquence: Mise à jour toutes les heures\n\n")
  
  cat("VARIABLES DISPONIBLES\n")
  cat("---------------------\n")
  cat("- temperature_2m : Température à 2m (°C)\n")
  cat("- precipitation : Précipitations (mm)\n")
  cat("- wind_speed_10m : Vitesse du vent à 10m (km/h)\n")
  cat("- wind_direction_10m : Direction du vent\n")
  cat("- relative_humidity_2m : Humidité relative\n")
  cat("- cloud_cover : Nébulosité (%)\n")
  cat("- pressure_msl : Pression au niveau de la mer (hPa)\n\n")
  
  cat("UTILISATION\n")
  cat("-----------\n")
  cat("# Télécharger prévisions pour Paris (2 jours)\n")
  cat("arome_data <- download_arome(\n")
  cat("  lat = 48.8566,\n")
  cat("  lon = 2.3522,\n")
  cat("  variables = c('temperature_2m', 'precipitation'),\n")
  cat("  forecast_days = 2\n")
  cat(")\n\n")
  
  cat("AVANTAGES OPEN-METEO\n")
  cat("--------------------\n")
  cat("- Gratuit, sans inscription\n")
  cat("- Pas de clé API nécessaire\n")
  cat("- Format JSON facile à utiliser\n")
  cat("- Combine automatiquement AROME et ARPEGE\n\n")
  
  cat("ALTERNATIVE (API OFFICIELLE)\n")
  cat("----------------------------\n")
  cat("Pour utiliser l'API officielle Météo-France:\n")
  cat("1. Créer un compte: https://portail-api.meteofrance.fr/\n")
  cat("2. Souscrire à l'API AROME\n")
  cat("3. Obtenir 3 clés API différentes (AROME, ARPEGE, autres)\n")
  cat("4. Traiter les données GRIB2\n\n")
  
  return(invisible(NULL))
}


# ============================================================================
# 3. API ARPEGE (Prévisions globales)
# ============================================================================

#' Télécharger les prévisions ARPEGE via Open-Meteo API
#' 
#' Open-Meteo fournit un accès gratuit aux modèles AROME et ARPEGE de Météo-France
#' Documentation: https://open-meteo.com/en/docs/meteofrance-api
#'
#' @param lat Latitude
#' @param lon Longitude
#' @param variables Variables météo
#' @param forecast_days Nombre de jours de prévision (1 à 4)
#' @return Dataframe avec les prévisions

download_arpege <- function(lat, 
                            lon, 
                            variables = c("temperature_2m", "precipitation", "wind_speed_10m", "pressure_msl"),
                            forecast_days = 4) {
  
  cat("\nTéléchargement des prévisions ARPEGE via Open-Meteo\n")
  cat("Position: Lat", lat, "/ Lon", lon, "\n")
  cat("Variables:", paste(variables, collapse = ", "), "\n")
  cat("Prévisions:", forecast_days, "jours\n\n")
  
  # Même API que AROME (Open-Meteo combine les deux modèles)
  base_url <- "https://api.open-meteo.com/v1/meteofrance"
  
  params <- list(
    latitude = lat,
    longitude = lon,
    hourly = paste(variables, collapse = ","),
    forecast_days = forecast_days,
    timezone = "auto"
  )
  
  tryCatch({
    
    cat("Requête à Open-Meteo API...\n")
    
    response <- GET(
      base_url,
      query = params,
      timeout(30)
    )
    
    if (status_code(response) == 200) {
      
      data_json <- content(response, "parsed")
      
      if (!is.null(data_json$hourly)) {
        
        # Convertir en dataframe manuellement
        hourly <- data_json$hourly
        
        # Créer le dataframe de base avec time
        df <- data.frame(
          time = as.POSIXct(unlist(hourly$time), format = "%Y-%m-%dT%H:%M"),
          stringsAsFactors = FALSE
        )
        
        # Ajouter chaque variable
        for (var in names(hourly)) {
          if (var != "time") {
            df[[var]] <- unlist(hourly[[var]])
          }
        }
        
        cat("Données téléchargées:", nrow(df), "heures de prévisions\n")
        cat("Période:", min(df$time), "à", max(df$time), "\n\n")
        
        # Renommer les colonnes
        if ("temperature_2m" %in% names(df)) {
          names(df)[names(df) == "temperature_2m"] <- "Temperature_C"
        }
        if ("precipitation" %in% names(df)) {
          names(df)[names(df) == "precipitation"] <- "Precipitation_mm"
        }
        if ("wind_speed_10m" %in% names(df)) {
          names(df)[names(df) == "wind_speed_10m"] <- "Vent_kmh"
        }
        if ("pressure_msl" %in% names(df)) {
          names(df)[names(df) == "pressure_msl"] <- "Pression_hPa"
        }
        
        return(df)
        
      } else {
        cat("Aucune donnée horaire dans la réponse\n")
        return(NULL)
      }
      
    } else {
      cat("Erreur API:", status_code(response), "\n")
      return(NULL)
    }
    
  }, error = function(e) {
    cat("Erreur:", conditionMessage(e), "\n")
    return(NULL)
  })
}


#' Informations sur l'API ARPEGE
info_arpege <- function() {
  
  cat("\n")
  cat("API ARPEGE - Prévisions globales\n")
  cat("=================================\n\n")
  
  cat("IMPLÉMENTATION UTILISÉE\n")
  cat("-----------------------\n")
  cat("Ce script utilise Open-Meteo API (gratuit, sans clé)\n")
  cat("URL: https://open-meteo.com/en/docs/meteofrance-api\n\n")
  
  cat("CARACTÉRISTIQUES ARPEGE\n")
  cat("-----------------------\n")
  cat("Modèle: ARPEGE (Action de Recherche Petite Échelle Grande Échelle)\n")
  cat("Résolution: Variable (5 km en France, 25 km ailleurs)\n")
  cat("Couverture: Mondiale\n")
  cat("Échéance: 0 à 102 heures (4 jours)\n")
  cat("Fréquence: Mise à jour 4 fois par jour\n\n")
  
  cat("VARIABLES DISPONIBLES\n")
  cat("---------------------\n")
  cat("- temperature_2m : Température à 2m (°C)\n")
  cat("- precipitation : Précipitations (mm)\n")
  cat("- wind_speed_10m : Vitesse du vent à 10m (km/h)\n")
  cat("- pressure_msl : Pression au niveau de la mer (hPa)\n")
  cat("- relative_humidity_2m : Humidité relative\n")
  cat("- cloud_cover : Nébulosité\n\n")
  
  cat("DIFFÉRENCES AVEC AROME\n")
  cat("----------------------\n")
  cat("ARPEGE: Couverture mondiale, 4 jours, résolution 5-25km\n")
  cat("AROME: France uniquement, 2 jours, résolution 1.3km\n")
  cat("Open-Meteo combine automatiquement les deux modèles\n\n")
  
  cat("UTILISATION\n")
  cat("-----------\n")
  cat("# Prévisions à 4 jours pour Lyon\n")
  cat("arpege_data <- download_arpege(\n")
  cat("  lat = 45.75,\n")
  cat("  lon = 4.85,\n")
  cat("  variables = c('temperature_2m', 'precipitation', 'pressure_msl'),\n")
  cat("  forecast_days = 4\n")
  cat(")\n\n")
  
  return(invisible(NULL))
}


# ============================================================================
# 4. DONNÉES ÉMISSIONS GES (TÉLÉCHARGEMENT MANUEL)
# ============================================================================

#' Informations pour télécharger les données GES
info_download_ges <- function() {
  
  cat("\n")
  cat("DONNÉES ÉMISSIONS GES - Téléchargement manuel\n")
  cat("==============================================\n\n")
  
  cat("SOURCE 1 : CITEPA via data.gouv.fr\n")
  cat("-----------------------------------\n")
  cat("URL: https://www.data.gouv.fr/fr/datasets/emissions-de-polluants-atmospheriques-et-de-gaz-a-effet-de-serre/\n\n")
  
  cat("SOURCE 2 : Ministère de la Transition Écologique\n")
  cat("-------------------------------------------------\n")
  cat("URL: https://www.statistiques.developpement-durable.gouv.fr/emissions-nationales-de-gaz-effet-de-serre-0\n\n")
  
  cat("Pour charger les données dans R:\n")
  cat("library(readr)\n")
  cat("ges_data <- read_csv('data/emissions_ges_2024.csv')\n\n")
  
  return(invisible(NULL))
}


# ============================================================================
# 5. DONNÉES PROJECTIONS DRIAS (TÉLÉCHARGEMENT MANUEL)
# ============================================================================

#' Informations pour télécharger les projections DRIAS
info_download_drias <- function() {
  
  cat("\n")
  cat("PROJECTIONS CLIMATIQUES DRIAS - Téléchargement manuel\n")
  cat("======================================================\n\n")
  
  cat("PORTAIL DRIAS\n")
  cat("-------------\n")
  cat("URL: https://www.drias-climat.fr/\n\n")
  
  cat("Étapes:\n")
  cat("1. Créer un compte gratuit\n")
  cat("2. Se connecter\n")
  cat("3. Aller dans 'Espace Données et Produits'\n")
  cat("4. Sélectionner scénario (RCP 2.6, 4.5, 8.5) et horizon\n")
  cat("5. Télécharger au format CSV\n\n")
  
  return(invisible(NULL))
}


# ============================================================================
# 6. FONCTIONS UTILITAIRES
# ============================================================================

#' Obtenir les codes départements français
get_departements <- function() {
  depts_metro <- c(
    "01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
    "11", "12", "13", "14", "15", "16", "17", "18", "19", "21",
    "22", "23", "24", "25", "26", "27", "28", "29", "2A", "2B",
    "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
    "40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
    "50", "51", "52", "53", "54", "55", "56", "57", "58", "59",
    "60", "61", "62", "63", "64", "65", "66", "67", "68", "69",
    "70", "71", "72", "73", "74", "75", "76", "77", "78", "79",
    "80", "81", "82", "83", "84", "85", "86", "87", "88", "89",
    "90", "91", "92", "93", "94", "95"
  )
  depts_dom <- c("971", "972", "973", "974", "976")
  return(c(depts_metro, depts_dom))
}


#' Afficher les codes départements avec noms
get_departements_noms <- function() {
  depts <- data.frame(
    code = c("01", "02", "06", "13", "25", "33", "34", "35", "38", "44", 
             "59", "62", "63", "67", "69", "75", "76", "83", "84", "92"),
    nom = c("Ain", "Aisne", "Alpes-Maritimes", "Bouches-du-Rhône", "Doubs",
            "Gironde", "Hérault", "Ille-et-Vilaine", "Isère", "Loire-Atlantique",
            "Nord", "Pas-de-Calais", "Puy-de-Dôme", "Bas-Rhin", "Rhône", 
            "Paris", "Seine-Maritime", "Var", "Vaucluse", "Hauts-de-Seine"),
    stringsAsFactors = FALSE
  )
  return(depts)
}


#' Coordonnées des grandes villes françaises
get_villes_coordonnees <- function() {
  villes <- data.frame(
    ville = c("Paris", "Marseille", "Lyon", "Toulouse", "Nice", 
              "Nantes", "Strasbourg", "Montpellier", "Bordeaux", "Lille"),
    lat = c(48.8566, 43.2965, 45.7640, 43.6047, 43.7102,
            47.2184, 48.5734, 43.6108, 44.8378, 50.6292),
    lon = c(2.3522, 5.3698, 4.8357, 1.4442, 7.2620,
            -1.5536, 7.7521, 3.8767, -0.5792, 3.0573),
    stringsAsFactors = FALSE
  )
  return(villes)
}


# ============================================================================
# 7. FONCTION PRINCIPALE
# ============================================================================

#' Télécharger les données climatiques
#'
#' @param type "historique", "arome", "arpege", ou "complet"
#' @param departements Vecteur de codes départements (pour historique)
#' @param lat Latitude (pour AROME/ARPEGE)
#' @param lon Longitude (pour AROME/ARPEGE)
#' @param annee Année des données historiques
#' @param granularite "jour", "mois", "annee"
#' @param save_dir Dossier de sauvegarde
#' @return Liste avec les données téléchargées

telecharger_donnees_climat <- function(type = "historique",
                                       departements = "75",
                                       lat = NULL,
                                       lon = NULL,
                                       annee = 2023,
                                       granularite = "mois",
                                       save_dir = "./data_climat") {
  
  cat("\nTéléchargement des données climatiques\n")
  cat("=======================================\n\n")
  
  resultats <- list()
  
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  if (type %in% c("historique", "complet")) {
    cat("DONNÉES HISTORIQUES\n")
    cat("-------------------\n")
    
    if (length(departements) == 1) {
      data <- download_meteo_departement(departements, annee, save_dir)
      if (!is.null(data)) {
        resultats$historique <- aggregate_meteo(data, granularite)
      }
    } else {
      data_multi <- download_meteo_multi(departements, annee, save_dir)
      if (length(data_multi) > 0) {
        combined <- bind_rows(data_multi)
        resultats$historique <- aggregate_meteo(combined, granularite)
      }
    }
  }
  
  if (type %in% c("arome", "complet") && !is.null(lat) && !is.null(lon)) {
    cat("\nPRÉVISIONS AROME\n")
    cat("----------------\n")
    resultats$arome <- download_arome(lat, lon)
  }
  
  if (type %in% c("arpege", "complet") && !is.null(lat) && !is.null(lon)) {
    cat("\nPRÉVISIONS ARPEGE\n")
    cat("-----------------\n")
    resultats$arpege <- download_arpege(lat, lon)
  }
  
  cat("\nTéléchargement terminé\n\n")
  
  return(resultats)
}


# ============================================================================
# 8. EXEMPLES D'UTILISATION
# ============================================================================

exemples_utilisation <- function() {
  
  cat("\n")
  cat("EXEMPLES D'UTILISATION\n")
  cat("======================\n\n")
  
  cat("1. DONNÉES HISTORIQUES - UN DÉPARTEMENT\n")
  cat("----------------------------------------\n")
  cat("data_paris <- download_meteo_departement('75', 2023)\n")
  cat("data_mois <- aggregate_meteo(data_paris, 'mois')\n\n")
  
  cat("2. DONNÉES HISTORIQUES - PLUSIEURS DÉPARTEMENTS\n")
  cat("------------------------------------------------\n")
  cat("depts <- c('75', '69', '13')  # Paris, Lyon, Marseille\n")
  cat("data_multi <- download_meteo_multi(depts, 2023)\n\n")
  
  cat("3. PRÉVISIONS AROME (haute résolution)\n")
  cat("---------------------------------------\n")
  cat("# Configurer la clé API\n")
  cat("Sys.setenv(METEOFRANCE_API_KEY = 'votre_cle')\n\n")
  cat("# Télécharger\n")
  cat("arome_paris <- download_arome(lat = 48.8566, lon = 2.3522)\n\n")
  
  cat("4. PRÉVISIONS ARPEGE (globales)\n")
  cat("--------------------------------\n")
  cat("arpege_lyon <- download_arpege(lat = 45.75, lon = 4.85, echeance = 72)\n\n")
  
  cat("5. TÉLÉCHARGEMENT COMPLET\n")
  cat("-------------------------\n")
  cat("resultats <- telecharger_donnees_climat(\n")
  cat("  type = 'complet',\n")
  cat("  departements = c('75', '69'),\n")
  cat("  lat = 48.8566,\n")
  cat("  lon = 2.3522,\n")
  cat("  annee = 2023,\n")
  cat("  granularite = 'mois'\n")
  cat(")\n\n")
  
  cat("6. INFORMATIONS SUR LES APIs\n")
  cat("-----------------------------\n")
  cat("info_arome()           # Infos sur AROME\n")
  cat("info_arpege()          # Infos sur ARPEGE\n")
  cat("info_download_ges()    # Données GES\n")
  cat("info_download_drias()  # Projections DRIAS\n\n")
  
  cat("7. COORDONNÉES DES VILLES\n")
  cat("-------------------------\n")
  cat("villes <- get_villes_coordonnees()\n")
  cat("View(villes)\n\n")
}


# ============================================================================
# AFFICHAGE AU CHARGEMENT
# ============================================================================

cat("\n")
cat("Script de téléchargement de données climatiques - Version 2.1\n")
cat("==============================================================\n\n")

cat("Fonctions principales:\n\n")
cat("  HISTORIQUE:\n")
cat("    download_meteo_departement()  - Télécharger un département\n")
cat("    download_meteo_multi()        - Plusieurs départements\n")
cat("    aggregate_meteo()             - Agréger par période\n\n")
cat("  PRÉVISIONS:\n")
cat("    download_arome()              - Prévisions haute résolution\n")
cat("    download_arpege()             - Prévisions globales\n\n")
cat("  TOUT-EN-UN:\n")
cat("    telecharger_donnees_climat()  - Fonction principale\n\n")

cat("Informations:\n\n")
cat("  info_arome()                  - Guide API AROME\n")
cat("  info_arpege()                 - Guide API ARPEGE\n")
cat("  info_download_ges()           - Données GES\n")
cat("  info_download_drias()         - Projections DRIAS\n")
cat("  exemples_utilisation()        - Tous les exemples\n\n")

cat("Configuration API:\n")
cat("  Sys.setenv(METEOFRANCE_API_KEY = 'votre_cle')\n")
cat("  Inscription: https://portail-api.meteofrance.fr/\n\n")

cat("Démarrage rapide:\n")
cat("  install_packages()            # Installer dépendances\n")
cat("  exemples_utilisation()        # Voir les exemples\n\n")