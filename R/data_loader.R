#' Chargement et préparation des données globales
#'
#' @return Une liste nommée contenant :
#' \itemize{
#'   \item \code{regions} : Objet sf des régions.
#'   \item \code{departements} : Objet sf des départements.
#'   \item \code{drias} : Dataframe des projections climatiques nettoyées.
#'   \item \code{meteo} : Connexion Arrow vers le dataset météo complet.
#'   \item \code{meteo_nationale} : Dataframe agrégé au niveau national.
#'   \item \code{meteo_regionale} : Dataframe agrégé au niveau régional.
#'   \item \code{meteo_departementale} : Dataframe agrégé au niveau départemental.
#' }
#'
#' @examples
#' \dontrun{
#' global_data <- load_raw_data()
#' }
load_raw_data <- function() {
  # 1. Configuration des chemins
  data_dir <- "../data"
  parquet_dir <- file.path(data_dir, "meteo_parquet")
  
  # Vérification dossier Data
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
    message("Dossier 'data' créé.")
  }
  
  # 2. Gestion des Données Météo (Parquet)
  # Si le dossier n'existe pas, on télécharge et dézippe
  if (!dir.exists(parquet_dir)) {
    message("Données météo introuvables. Téléchargement depuis HuggingFace...")
    
    
    zip_url <- "https://huggingface.co/datasets/meteo2100/meteo2100/resolve/main/meteo_parquet.zip?download=true"
    dest_zip <- file.path(data_dir, "meteo_parquet.zip")
    
    tryCatch({
      response <- GET(zip_url,
                      write_disk(dest_zip, overwrite = TRUE),
                      progress(),
                      timeout(600))
      
      if (status_code(response) == 200) {
        message("Décompression de l'archive...")
        unzip(dest_zip, exdir = data_dir)
        message("Données météo installées avec succès.")
      } else {
        stop(paste(
          "Échec téléchargement. Code HTTP :",
          status_code(response)
        ))
      }
      
    }, error = function(e) {
      stop("Erreur critique lors du téléchargement : ", e$message)
    }, finally = {
      if (file.exists(dest_zip))
        unlink(dest_zip)
    })
  }
  
  # Chargement des parquets avec arrow
  data_meteo_arrow <- arrow::open_dataset(parquet_dir)
  
  # Sous fonction
  load_or_create_rds <- function(file_name, create_func) {
    path <- file.path(data_dir, file_name)
    if (file.exists(path)) {
      message(paste("Chargement cache :", file_name))
      return(readRDS(path))
    } else {
      message(paste("Création/Téléchargement :", file_name))
      data <- create_func()
      saveRDS(data, path)
      return(data)
    }
  }
  
  # 3. Gestion DRIAS
  dest_txt <- file.path(data_dir, "prevision_DRIAS.txt")
  if (!file.exists(dest_txt)) {
    message("Téléchargement des projections DRIAS...")
    tryCatch({
      resp <- GET(
        "https://huggingface.co/datasets/torvikk/meteo2100/resolve/main/prevision_DRIAS.txt?download=true",
        write_disk(dest_txt, overwrite = TRUE),
        progress(),
        timeout(60)
      )
      if (status_code(resp) != 200)
        stop("Erreur 404/401 sur l'URL DRIAS")
    }, error = function(e) {
      warning("Impossible de télécharger DRIAS. Vérifiez l'URL dans data_loader.R")
      return(data.frame(
        Contexte = character(),
        annee = numeric(),
        Temp_moy = numeric()
      ))
    })
  }
  drias_data <- process_drias_projections(dest_txt)
  
  # 4. Gestion Carte
  # Chargement Régions
  france_regions <- load_or_create_rds("regions.rds", function() {
    url <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/regions.geojson"
    st_read(url, quiet = TRUE) %>% rename(NOM_REGION = nom)
  })
  
  # Chargement Départements
  france_departements <- load_or_create_rds("departements.rds", function() {
    url <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson"
    st_read(url, quiet = TRUE) %>% rename(NOM_DEPT = nom)
  })
  
  #5. Gestion aggregation
  # Aggregation France
  meteo_nationale <- load_or_create_rds("meteo_nationale.rds", function() {
    aggregate_meteo(data_meteo_arrow, "jour", "Nationale")
  })
  
  # Aggregation Régionale
  meteo_regionale <- load_or_create_rds("meteo_regionale.rds", function() {
    aggregate_meteo(data_meteo_arrow, "jour", "Régionale")
  })
  
  # Aggregation Départementale
  meteo_departementale <- load_or_create_rds("meteo_departementale.rds", function() {
    aggregate_meteo(data_meteo_arrow, "jour", "Départementale")
  })
  
  return(
    list(
      regions = france_regions,
      departements = france_departements,
      drias = drias_data,
      meteo = data_meteo_arrow,
      meteo_nationale = meteo_nationale,
      meteo_regionale = meteo_regionale,
      meteo_departementale = meteo_departementale
    )
  )
}