source("functions/climate_data_downloader.R")

load_raw_data <- function() {
  url_regions <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/regions.geojson"
  url_departements <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson"
  #url_communes <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes.geojson"
  
  france_regions <- st_read(url_regions, quiet = FALSE)
  france_departements <- st_read(url_departements, quiet = FALSE)
  #france_communes <- st_read(url_communes, quiet = TRUE)
  
  install_packages()
  data_meteo <- download_meteo_multi(
    c("01","02","25","12","89","74","66")
  )
  
  return(list(
    regions = france_regions,
    departements = france_departements,
    meteo = data_meteo
    )
  )
}