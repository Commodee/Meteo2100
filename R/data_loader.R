load_raw_data <- function() {
  url_regions <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/regions.geojson"
  url_departements <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson"
  #url_communes <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes.geojson"
  
  france_regions <- st_read(url_regions, quiet = FALSE) %>% rename(
    NOM_REGION = nom
    )
    
  france_departements <- st_read(url_departements, quiet = FALSE) %>% 
    rename(
      NOM_DEPT = nom
    )
    
  #france_communes <- st_read(url_communes, quiet = TRUE)
  
  dossier_parquet <- "../data/meteo_parquet"
  liste_depts <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
                       "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21",
                       "22", "23", "24", "25", "26", "27", "28", "29",
                       "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
                       "40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
                       "50", "51", "52", "53", "54", "55", "56", "57", "58", "59",
                       "60", "61", "62", "63", "64", "65", "66", "67", "68", "69",
                       "70", "71", "72", "73", "74", "75", "76", "77", "78", "79",
                       "80", "81", "82", "83", "84", "85", "86", "87", "88", "89",
                       "90", "91", "92", "93", "94", "95","971", "972", "973", "974", "976")
  
  # download_meteo_multi_parquet(liste_depts ,output_dir = dossier_parquet, mode = "full")
  
  
  
  data_meteo_arrow <- arrow::open_dataset(dossier_parquet)
  
  meteo_nationale <- aggregate_meteo(data_meteo_arrow, "jour", "Nationale")
  meteo_regionale <- aggregate_meteo(data_meteo_arrow, "jour", "Régionale")
  meteo_departementale <- aggregate_meteo(data_meteo_arrow, "jour", "Départementale")
  
  
  return(list(
    regions = france_regions,
    departements = france_departements,
    meteo = data_meteo_arrow,
    meteo_nationale = meteo_nationale,
    meteo_regionale= meteo_regionale,
    meteo_departementale = meteo_departementale
  )
  )
}