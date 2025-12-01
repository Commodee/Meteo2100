plotplot <- function(data, niveau_geo, input_geo, granularite_temps){
  data_mieux <- aggregate_meteo(data, granularite_temps, niveau_geo, input_geo)
  
  shiny::validate(
    shiny::need(
      nrow(data_mieux) > 0, 
      "Aucune donnée météo ne correspond à votre sélection géographique et temporelle."
    )
  )
  
  return(ggplot(data_mieux, aes(x = periode, y = Temperature_moyenne))+geom_line())
}