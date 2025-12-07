process_drias_projections <- function(chemin_fichier) {
  raw_data <- read.csv2(chemin_fichier, sep = ";", header = TRUE, dec=".")
  
  full_data <- raw_data %>%
    mutate(
      annee = case_when(
        Periode == "REF" ~ 2005,
        Periode == "H1" ~ 2025,
        Periode == "H2" ~ 2055,
        Periode == "H3" ~ 2085,
        TRUE ~ as.numeric(as.character(Periode)) # Sécurité si jamais
      ),
      
      Contexte = case_when(
        str_detect(Contexte, "2.6") ~ "rcp26",
        str_detect(Contexte, "4.5") ~ "rcp45",
        str_detect(Contexte, "8.5") ~ "rcp85",
        TRUE ~ "historique"
      )
    )
  
  summarise_data <- full_data %>%
    group_by(Contexte, annee) %>%
    summarise(
      Temp_moy = mean(Temp_moy, na.rm = TRUE),
      Temp_min = mean(Temp_min, na.rm = TRUE),
      Temp_max = mean(Temp_max, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(summarise_data)
}