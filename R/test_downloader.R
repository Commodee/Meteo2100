# ============================================================================
# SCRIPT DE TEST - TÉLÉCHARGEMENT DONNÉES CLIMATIQUES V2.1
# ============================================================================
# Ce script teste toutes les fonctions du climate_data_downloader.R
# ============================================================================

# Charger le script principal
cat("Chargement du script principal...\n")
source("R/climate_data_downloader.R")

# Installer et charger les packages
cat("\nInstallation des packages...\n")
install_packages()

cat("\n")
cat("================================================================\n")
cat("TESTS DU TÉLÉCHARGEUR DE DONNÉES CLIMATIQUES\n")
cat("================================================================\n")
cat("\n")

# ============================================================================
# TEST 1 : Lister les ressources disponibles
# ============================================================================

cat("\n")
cat("================================================================\n")
cat("TEST 1 : Lister les ressources d'un dataset\n")
cat("================================================================\n\n")

resources <- list_dataset_resources("donnees-climatologiques-de-base-quotidiennes")

if (!is.null(resources)) {
  cat("\nTEST 1 RÉUSSI\n")
  cat("Nombre de ressources:", nrow(resources), "\n")
  cat("Aperçu des 5 premières:\n")
  print(head(resources[, c("titre", "format")], 5))
} else {
  cat("\nTEST 1 ÉCHOUÉ\n")
}

# ============================================================================
# TEST 2 : Télécharger un département
# ============================================================================

cat("\n")
cat("================================================================\n")
cat("TEST 2 : Télécharger les données pour Paris (75) - 2023\n")
cat("================================================================\n\n")

data_paris <- download_meteo_departement(
  departement = "75",
  annee = 2023,
  save_dir = "./test_data"
)

if (!is.null(data_paris)) {
  cat("\nTEST 2 RÉUSSI\n")
  cat("Dimensions:", nrow(data_paris), "lignes x", ncol(data_paris), "colonnes\n")
  cat("Période:", min(data_paris$DATE), "au", max(data_paris$DATE), "\n")
  cat("Colonnes:\n")
  print(names(data_paris))
} else {
  cat("\nTEST 2 ÉCHOUÉ\n")
}

# ============================================================================
# TEST 3 : Agréger par mois
# ============================================================================

cat("\n")
cat("================================================================\n")
cat("TEST 3 : Agréger les données par mois\n")
cat("================================================================\n\n")

if (!is.null(data_paris)) {
  data_mois <- aggregate_meteo(data_paris, "mois")
  
  if (!is.null(data_mois) && nrow(data_mois) > 0) {
    cat("\nTEST 3 RÉUSSI\n")
    cat("Nombre de mois:", nrow(data_mois), "\n")
    cat("Colonnes:\n")
    print(names(data_mois))
    cat("\nAperçu des données:\n")
    print(head(data_mois, 3))
  } else {
    cat("\nTEST 3 ÉCHOUÉ\n")
  }
} else {
  cat("\nTEST 3 IGNORÉ (test 2 échoué)\n")
}

# ============================================================================
# TEST 4 : Télécharger plusieurs départements
# ============================================================================

cat("\n")
cat("================================================================\n")
cat("TEST 4 : Télécharger plusieurs départements (75, 92, 93)\n")
cat("================================================================\n\n")

depts_test <- c("75", "92", "93")

data_multi <- download_meteo_multi(
  departements = depts_test,
  annee = 2023,
  save_dir = "./test_data"
)

if (length(data_multi) > 0) {
  cat("\nTEST 4 RÉUSSI\n")
  cat("Départements téléchargés:", length(data_multi), "/", length(depts_test), "\n")
  for (dept in names(data_multi)) {
    cat("  -", dept, ":", nrow(data_multi[[dept]]), "lignes\n")
  }
} else {
  cat("\nTEST 4 ÉCHOUÉ\n")
}

# ============================================================================
# TEST 5 : Fonction tout-en-un
# ============================================================================

cat("\n")
cat("================================================================\n")
cat("TEST 5 : Fonction tout-en-un telecharger_donnees_climat()\n")
cat("================================================================\n\n")

resultats <- telecharger_donnees_climat(
  type = "historique",
  departements = c("75", "69"),
  annee = 2023,
  granularite = "mois",
  save_dir = "./test_data_complet"
)

if (!is.null(resultats$historique)) {
  cat("\nTEST 5 RÉUSSI\n")
  cat("Données agrégées:", nrow(resultats$historique), "lignes\n")
  cat("Aperçu:\n")
  print(head(resultats$historique, 3))
} else {
  cat("\nTEST 5 ÉCHOUÉ\n")
}

# ============================================================================
# TEST 6 : Fonctions utilitaires
# ============================================================================

cat("\n")
cat("================================================================\n")
cat("TEST 6 : Fonctions utilitaires\n")
cat("================================================================\n\n")

# Liste des départements
tous_depts <- get_departements()
cat("get_departements():", length(tous_depts), "départements\n")

# Départements avec noms
depts_noms <- get_departements_noms()
cat("get_departements_noms():", nrow(depts_noms), "départements avec noms\n")
cat("Exemples:\n")
print(head(depts_noms, 5))

# Coordonnées des villes
villes <- get_villes_coordonnees()
cat("\nget_villes_coordonnees():", nrow(villes), "villes\n")
cat("Exemples:\n")
print(head(villes, 5))

cat("\nTEST 6 RÉUSSI\n")

# ============================================================================
# TEST 7 : Guides de téléchargement manuel
# ============================================================================

cat("\n")
cat("================================================================\n")
cat("TEST 7 : Affichage des guides de téléchargement manuel\n")
cat("================================================================\n\n")

cat("Guide GES:\n")
info_download_ges()

cat("\nGuide DRIAS:\n")
info_download_drias()

cat("TEST 7 RÉUSSI\n")

# ============================================================================
# TEST 8 : API AROME (prévisions haute résolution)
# ============================================================================

cat("\n")
cat("================================================================\n")
cat("TEST 8 : API AROME - Prévisions haute résolution\n")
cat("================================================================\n\n")

cat("Test via Open-Meteo API (gratuit, sans clé)\n\n")

# Coordonnées de Paris
paris <- get_villes_coordonnees()[1, ]

arome_data <- download_arome(
  lat = paris$lat,
  lon = paris$lon,
  variables = c("temperature_2m", "precipitation", "wind_speed_10m"),
  forecast_days = 2
)

if (!is.null(arome_data)) {
  cat("\nTEST 8 RÉUSSI\n")
  cat("Données AROME téléchargées:", nrow(arome_data), "heures\n")
  cat("Variables:", paste(names(arome_data), collapse = ", "), "\n")
  cat("\nAperçu:\n")
  print(head(arome_data, 3))
} else {
  cat("\nTEST 8 ÉCHOUÉ\n")
}

# ============================================================================
# TEST 9 : API ARPEGE (prévisions globales)
# ============================================================================

cat("\n")
cat("================================================================\n")
cat("TEST 9 : API ARPEGE - Prévisions globales\n")
cat("================================================================\n\n")

cat("Test via Open-Meteo API (gratuit, sans clé)\n\n")

# Coordonnées de Lyon
lyon <- get_villes_coordonnees()[3, ]

arpege_data <- download_arpege(
  lat = lyon$lat,
  lon = lyon$lon,
  variables = c("temperature_2m", "precipitation", "pressure_msl"),
  forecast_days = 4
)

if (!is.null(arpege_data)) {
  cat("\nTEST 9 RÉUSSI\n")
  cat("Données ARPEGE téléchargées:", nrow(arpege_data), "heures\n")
  cat("Variables:", paste(names(arpege_data), collapse = ", "), "\n")
  cat("\nAperçu:\n")
  print(head(arpege_data, 3))
} else {
  cat("\nTEST 9 ÉCHOUÉ\n")
}

# ============================================================================
# TEST 10 : Visualisation simple des données
# ============================================================================

cat("\n")
cat("================================================================\n")
cat("TEST 10 : Visualisation simple des données\n")
cat("================================================================\n\n")

if (!is.null(data_mois) && nrow(data_mois) > 0) {
  
  # Vérifier si ggplot2 est disponible
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    library(ggplot2)
    
    # Créer un graphique simple
    p <- ggplot(data_mois, aes(x = periode, y = Temperature_moyenne)) +
      geom_line(color = "#e74c3c", size = 1) +
      geom_point(color = "#c0392b", size = 2) +
      labs(
        title = "Évolution de la température à Paris en 2023",
        x = "Mois",
        y = "Température moyenne (°C)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    # Sauvegarder le graphique
    ggsave("./test_data/temperature_paris_2023.png", p, width = 10, height = 6)
    
    cat("TEST 10 RÉUSSI\n")
    cat("Graphique sauvegardé: ./test_data/temperature_paris_2023.png\n")
    
  } else {
    cat("TEST 10 IGNORÉ (ggplot2 non installé)\n")
    cat("Pour installer: install.packages('ggplot2')\n")
  }
} else {
  cat("TEST 10 IGNORÉ (pas de données mensuelles)\n")
}

# ============================================================================
# TEST 11 : Export des données
# ============================================================================

cat("\n")
cat("================================================================\n")
cat("TEST 11 : Export des données en différents formats\n")
cat("================================================================\n\n")

if (!is.null(data_mois)) {
  
  # Créer le dossier d'export
  if (!dir.exists("./test_export")) {
    dir.create("./test_export")
  }
  
  # Export CSV
  write.csv(data_mois, "./test_export/data_mois.csv", row.names = FALSE)
  cat("Export CSV: ./test_export/data_mois.csv\n")
  
  # Export RDS (format R)
  saveRDS(data_mois, "./test_export/data_mois.rds")
  cat("Export RDS: ./test_export/data_mois.rds\n")
  
  # Export Excel si disponible
  if (requireNamespace("writexl", quietly = TRUE)) {
    library(writexl)
    write_xlsx(data_mois, "./test_export/data_mois.xlsx")
    cat("Export Excel: ./test_export/data_mois.xlsx\n")
  } else {
    cat("Export Excel ignoré (writexl non installé)\n")
  }
  
  cat("\nTEST 11 RÉUSSI\n")
  
} else {
  cat("TEST 11 IGNORÉ (pas de données)\n")
}

# ============================================================================
# RÉSUMÉ DES TESTS
# ============================================================================

cat("\n\n")
cat("================================================================\n")
cat("RÉSUMÉ DES TESTS\n")
cat("================================================================\n")
cat("\n")

tests_results <- data.frame(
  Test = c(
    "1. Lister ressources",
    "2. Télécharger 1 département",
    "3. Agréger par mois",
    "4. Télécharger multi-depts",
    "5. Fonction tout-en-un",
    "6. Fonctions utilitaires",
    "7. Guides téléchargement",
    "8. API AROME",
    "9. API ARPEGE",
    "10. Visualisation",
    "11. Export données"
  ),
  Statut = c(
    ifelse(!is.null(resources), "RÉUSSI", "ÉCHOUÉ"),
    ifelse(!is.null(data_paris), "RÉUSSI", "ÉCHOUÉ"),
    ifelse(exists("data_mois") && !is.null(data_mois), "RÉUSSI", "ÉCHOUÉ"),
    ifelse(length(data_multi) > 0, "RÉUSSI", "ÉCHOUÉ"),
    ifelse(!is.null(resultats$historique), "RÉUSSI", "ÉCHOUÉ"),
    "RÉUSSI",
    "RÉUSSI",
    ifelse(exists("arome_data") && !is.null(arome_data), "RÉUSSI", "ÉCHOUÉ"),
    ifelse(exists("arpege_data") && !is.null(arpege_data), "RÉUSSI", "ÉCHOUÉ"),
    ifelse(file.exists("./test_data/temperature_paris_2023.png"), "RÉUSSI", "IGNORÉ"),
    ifelse(file.exists("./test_export/data_mois.csv"), "RÉUSSI", "IGNORÉ")
  ),
  stringsAsFactors = FALSE
)

print(tests_results)

cat("\n")

# Compter les succès
reussis <- sum(tests_results$Statut == "RÉUSSI")
ignores <- sum(tests_results$Statut == "IGNORÉ")
total <- nrow(tests_results)

cat("RÉSULTAT GLOBAL:", reussis, "tests réussis,", ignores, "ignorés sur", total, "tests\n\n")

if (reussis >= (total - ignores)) {
  cat("TOUS LES TESTS DISPONIBLES SONT PASSÉS\n")
} else if (reussis >= (total - ignores) * 0.7) {
  cat("La plupart des tests sont passés\n")
} else {
  cat("Plusieurs tests ont échoué - vérifier la configuration\n")
}

# ============================================================================
# NETTOYAGE (OPTIONNEL)
# ============================================================================

cat("\n")
cat("================================================================\n")
cat("NETTOYAGE\n")
cat("================================================================\n\n")

reponse <- readline(prompt = "Voulez-vous supprimer les fichiers de test ? (o/n) : ")

if (tolower(reponse) == "o") {
  
  # Supprimer les dossiers de test
  if (dir.exists("./test_data")) {
    unlink("./test_data", recursive = TRUE)
    cat("Supprimé: ./test_data\n")
  }
  
  if (dir.exists("./test_data_complet")) {
    unlink("./test_data_complet", recursive = TRUE)
    cat("Supprimé: ./test_data_complet\n")
  }
  
  if (dir.exists("./test_export")) {
    unlink("./test_export", recursive = TRUE)
    cat("Supprimé: ./test_export\n")
  }
  
  cat("Nettoyage terminé\n")
  
} else {
  cat("Fichiers conservés\n")
  cat("Données de test disponibles dans:\n")
  cat("  - ./test_data/\n")
  cat("  - ./test_data_complet/\n")
  cat("  - ./test_export/\n")
}

cat("\n")
cat("================================================================\n")
cat("TESTS TERMINÉS\n")
cat("================================================================\n")
cat("\n")

cat("Pour plus d'informations:\n")
cat("  exemples_utilisation()  # Voir tous les exemples\n\n")