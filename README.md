# Meteo2100

**Analyse du climat passé et projections futures en France (1950 - 2100).**

Ce projet de Master 2 est une application **R Shiny** interactive permettant d'explorer les données météorologiques historiques de Météo-France et de visualiser les projections climatiques selon les scénarios du GIEC.

## Fonctionnalités

  * **Analyse Historique** : Visualisation des températures et précipitations (Moyennes, Min, Max) à l'échelle nationale, régionale, départementale ou par station.
  * **Cartographie Interactive** : Carte dynamique (Leaflet) pour observer les disparités climatiques sur le territoire.
  * **Projections 2100** : Simulation des trajectoires de température selon les scénarios du GIEC (Optimiste RCP 2.6, Intermédiaire RCP 4.5, Pessimiste RCP 8.5) basées sur les données DRIAS.

## Installation

1.  Assurez-vous d'avoir **R** et **RStudio** installés.
2.  Installez les librairies nécessaires via la console R :

```r
install.packages(c("shiny", "shinyWidgets", "tidyverse", "arrow", "leaflet", "sf", "httr", "waiter"))
```

## Utilisation

1.  Ouvrez le fichier `R/app.R` dans RStudio.
2.  Cliquez sur le bouton **Run App** (ou exécutez `shiny::runApp("R/app.R")`).
3.  **Note importante** : Au premier lancement, l'application téléchargera et configurera automatiquement les données (\~600 Mo depuis HuggingFace). Cela peut prendre quelques minutes.

## Données

Les données sont traitées et agrégées automatiquement par l'application :

  * **Historique** : Données publiques Météo-France (1950-2024).
  * **Projections** : Données DRIAS ajustées (Correction de biais par rapport à l'historique local).
  * **Géographie** : Contours administratifs (API Géo / GeoJSON).

## Auteurs

* Victor Frison
* Jonas Carlu
* Adrien Mathieu