# Météo2100

**Analyse du climat passé et projections futures en France (1950 - 2100).**

Ce projet de Master 2 est une application **R Shiny** interactive permettant d'explorer les données météorologiques historiques de Météo-France et de visualiser les projections climatiques selon les scénarios du GIEC.

## Fonctionnalités

L'application est divisée en plusieurs modules d'exploration :

### 1. Où en est-on ? (Analyse Historique)
Visualisation temporelle détaillée des températures et précipitations.
*   **Échelles** : Nationale, Régionale, Départementale ou par Station Météo.
*   **Indicateurs** : Températures (Moyenne, Min, Max) et Précipitations.
*   **KPIs** : Affichage dynamique des records (Chaleur, Froid, Pluie, Sécheresse) sur la période sélectionnée.

### 2. Carte en folie (Cartographie)
Exploration spatiale interactive via **Leaflet**.
*   Comparaison des régions et départements.
*   Visualisation des disparités climatiques sur le territoire.

### 3. Et demain ? (Projections Climatiques)
Simulation des trajectoires de température jusqu'en 2100 basées sur les données **DRIAS**.
*   **Scénarios GIEC** :
    *   🟢 **RCP 2.6** (Optimiste - Accord de Paris)
    *   🟠 **RCP 4.5** (Intermédiaire)
    *   🔴 **RCP 8.5** (Pessimiste - Business as usual)
*   **Correction de Biais** : Les projections sont ajustées localement par rapport à l'historique observé (1976-2005).

### 4. Mise à jour des données
Un module dédié permet de mettre à jour les données météorologiques récentes (2025-2026) directement depuis **data.gouv.fr**.
*   Téléchargement automatique et parallélisé.
*   Recalcul des agrégats (National, Régional, Départemental).

---

## Installation

1.  Assurez-vous d'avoir **R** et **RStudio** installés.
2.  Installez les dépendances nécessaires via la console R :

```r
install.packages(c(
  "shiny", "bslib", "shinyWidgets", "shinycssloaders", "waiter",
  "tidyverse", "arrow", "sf", "leaflet", "httr",
  "future", "furrr", "parallel"
))
```

## Utilisation

1.  Ouvrez le projet `Meteo2100.Rproj` dans RStudio.
2.  Ouvrez le fichier `R/app.R`.
3.  Cliquez sur le bouton **Run App** (ou exécutez `shiny::runApp("R/app.R")`).

> **Note** : Au premier lancement, l'application peut prendre quelques minutes pour charger les données agrégées.

ou pour lancer l'application directement depuis la console R :

```r
shiny::runApp('R')
```

---

## Structure du Projet

```
Meteo2100/
├── data/                   # Données (RDS, Parquet)
├── R/
│   ├── app.R               # Point d'entrée de l'application (UI & Server)
│   ├── data_loader.R       # Chargement et gestion des données
│   └── functions/          # Fonctions utilitaires
│       ├── aggregate_meteo.R          # Logique d'agrégation
│       ├── climate_data_downloader.R  # Script de mise à jour (data.gouv)
│       ├── plot.R                     # Fonctions graphiques (ggplot2, leaflet)
│       ├── projections_loader.R       # Gestion des données DRIAS
│       └── ui_helpers.R               # Composants UI (Cartes KPI)
└── README.md
```

## Auteurs

Projet réalisé dans le cadre du Master 2 par :
*   **Victor Frison**
*   **Adrien Mathier**
*   **Jonas Carlu**