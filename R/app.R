# load libraries ----------------------------------------------------------
library(arrow)
library(bslib)
library(future)
library(furrr)
library(httr)
library(leaflet)
library(sf)
library(shiny)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse)
library(waiter)


# load externals scripts ----------------------------------------------------
source("data_loader.R")

source("functions/aggregate_meteo.R")
source("functions/climate_data_downloader.R")
source("functions/plot.R")
source("functions/projections_loader.R")


# ui ----------------------------------------------------------------------
ui <- page_navbar(
  id = "nav_principal",
  title = div(icon("cloud-sun"), "Météo2100", style = "font-weight: bold; font-size: 1.3em; margin-right: 30px; color: white;"),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    font_scale = 0.9,
    primary = "#3498db",
    success = "#34DBCA",
    info = "#9b59b6",
    warning = "#f39c12",
    danger = "#e74c3c",
    # base_font = font_google("Roboto"),
    # heading_font = font_google("Montserrat")
  ),
  fillable = TRUE,
  header = tagList(
    useWaiter(),
    tags$head(
      tags$style(HTML("
        .info-card {
          color: white;
          text-align: center;
          border-radius: 15px;
          padding: 20px;
          height: 100%;
          display: flex;
          flex-direction: column;
          justify-content: space-between;
          align-items: center;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
          transition: transform 0.2s;
        }
        .info-card:hover {
          transform: translateY(-5px);
        }
        .info-card-title {
          font-size: 1.1em;
          font-weight: 500;
          opacity: 0.9;
          margin-bottom: 10px;
        }
        .info-card-value {
          font-size: 2.5em;
          font-weight: bold;
          margin: 10px 0;
        }
        .info-card-desc {
          font-size: 0.9em;
          opacity: 0.8;
        }
        .info-tooltip {
          cursor: help;
          text-decoration: underline dotted;
        }
      "))
    )
  ),

  # tab_situation ----
  nav_panel(
    "Où en est on ?",
    value = "nav_situation",
    icon = icon("chart-line"),
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        accordion(
          id = "sit_accordion",
          multiple = FALSE,
          accordion_panel(
            "Données",
            icon = icon("database"),
            radioGroupButtons(
              inputId = "sit_input_var_type",
              label = "Variable :",
              choices = c("Temperature", "Precipitation"),
              selected = "Temperature",
              status = "primary",
              justified = TRUE,
              checkIcon = list(yes = icon("check"))
            ),
            uiOutput("sit_ui_temp_metric")
          ),
          accordion_panel(
            "Choix du Territoire",
            icon = icon("map-location-dot"),
            prettyRadioButtons(
              inputId = "sit_input_scale",
              label = "Échelle d'analyse :",
              choices = c(
                "France entière" = "Nationale",
                "Par Région" = "Régionale",
                "Par Département" = "Départementale",
                "Station Météo" = "Station Météo"
              ),
              selected = "Nationale",
              status = "primary",
              shape = "curve",
              outline = TRUE,
              animation = "pulse"
            ),
            uiOutput("sit_ui_location_selector")
          ),
          accordion_panel(
            "Temps",
            icon = icon("calendar"),
            prettyRadioButtons(
              inputId = "sit_input_temporal_scale",
              label = "Fréquence :",
              choices = c(
                "Jour" = "jour",
                "Mois" = "mois",
                "Année" = "annee"
              ),
              selected = "annee",
              status = "success",
              shape = "curve"
            ),
            uiOutput("sit_ui_date_range")
          )
        ) # accordion
      ),
      # sidebar

      layout_columns(
        col_widths = c(9, 3),
        card(
          full_screen = TRUE,
          card_header("Visualisation des données historiques"),
          textOutput("sit_text_info"),
          plotOutput("sit_plot_history", height = "500px") %>% withSpinner(color = "#3498db", type = 6)
        ),
        tagList(
          card(card_header("Info 1"), "Contenu vide", style = "height: 150px;"),
          card(card_header("Info 2"), "Contenu vide", style = "height: 150px;"),
          card(card_header("Info 3"), "Contenu vide", style = "height: 150px;")
        )
      ) # layout_columns
    ) # layout_sidebar
  ),
  # tab_situation

  # tab_carte ----
  nav_panel(
    "Carte en folie",
    value = "nav_map",
    icon = icon("map-marked-alt"),
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        accordion(
          multiple = FALSE,
          id = "map_accordion",
          accordion_panel(
            "Données",
            icon = icon("database"),
            radioGroupButtons(
              inputId = "map_input_var_type",
              label = "On affiche quoi ?",
              choices = c("Temperature", "Precipitation"),
              selected = "Temperature",
              status = "primary",
              justified = TRUE,
              checkIcon = list(yes = icon("check"))
            ),
            uiOutput("map_ui_temp_metric")
          ),
          accordion_panel(
            "Choix du Territoire",
            icon = icon("map-location-dot"),
            radioButtons(
              inputId = "map_input_scale",
              label = "Granularité",
              choices = c("Régionale", "Départementale"),
              selected = "Régionale"
            )
          ),
          accordion_panel(
            "Temps",
            icon = icon("calendar"),
            radioButtons(
              inputId = "map_input_temporal_scale",
              label = "Temporalité",
              choices = c(
                "Jour" = "jour",
                "Mois" = "mois",
                "Année" = "annee"
              ),
              selected = "jour"
            ),
            uiOutput("map_ui_date_selector")
          )
        ) # accordion
      ),
      # sidebar

      layout_columns(
        col_widths = c(9, 3),
        card(
          full_screen = TRUE,
          card_header("Exploration Cartographique"),
          card_body(padding = 0, leafletOutput("map_output_leaflet", height = "500px") %>% withSpinner(color = "#3498db", type = 6))
        ),
        tagList(
          uiOutput("map_info_max", style = "height: 180px; margin-bottom: 15px;"),
          uiOutput("map_info_min", style = "height: 180px; margin-bottom: 15px;"),
          uiOutput("map_info_mean", style = "height: 180px; margin-bottom: 15px;")
        )
      ) # layout_columns
    ) # layout_sidebar
  ),
  # tab_carte

  # tab_demain ----
  nav_panel(
    "Et demain ?",
    value = "nav_projection",
    icon = icon("hourglass"),
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        accordion(
          multiple = FALSE,
          id = "proj_accordion",
          accordion_panel(
            "Territoire",
            icon = icon("map-location-dot"),
            p("Simulez l'avenir selon les différents scénarios du GIEC."),
            radioButtons(
              inputId = "proj_input_scale",
              label = "Échelle :",
              choices = c("Nationale", "Régionale", "Départementale"),
              selected = "Régionale"
            ),
            uiOutput("proj_ui_location_selector")
          ),
          accordion_panel(
            "Scénario Climatique",
            icon = icon("globe"),
            radioButtons(
              inputId = "proj_input_scenario",
              label = "Scénario (GIEC) :",
              choices = c(
                "Optimiste (RCP 2.6)" = "rcp26",
                "Intermédiaire (RCP 4.5)" = "rcp45",
                "Pessimiste (RCP 8.5)" = "rcp85"
              ),
              selected = "rcp45"
            )
          )
        ) # accordion
      ),
      # sidebar

      layout_columns(
        col_widths = c(9, 3),
        card(
          card_header("Trajectoire de température"),
          plotOutput("proj_plot_trajectory", height = "500px") %>% withSpinner(color = "#3498db", type = 6)
        ),
        tagList(
          uiOutput("proj_card_scenario", style = "height: 180px; margin-bottom: 15px;"),
          uiOutput("proj_info_bias", style = "height: 180px; margin-bottom: 15px;"),
          uiOutput("proj_info_delta", style = "height: 180px; margin-bottom: 15px;")
        )
      ) # layout_columns
    ) # layout_sidebar
  ),
  # tab_demain

  # tab_update ----
  nav_panel(
    "Mise à jour",
    icon = icon("sync"),
    card(
      card_header("Mise à jour des données météorologiques"),
      p("Les données de base (Hugging Face) s'arrêtent au 27/11/2025."),
      p("Pour obtenir les données les plus récentes (2024-202x), vous pouvez lancer une mise à jour."),
      p("Cette opération va :"),
      tags$ul(
        tags$li("Supprimer les fichiers Parquet existants pour la période 2024-202x."),
        tags$li("Télécharger les dernières données disponibles depuis data.gouv.fr."),
        tags$li("Recalculer les agrégations (fichiers RDS).")
      ),
      br(),
      actionButton("btn_ask_update", "Lancer la mise à jour", class = "btn-warning", icon = icon("download"))
    )
  ),

  # footer ----
  footer = tags$footer(style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); text-align: center; font-size: 0.9em; color: white;", p(
    "Fait par : Victor Frison • Adrien Mathier • Jonas Carlu"
  ))
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  # Création de l'écran de chargement
  w <- Waiter$new(
    html = tagList(
      spin_flower(),
      h3("Chargement des données climatiques..."),
      p("Cela peut prendre 30 secondes au premier lancement")
    ),
    color = "rgba(52, 152, 219, 0.9)"
  )
  # On l'affiche
  w$show()

  # Ecran de chargement pour la mise à jour
  w_update <- Waiter$new(
    html = tagList(
      spin_refresh(),
      h3("Mise à jour et Recalcul..."),
      p("Cela peut prendre quelques minutes.")
    ),
    color = "rgba(52, 152, 219, 0.9)"
  )

  # Trigger pour recharger les données
  trigger_reload <- reactiveVal(0)

  # Chargement des données
  global_data_reactive <- reactive({
    trigger_reload()
    result <- load_raw_data()
    result
  })

  # Préparation des vecteurs de choix
  vec_dep <- reactive({
    global_data_reactive()$meteo_departementale %>%
      pull(NOM_DEPT) %>%
      unique() %>%
      sort()
  })

  vec_region <- reactive({
    global_data_reactive()$meteo_regionale %>%
      pull(NOM_REGION) %>%
      unique() %>%
      sort()
  })

  vec_commune <- reactive({
    global_data_reactive()$meteo %>%
      arrange(NOM_USUEL) %>%
      select(NOM_USUEL) %>%
      distinct(NOM_USUEL) %>%
      collect() %>%
      pull(NOM_USUEL)
  })

  # Logique de l'écran de chargement
  observe({
    req(global_data_reactive())
    # On force le calcul des vecteurs pour que l'interface soit fluide
    # et on attend qu'ils soient prêts pour cacher le loader
    vec_dep()
    vec_region()
    vec_commune()
    # on cache le loader quand tout est chargé
    w$hide()
    w_update$hide()
  })

  # ---- Tab Situation ----
  output$sit_ui_location_selector <- renderUI({
    switch(input$sit_input_scale,
      "Station Météo" = selectInput("sit_input_loc_commune", "Choisir la station météo", vec_commune()),
      "Départementale" = selectInput("sit_input_loc_dept", "Choisir le département", vec_dep()),
      "Régionale" = selectInput("sit_input_loc_reg", "Choisir la région", vec_region()),
      "Nationale" = NULL
    )
  })

  output$sit_ui_temp_metric <- renderUI({
    if (input$sit_input_var_type == "Temperature") {
      radioGroupButtons(
        inputId = "sit_input_temp_metric",
        label = "Quelle temperature ?",
        choices = c(
          "Max",
          "Min",
          "Moy",
          "3-en-1"
        ),
        selected = "Moy",
        status = "success",
        justified = TRUE,
        checkIcon = list(yes = icon("check"))
      )
    } else {
      NULL
    }
  })

  # Selcteur de date
  output$sit_ui_date_range <- renderUI({
    req(input$sit_input_temporal_scale)

    if (input$sit_input_temporal_scale == "annee") {
      # CAS 1 : ANNÉE
      # On veut choisir une plage d'années (ex: 1950 - 2023)
      airDatepickerInput(
        inputId = "sit_input_date_range",
        label = "Période (Années) :",
        range = TRUE,
        view = "years",
        minView = "years",
        dateFormat = "yyyy",
        value = c("1950-01-01", "2025-12-31")
      )
    } else if (input$sit_input_temporal_scale == "mois") {
      # CAS 2 : MOIS
      # On sélectionne des mois et années (ex: Jan 2020 - Dec 2022)
      airDatepickerInput(
        inputId = "sit_input_date_range",
        label = "Période (Mois) :",
        range = TRUE,
        view = "years",
        minView = "months",
        dateFormat = "MM/yyyy",
        value = c("2020-01-01", "2023-12-31")
      )
    } else {
      # CAS 3 : JOUR
      # On sélectionne des jours précis
      airDatepickerInput(
        inputId = "sit_input_date_range",
        label = "Période (Jours) :",
        range = TRUE,
        view = "months",
        minView = "days",
        dateFormat = "dd/MM/yyyy",
        # Par défaut on met les 6 derniers mois pour ne pas surcharger
        value = c(Sys.Date() - 180, Sys.Date())
      )
    }
  })

  # plot
  output$sit_plot_history <- renderPlot({
    req(
      input$sit_input_scale,
      input$sit_input_date_range,
      input$sit_input_var_type
    )

    date_deb <- as.Date(input$sit_input_date_range[1])
    date_fin <- as.Date(input$sit_input_date_range[2])
    if (input$sit_input_temporal_scale == "annee") {
      date_fin <- as.Date(paste0(year(date_fin), "-12-31"))
    }

    if (input$sit_input_scale == "Station Météo") {
      req(input$sit_input_loc_commune)


      data_filtered <- global_data_reactive()$meteo %>%
        filter(NOM_USUEL == input$sit_input_loc_commune) %>%
        filter(DATE >= date_deb, DATE <= date_fin) %>%
        select(DATE, TM, TN, TX, RR) %>%
        collect() %>%
        rename(
          periode = DATE,
          Temperature_moyenne = TM,
          Temperature_min = TN,
          Temperature_max = TX,
          Precipitation_mm_moy = RR
        ) %>%
        mutate(
          Temperature_moyenne = as.numeric(Temperature_moyenne),
          Temperature_min = as.numeric(Temperature_min),
          Temperature_max = as.numeric(Temperature_max),
          Precipitation_mm_moy = as.numeric(Precipitation_mm_moy)
        )

      titre <- input$sit_input_loc_commune
    } else {
      data_source <- switch(input$sit_input_scale,
        "Nationale" = global_data_reactive()$meteo_nationale,
        "Régionale" = global_data_reactive()$meteo_regionale,
        "Départementale" = global_data_reactive()$meteo_departementale
      )

      # Filtrage Géo
      if (input$sit_input_scale == "Régionale") {
        req(input$sit_input_loc_reg)
        data_source <- data_source %>% filter(NOM_REGION == input$sit_input_loc_reg)
        titre <- input$sit_input_loc_reg
      } else if (input$sit_input_scale == "Départementale") {
        req(input$sit_input_loc_dept)
        data_source <- data_source %>% filter(NOM_DEPT == input$sit_input_loc_dept)
        titre <- input$sit_input_loc_dept
      } else {
        titre <- "France Entière"
      }

      data_filtered <- data_source %>% filter(periode >= date_deb, periode <= date_fin)
    }

    shiny::validate(need(nrow(data_filtered) > 0, "Pas de données sur cette période."))

    # 2. Ré-agrégation Temporelle (si besoin)
    data_ready <- reaggregate_tempo(data_filtered, input$sit_input_temporal_scale)

    # 3. Plot
    if (input$sit_input_var_type == "Temperature") {
      req(input$sit_input_temp_metric)
      plot_temp(
        data_ready,
        titre,
        input$sit_input_temporal_scale,
        input$sit_input_temp_metric
      )
    } else {
      plot_prec(data_ready, titre, input$sit_input_temporal_scale)
    }
  })

  # ---- Tab Carte ----
  output$map_ui_temp_metric <- renderUI({
    if (input$map_input_var_type == "Temperature") {
      radioGroupButtons(
        inputId = "map_input_temp_metric",
        label = "Quelle temperature ?",
        choices = c("Max", "Min", "Moy"),
        selected = "Moy",
        status = "success",
        justified = TRUE,
        checkIcon = list(yes = icon("check"))
      )
    } else {
      div(
        class = "alert alert-info",
        style = "margin-top: 10px; font-size: 0.9em;",
        icon("info-circle"),
        tags$b("Note méthodologique :"), br(),
        "La valeur affichée correspond à la moyenne des précipitations sur le territoire, cumulée sur la période sélectionnée."
      )
    }
  })

  # Selcteur de date
  output$map_ui_date_selector <- renderUI({
    req(input$map_input_temporal_scale)

    if (input$map_input_temporal_scale == "annee") {
      # CAS 1 : ANNÉE
      airDatepickerInput(
        inputId = "map_input_date",
        label = "Quelle année :",
        range = FALSE,
        view = "years",
        minView = "years",
        dateFormat = "yyyy",
        value = c("2025-01-01")
      )
    } else if (input$map_input_temporal_scale == "mois") {
      airDatepickerInput(
        inputId = "map_input_date",
        label = "Quel mois :",
        range = FALSE,
        view = "years",
        minView = "months",
        dateFormat = "MM/yyyy",
        value = c("2020-01-01")
      )
    } else {
      # CAS 3 : JOUR
      airDatepickerInput(
        inputId = "map_input_date",
        label = "Quel jour :",
        range = FALSE,
        view = "months",
        minView = "days",
        dateFormat = "dd/MM/yyyy",
        value = "2025-01-01"
      )
    }
  })

  # Reactive pour les données de la carte
  map_data_reactive <- reactive({
    req(input$map_input_scale, input$map_input_date, input$map_input_var_type)

    # 1. Alignement Date
    date_cible <- as.Date(input$map_input_date)
    if (input$map_input_temporal_scale == "annee") {
      date_cible <- floor_date(date_cible, "year")
    }
    if (input$map_input_temporal_scale == "mois") {
      date_cible <- floor_date(date_cible, "month")
    }

    # 2. Choix Source
    if (input$map_input_scale == "Départementale") {
      map_geo <- global_data_reactive()$departements
      data_meteo <- global_data_reactive()$meteo_departementale
      key_col <- "NOM_DEPT"

      # Patch pour la Corse : Duplication de "Corse" vers "Corse-du-Sud" et "Haute-Corse"
      if ("Corse" %in% data_meteo$NOM_DEPT) {
        corse_data <- data_meteo %>% filter(NOM_DEPT == "Corse")

        corse_2a <- corse_data %>% mutate(NOM_DEPT = "Corse-du-Sud")
        corse_2b <- corse_data %>% mutate(NOM_DEPT = "Haute-Corse")

        data_meteo <- data_meteo %>%
          filter(NOM_DEPT != "Corse") %>%
          bind_rows(corse_2a, corse_2b)
      }
    } else {
      map_geo <- global_data_reactive()$regions
      data_meteo <- global_data_reactive()$meteo_regionale
      key_col <- "NOM_REGION"
    }

    # 3. Filtre Temporel
    # On filtre d'abord l'année pour réduire la taille des données
    annee_cible <- year(date_cible)
    data_subset <- data_meteo %>%
      filter(year(periode) == annee_cible)

    # 4. Ré-agrégation & Sélection finale
    # Transforme jour -> mois/année et garde la date cible
    data_final_meteo <- reaggregate_tempo(data_subset, input$map_input_temporal_scale) %>%
      filter(periode == date_cible)

    # Filtre DOM-TOM pour la carte et les indicateurs
    doms <- c("Guadeloupe", "Martinique", "Guyane", "La Réunion", "Mayotte")
    if (key_col == "NOM_DEPT") {
      data_final_meteo <- data_final_meteo %>% filter(!NOM_DEPT %in% doms)
    } else {
      data_final_meteo <- data_final_meteo %>% filter(!NOM_REGION %in% doms)
    }

    shiny::validate(need(
      nrow(data_final_meteo) > 0,
      paste("Pas de données pour", date_cible)
    ))

    # Vérification supplémentaire : si les lignes existent mais sont vides (NA)
    col_check <- if (input$map_input_var_type == "Temperature") {
      req(input$map_input_temp_metric)
      switch(input$map_input_temp_metric,
        "Max" = "Temperature_max",
        "Min" = "Temperature_min",
        "Moy" = "Temperature_moyenne",
        "Temperature_moyenne"
      )
    } else {
      "Precipitation_mm_moy"
    }

    if (col_check %in% names(data_final_meteo)) {
      shiny::validate(need(
        !all(is.na(data_final_meteo[[col_check]])),
        paste("Pas de données pour", date_cible)
      ))
    }

    list(
      data = data_final_meteo,
      geo = map_geo,
      key = key_col,
      col_var = col_check,
      date = date_cible
    )
  })

  output$map_output_leaflet <- renderLeaflet({
    res <- map_data_reactive()

    # 5. Jointure
    map_final <- res$geo %>% left_join(res$data, by = res$key)
    if (!inherits(map_final, "sf")) {
      map_final <- st_as_sf(map_final)
    }

    # 6. Plot
    if (input$map_input_var_type == "Temperature") {
      req(input$map_input_temp_metric)
    }

    plot_map_leaflet(
      data_map        = map_final,
      var_type        = input$map_input_var_type,
      # "Temperature" ou "Precipitation"
      temp_type       = input$map_input_temp_metric,
      # "Temperature moy", etc.
      col_name_region = res$key # "NOM_DEPT" ou "NOM_REGION"
    )
  })

  # Indicateur Max
  output$map_info_max <- renderUI({
    res <- map_data_reactive()
    data <- res$data
    col <- res$col_var

    # Trouver le max
    row_max <- data %>%
      filter(!!sym(col) == max(!!sym(col), na.rm = TRUE)) %>%
      slice(1)
    val_max <- row_max[[col]]
    nom_max <- row_max[[res$key]]

    is_temp <- input$map_input_var_type == "Temperature"
    unit <- if (is_temp) "°C" else "mm"

    # Style
    bg_color <- if (is_temp) {
      "linear-gradient(135deg, #e74c3c 0%, #c0392b 100%)" # Rouge
    } else {
      "linear-gradient(135deg, #3498db 0%, #2980b9 100%)" # Bleu
    }

    title_card <- if (is_temp) "Le plus chaud" else "Le plus pluvieux"

    div(
      class = "info-card",
      style = paste0("background: ", bg_color, ";"),
      div(class = "info-card-title", title_card),
      div(class = "info-card-value", paste0(round(val_max, 1), " ", unit)),
      div(class = "info-card-desc", nom_max)
    )
  })

  # Indicateur Min
  output$map_info_min <- renderUI({
    res <- map_data_reactive()
    data <- res$data
    col <- res$col_var

    # Trouver le min
    row_min <- data %>%
      filter(!!sym(col) == min(!!sym(col), na.rm = TRUE)) %>%
      slice(1)
    val_min <- row_min[[col]]
    nom_min <- row_min[[res$key]]

    is_temp <- input$map_input_var_type == "Temperature"
    unit <- if (is_temp) "°C" else "mm"

    # Style
    bg_color <- if (is_temp) {
      "linear-gradient(135deg, #3498db 0%, #2980b9 100%)" # Bleu
    } else {
      "linear-gradient(135deg, #f39c12 0%, #d35400 100%)" # Orange/Sec
    }

    title_card <- if (is_temp) "Le plus froid" else "Le plus sec"

    div(
      class = "info-card",
      style = paste0("background: ", bg_color, ";"),
      div(class = "info-card-title", title_card),
      div(class = "info-card-value", paste0(round(val_min, 1), " ", unit)),
      div(class = "info-card-desc", nom_min)
    )
  })

  # Indicateur Moyenne Nationale
  output$map_info_mean <- renderUI({
    res <- map_data_reactive()

    # On calcule la moyenne (France Métropolitaine)
    col <- res$col_var
    val_moy <- mean(res$data[[col]], na.rm = TRUE)

    is_temp <- input$map_input_var_type == "Temperature"
    unit <- if (is_temp) "°C" else "mm"

    bg_color <- "linear-gradient(135deg, #2ecc71 0%, #1abc9c 100%)" # Vert

    div(
      class = "info-card",
      style = paste0("background: ", bg_color, ";"),
      div(class = "info-card-title", "Moyenne Nationale"),
      div(class = "info-card-value", paste0(round(val_moy, 1), " ", unit)),
      div(class = "info-card-desc", "France Métropolitaine")
    )
  })

  # ---- Tab Demain ----
  output$proj_ui_location_selector <- renderUI({
    switch(input$proj_input_scale,
      "Nationale"      = NULL,
      "Régionale"      = selectInput("proj_input_loc_reg", "Région :", vec_region(), selected = "Île-de-France"),
      "Départementale" = selectInput("proj_input_loc_dept", "Département :", vec_dep())
    )
  })

  # Reactive pour les données de projection
  proj_data_reactive <- reactive({
    req(input$proj_input_scale, input$proj_input_scenario)
    if (input$proj_input_scale == "Régionale") {
      req(input$proj_input_loc_reg)
    }
    if (input$proj_input_scale == "Départementale") {
      req(input$proj_input_loc_dept)
    }

    # 1. Récupération de l'Historique
    if (input$proj_input_scale == "Nationale") {
      data_source <- global_data_reactive()$meteo_nationale
      titre <- "France Métropolitaine"
    } else if (input$proj_input_scale == "Régionale") {
      data_source <- global_data_reactive()$meteo_regionale %>%
        filter(NOM_REGION == input$proj_input_loc_reg)
      titre <- input$proj_input_loc_reg
    } else {
      # Départementale
      data_source <- global_data_reactive()$meteo_departementale %>%
        filter(NOM_DEPT == input$proj_input_loc_dept)
      titre <- input$proj_input_loc_dept
    }

    data_hist <- reaggregate_tempo(data_source, "annee") %>%
      mutate(annee = year(periode), scenario = "Historique")

    # 2. Récupération des Projections
    raw_proj <- global_data_reactive()$drias
    shiny::validate(need(nrow(raw_proj) > 0, "Données DRIAS introuvables."))

    # 3. Calcul du Biais (Offset)
    # On cale la courbe DRIAS sur la réalité historique locale (période 1976-2005)
    ref_hist <- mean(data_hist$Temperature_moyenne[data_hist$annee %in% 1976:2005], na.rm = TRUE)
    if (is.na(ref_hist)) {
      ref_hist <- mean(data_hist$Temperature_moyenne, na.rm = TRUE)
    } # Fallback

    ref_proj <- mean(raw_proj$Temp_moy[raw_proj$annee == 2005], na.rm = TRUE)
    offset <- ref_hist - ref_proj

    # 4. Calcul de la Normale 1991-2020 (pour le Delta)
    ref_normale <- mean(data_hist$Temperature_moyenne[data_hist$annee %in% 1991:2020], na.rm = TRUE)
    if (is.na(ref_normale)) {
      ref_normale <- ref_hist # Fallback
    }

    # 5. Ajustement des Projections
    data_proj_final <- raw_proj %>%
      mutate(
        Temperature_moyenne = Temp_moy + offset,
        Temperature_min     = Temp_min + offset,
        Temperature_max     = Temp_max + offset
      )

    list(
      data_hist = data_hist,
      data_proj = data_proj_final,
      offset = offset,
      titre = titre,
      ref_hist = ref_hist,
      ref_normale = ref_normale
    )
  })

  # Info 1 : Scénario
  output$proj_card_scenario <- renderUI({
    info <- switch(input$proj_input_scenario,
      "rcp26" = list(
        titre = "Scénario Optimiste",
        valeur = "RCP 2.6",
        desc = "Fortes réductions d'émissions (Accord de Paris). La température se stabilise vers 2050.",
        bg = "linear-gradient(135deg, #2ecc71 0%, #1abc9c 100%)"
      ),
      "rcp45" = list(
        titre = "Scénario Intermédiaire",
        valeur = "RCP 4.5",
        desc = "Les émissions plafonnent vers 2040. Le réchauffement ralentit mais continue.",
        bg = "linear-gradient(135deg, #f39c12 0%, #d35400 100%)"
      ),
      "rcp85" = list(
        titre = "Scénario Pessimiste",
        valeur = "RCP 8.5",
        desc = "Aucune régulation ('Business as Usual'). Hausse brutale et continue des températures.",
        bg = "linear-gradient(135deg, #e74c3c 0%, #c0392b 100%)"
      )
    )

    div(
      class = "info-card",
      style = paste0("background: ", info$bg, ";"),
      div(class = "info-card-title", info$titre),
      div(class = "info-card-value", info$valeur),
      div(
        class = "info-card-desc",
        info$desc
      )
    )
  })

  # Info 2 : Biais
  output$proj_info_bias <- renderUI({
    data <- proj_data_reactive()

    div(
      class = "info-card",
      style = "background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);",
      div(class = "info-card-title", "Correction de Biais"),
      div(class = "info-card-value", paste0(round(data$offset, 1), " °C")),
      div(
        class = "info-card-desc",
        tooltip(
          trigger = span("Explication", class = "info-tooltip", icon("info-circle")),
          "Les projections DRIAS sont basées sur la période 1976-2005. On applique un biais local en comparant notre historique (1976-2005) à la projection DRIAS de 2005.",
          placement = "top"
        )
      )
    )
  })

  # Info 3 : Delta
  output$proj_info_delta <- renderUI({
    data <- proj_data_reactive()

    # Dernière année disponible dans les projections
    last_year <- max(data$data_proj$annee, na.rm = TRUE)

    # Valeur future pour le scénario choisi
    future_val <- data$data_proj %>%
      filter(annee == last_year, Contexte == input$proj_input_scenario) %>%
      pull(Temperature_moyenne) %>%
      mean(na.rm = TRUE)

    delta <- future_val - data$ref_normale

    # Couleur dynamique selon le delta
    bg_color <- if (delta > 4) {
      "linear-gradient(135deg, #e74c3c 0%, #c0392b 100%)" # Rouge foncé
    } else if (delta > 2) {
      "linear-gradient(135deg, #f39c12 0%, #d35400 100%)" # Orange
    } else {
      "linear-gradient(135deg, #2ecc71 0%, #1abc9c 100%)" # Vert
    }

    div(
      class = "info-card",
      style = paste0("background: ", bg_color, ";"),
      div(class = "info-card-title", "Écart vs Normale"),
      div(class = "info-card-value", paste0("+", round(delta, 1), " °C")),
      div(
        class = "info-card-desc",
        tooltip(
          trigger = span(paste0("Horizon ", last_year), class = "info-tooltip", icon("info-circle")),
          paste0("Écart prévu en ", last_year, " par rapport à la normale 1991-2020."),
          placement = "top"
        )
      )
    )
  })

  # Graphique Projection
  output$proj_plot_trajectory <- renderPlot({
    data <- proj_data_reactive()

    plot_projection_graph(
      data_hist       = data$data_hist,
      data_proj       = data$data_proj,
      scenario_choisi = input$proj_input_scenario,
      titre           = data$titre,
      offset_val      = data$offset
    )
  })

  # ---- Pré-chargement ----
  # Permet de charger tout les inputs par default dans les accordéons
  # Sans cela les plots ne s"affichent pas

  # Onglet Situation
  outputOptions(output, "sit_ui_location_selector", suspendWhenHidden = FALSE)
  outputOptions(output, "sit_ui_temp_metric", suspendWhenHidden = FALSE)
  outputOptions(output, "sit_ui_date_range", suspendWhenHidden = FALSE)

  # Onglet Carte
  outputOptions(output, "map_ui_temp_metric", suspendWhenHidden = FALSE)
  outputOptions(output, "map_ui_date_selector", suspendWhenHidden = FALSE)

  # Onglet Demain
  outputOptions(output, "proj_ui_location_selector", suspendWhenHidden = FALSE)

  # ---- Logique Mise à jour ----
  observeEvent(input$btn_ask_update, {
    showModal(modalDialog(
      title = "Confirmation",
      "Êtes-vous sûr de vouloir mettre à jour les données ? Cela peut prendre quelques minutes.",
      footer = tagList(
        modalButton("Annuler"),
        actionButton("btn_confirm_update", "Confirmer", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$btn_confirm_update, {
    removeModal()
    w_update$show()

    tryCatch(
      {
        # 1. Suppression des fichiers Parquet récents (2024-202x)
        parquet_dir <- "../data/meteo_parquet"
        if (dir.exists(parquet_dir)) {
          files <- list.files(parquet_dir, full.names = TRUE)
          # On cherche les fichiers qui contiennent 2024 dans leur nom
          files_to_delete <- files[grepl("2024", files)]
          if (length(files_to_delete) > 0) {
            unlink(files_to_delete, force = TRUE)
          }
        }

        # 2. Téléchargement des nouvelles données
        ref_geo <- get_referentiel_geo(verbose = FALSE)

        if (!is.null(ref_geo)) {
          download_meteo_multi_parquet(
            departements = ref_geo$CODE_DEPT,
            mode = "full",
            output_dir = parquet_dir,
            parallel = TRUE,
            n_cores = 4,
            verbose = FALSE
          )
        }

        # 3. Suppression des RDS pour forcer le recalcul
        rds_files <- c("meteo_nationale.rds", "meteo_regionale.rds", "meteo_departementale.rds")

        # On essaie plusieurs chemins possibles pour être sûr de trouver les fichiers
        dirs_to_try <- c("../data", "data", "./data")

        for (d in dirs_to_try) {
          paths <- file.path(d, rds_files)
          existing_paths <- paths[file.exists(paths)]
          if (length(existing_paths) > 0) {
            unlink(existing_paths, force = TRUE)
          }
        }

        # Petit nettoyage mémoire
        gc()

        # 4. Rechargement des données
        trigger_reload(trigger_reload() + 1)

        showNotification("Téléchargement terminé. Traitement des données en cours...", type = "message")
      },
      error = function(e) {
        w_update$hide()
        showNotification(paste("Erreur :", e$message), type = "error")
      }
    )
  })
}

# app ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)
