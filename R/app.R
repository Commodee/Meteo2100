# load libraries ----------------------------------------------------------
library(arrow)
library(bslib)
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
    useWaiter()
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
                "Ville précise" = "Station Météo"
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

      card(
        full_screen = TRUE,
        card_header("Visualisation des données historiques"),
        textOutput("sit_text_info"),
        plotOutput("sit_plot_history", height = "500px") %>% withSpinner(color = "#3498db", type = 6)
      ) # card
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
            radioButtons(
              inputId = "map_input_var_type",
              label = "On affiche quoi ?",
              choices = c("Temperature", "Precipitation"),
              selected = "Temperature"
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
                "Jour  (Attention, le graphique peut mettre du temps a apparaitre)" = "jour",
                "Mois" = "mois",
                "Année" = "annee"
              ),
              selected = "annee"
            ),
            uiOutput("map_ui_date_selector")
          )
        ) # accordion
      ),
      # sidebar

      card(
        full_screen = TRUE,
        card_header("Exploration Cartographique"),
        card_body(padding = 0, leafletOutput("map_output_leaflet", height = "500px") %>% withSpinner(color = "#3498db", type = 6))
      ) # card
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

      card(
        card_header("Trajectoire de température"),
        plotOutput("proj_plot_trajectory", height = "500px") %>% withSpinner(color = "#3498db", type = 6),
        wellPanel(h4("Détails du scénario"), textOutput("proj_text_scenario_desc"))
      ) # card
    ) # layout_sidebar
  ),
  # tab_demain

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

  # Chargement des données
  global_data_reactive <- eventReactive(TRUE,
    {
      result <- load_raw_data()
      result
    },
    ignoreNULL = FALSE
  )

  # Préparation des vecteurs de choix
  vec_dep <- reactive({
    global_data_reactive()$meteo %>%
      select(NOM_DEPT, CODE_DEPT) %>%
      distinct(NOM_DEPT, CODE_DEPT) %>%
      collect() %>%
      arrange(CODE_DEPT) %>%
      pull(NOM_DEPT)
  })

  vec_region <- reactive({
    global_data_reactive()$meteo %>%
      arrange(NOM_REGION) %>%
      select(NOM_REGION) %>%
      distinct(NOM_REGION) %>%
      collect() %>%
      pull(NOM_REGION)
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
  })

  # ---- Tab Situation ----
  output$sit_ui_location_selector <- renderUI({
    switch(input$sit_input_scale,
      "Station Météo" = selectInput("sit_input_loc_commune", "Choisir la commune", vec_commune()),
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
      radioButtons(
        inputId = "map_input_temp_metric",
        label = "Quelle temperature ?",
        choices = c("Temperature max", "Temperature min", "Temperature moy"),
        selected = "Temperature moy"
      )
    } else {
      NULL
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
        value = "2025-12-01"
      )
    }
  })

  output$map_output_leaflet <- renderLeaflet({
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

    shiny::validate(need(
      nrow(data_final_meteo) > 0,
      paste("Pas de données pour", date_cible)
    ))

    # 5. Jointure
    map_final <- map_geo %>% left_join(data_final_meteo, by = key_col)
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
      col_name_region = key_col # "NOM_DEPT" ou "NOM_REGION"
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

  # Description Scénario
  output$proj_text_scenario_desc <- renderText({
    switch(input$proj_input_scenario,
      "rcp26" = "🟢 Scénario Optimiste (Accord de Paris) : Fortes réductions d'émissions. La température se stabilise vers 2050.",
      "rcp45" = "🟠 Scénario Intermédiaire : Les émissions plafonnent vers 2040. Le réchauffement ralentit mais continue.",
      "rcp85" = "🔴 Scénario Pessimiste : Aucune régulation ('Business as Usual'). Hausse brutale et continue des températures."
    )
  })

  # Graphique Projection
  output$proj_plot_trajectory <- renderPlot({
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

    # 4. Ajustement des Projections
    data_proj_final <- raw_proj %>%
      mutate(
        Temperature_moyenne = Temp_moy + offset,
        Temperature_min     = Temp_min + offset,
        Temperature_max     = Temp_max + offset
      )

    # 5. Appel de la fonction de plot
    plot_projection_graph(
      data_hist       = data_hist,
      data_proj       = data_proj_final,
      scenario_choisi = input$proj_input_scenario,
      titre           = titre,
      offset_val      = offset
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
}

# app ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)
