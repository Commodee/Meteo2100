# load libraries ----------------------------------------------------------
library(arrow)
library(httr)
library(leaflet)
library(sf)
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(waiter)


# load externals scripts ----------------------------------------------------
source("data_loader.R")

source("functions/aggregate_meteo.R")
source("functions/climate_data_downloader.R")
source("functions/plot.R")
source("functions/projections_loader.R")



# load data ----------------------------------------------------------------
global_data <- load_raw_data()


# process data ------------------------------------------------------------
vec_dep <- global_data$meteo %>% 
  select(NOM_DEPT, CODE_DEPT) %>% 
  distinct(NOM_DEPT, CODE_DEPT) %>%
  collect() %>%
  arrange(CODE_DEPT) %>%
  pull(NOM_DEPT)

vec_region <- global_data$meteo %>% 
  arrange(NOM_REGION) %>% 
  select(NOM_REGION) %>% 
  distinct(NOM_REGION) %>%
  collect() %>%
  pull(NOM_REGION)

vec_commune <- global_data$meteo %>% 
  arrange(NOM_USUEL) %>% 
  select(NOM_USUEL) %>%
  distinct(NOM_USUEL) %>%
  collect() %>%
  pull(NOM_USUEL)

# ui ----------------------------------------------------------------------
ui <- fluidPage(
  autoWaiter(id="plot1",html = spin_3(), color = "white"),
  autoWaiter(id="carte_interactive",html = spin_3(), color = "white"),
  
  titlePanel("Météo2100"),
  tabsetPanel(
    type = "tab",
    
    # tab_situation
    tabPanel(
      "Où en est on ?",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h1("Sidebar"),
          radioButtons(
            inputId = "situation_plot",
            label = "On affiche quoi ?",
            choices = c("Temperature", "Precipitation"),
            selected = "Temperature"
          ),
          uiOutput("situation_temp_choix"),
          hr(),
          radioButtons(
            inputId = "situation_gran",
            label = "Granularité",
            choices = c("Nationale", "Régionale", "Départementale", "Station Météo"),
            selected = "Nationale"
          ),
          uiOutput("situation_gran_ui"),
          
          hr(),
          
          radioButtons(
            inputId = "situation_tempo",
            label = "Temporalité",
            choices = c("Jour (Attention, le graphique peut mettre du temps a apparaitre)" = "jour", 
                        "Mois" = "mois", 
                        "Année" = "annee"),
            selected = "annee"
          ),
          uiOutput("date_range_ui")
        ), # sidebarPanel
        
        mainPanel(
          width = 9,
          h1("Graphs et indicateurs"),
          textOutput("text"),
          plotOutput("plot1", height = "600px")
        ) # mainPanel
      ) # sidebarLayout
    ), # tab_situation
    
    # tab_carte
    tabPanel(
      "Carte en folie",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h1("Sidebar"),
          radioButtons(
            inputId = "carte_plot",
            label = "On affiche quoi ?",
            choices = c("Temperature", "Precipitation"),
            selected = "Temperature"
          ),
          uiOutput("carte_temp_choix"),
          hr(),
          # granularité
          radioButtons(
            inputId = "carte_ratio",
            label = "Granularité",
            choices = c("Régionale", "Départementale"),
            selected = "Régionale"
          ),
          
          hr(),
          
          radioButtons(
            inputId = "carte_tempo",
            label = "Temporalité",
            choices = c("Jour  (Attention, le graphique peut mettre du temps a apparaitre)" = "jour", 
                        "Mois" = "mois", 
                        "Année" = "annee"),
            selected = "annee"
          ),
          uiOutput("carte_date_choix")
        ), # sidebarPanel
        
        mainPanel(
          width = 9,
          h1("Carte"),
          leafletOutput("carte_interactive", height = "80vh")
        ) # mainPanel
      ) # sidebarLayout
    ), # tab_carte
    
    # tab_demain
    tabPanel(
      "Et demain ?",
      sidebarLayout(
        sidebarPanel(
          h3("Projections 2100"),
          p("Simulez l'avenir selon les différents scénarios du GIEC."),
          
          # Choix géographique 
          selectInput(
            inputId = "demain_region",
            label = "Choisir la région :",
            choices = vec_region, 
            selected = "Île-de-France"
          ),
          
          hr(),
          
          # Choix du Scénario (Le cœur du sujet)
          radioButtons(
            inputId = "scenario_giec",
            label = "Scénario d'émissions (GIEC) :",
            choices = c(
              "Optimiste (RCP 2.6) - Accord de Paris" = "rcp26",
              "Intermédiaire (RCP 4.5) - Politique actuelle" = "rcp45",
              "Pessimiste (RCP 8.5) - 'Business as usual'" = "rcp85"
            ),
            selected = "rcp45"
          ),
          
          # Horizon temporel
          sliderInput(
            inputId = "horizon_annee",
            label = "Jusqu'à quelle année ?",
            min = 2024, 
            max = 2100, 
            value = 2050,
            sep = ""
          )
        ),
        
        mainPanel(
          h2("Trajectoire de température"),
          plotOutput("plot_projection"),
          br(),
          wellPanel(
            h4("Détails du scénario"),
            textOutput("desc_scenario")
          )
        )
      )
    ),
))

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  # ---- Tab Situation ----
  output$situation_gran_ui <- renderUI({
    switch(input$situation_gran,
           "Communale" = selectInput("situation_commune", "Choisir la commune", vec_commune),
           "Départementale" = selectInput("situation_dep", "Choisir le département", vec_dep),
           "Régionale" = selectInput("situation_reg", "Choisir la région", vec_region),
           "Nationale" = NULL
    )
  })
  
  output$situation_temp_choix <- renderUI({
    if (input$situation_plot == "Temperature") {
      radioButtons(
        inputId = "situation_temp_choix",
        label = "Quelle temperature ?",
        choices = c("Temperature max", "Temperature min", "Temperature moy", "Tout"),
        selected = "Temperature moy"
      )
    } else {
      NULL
    }
  })
  
  # Selcteur de date
  output$date_range_ui <- renderUI({
    req(input$situation_tempo)
    
    if (input$situation_tempo == "annee") {
      # CAS 1 : ANNÉE
      # On veut choisir une plage d'années (ex: 1950 - 2023)
      airDatepickerInput(
        inputId = "plage_dates",
        label = "Période (Années) :",
        range = TRUE,
        view = "years",
        minView = "years",
        dateFormat = "yyyy",
        value = c("1950-01-01", "2025-12-31")
      )
      
    } else if (input$situation_tempo == "mois") {
      # CAS 2 : MOIS
      # On sélectionne des mois et années (ex: Jan 2020 - Dec 2022)
      airDatepickerInput(
        inputId = "plage_dates",
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
        inputId = "plage_dates",
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
  output$plot1 <- renderPlot({
    req(input$situation_gran, input$plage_dates)
    
    # 1. Sélection de la source de données
    data_source <- switch(
      input$situation_gran,
      "Nationale" = global_data$meteo_nationale,
      "Régionale" = global_data$meteo_regionale,
      "Départementale" = global_data$meteo_departementale
    )
    
    # 2. Filtrage Géographique
    if (input$situation_gran == "Régionale") {
      req(input$situation_reg)
      data_source <- data_source %>% filter(NOM_REGION == input$situation_reg)
      titre <- input$situation_reg
    } else if (input$situation_gran == "Départementale") {
      req(input$situation_dep)
      data_source <- data_source %>% filter(NOM_DEPT == input$situation_dep)
      titre <- input$situation_dep
    } else {
      titre <- "France Entière"
    }
    
    # 3. Filtrage Date
    date_deb <- as.Date(input$plage_dates[1])
    date_fin <- as.Date(input$plage_dates[2])
    if(input$situation_tempo == "annee") date_fin <- as.Date(paste0(year(date_fin), "-12-31"))
    
    data_filtered <- data_source %>% filter(periode >= date_deb, periode <= date_fin)
    
    shiny::validate(need(nrow(data_filtered) > 0, "Pas de données sur cette période."))
    
    # 4. Ré-agrégation Temporelle (Jour -> Mois ou Année)
    data_ready <- reaggregate_tempo(data_filtered, input$situation_tempo)
    
    # 5. Plot
    if(input$situation_plot == "Temperature"){
      plot_temp(data_ready, titre, input$situation_tempo, input$situation_temp_choix)
    } else {
      plot_prec(data_ready, titre, input$situation_tempo)
    }
  })
  
  
  # ---- Tab Carte ----
  output$carte_temp_choix <- renderUI({
    if (input$carte_plot == "Temperature") {
      radioButtons(
        inputId = "Carte_temp_choix",
        label = "Quelle temperature ?",
        choices = c("Temperature max", "Temperature min", "Temperature moy"),
        selected = "Temperature moy"
      )
    } else {
      NULL
    }
  })
  
  # Selcteur de date
  output$carte_date_choix <- renderUI({
    req(input$carte_tempo)
    
    if (input$carte_tempo == "annee") {
      # CAS 1 : ANNÉE
      airDatepickerInput(
        inputId = "carte_date",
        label = "Quelle année :",
        range = FALSE,
        view = "years",
        minView = "years",
        dateFormat = "yyyy",
        value = c("2025-01-01")
      )
      
    } else if (input$carte_tempo == "mois") {
      airDatepickerInput(
        inputId = "carte_date",
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
        inputId = "carte_date",
        label = "Quel jour :",
        range = FALSE,
        view = "months",
        minView = "days",
        dateFormat = "dd/MM/yyyy",
        value = "2025-12-01" 
      )
    }
  })
  
  output$carte_interactive <- renderLeaflet({
    req(input$carte_ratio, input$carte_date)
    
    # 1. Alignement Date
    date_cible <- as.Date(input$carte_date)
    if (input$carte_tempo == "annee") date_cible <- floor_date(date_cible, "year")
    if (input$carte_tempo == "mois")  date_cible <- floor_date(date_cible, "month")
    
    # 2. Choix Source
    if (input$carte_ratio == "Départementale") {
      map_geo <- global_data$departements
      data_meteo <- global_data$meteo_departementale
      key_col <- "NOM_DEPT"
    } else {
      map_geo <- global_data$regions
      data_meteo <- global_data$meteo_regionale
      key_col <- "NOM_REGION"
    }
    
    # 3. Filtre Temporel
    # On filtre d'abord l'année concernée pour aller vite
    annee_cible <- year(date_cible)
    data_subset <- data_meteo %>% 
      filter(year(periode) == annee_cible) 
    
    # 4. Ré-agrégation & Sélection finale
    # On transforme les jours en Mois/Année, PUIS on garde la date cible
    data_final_meteo <- reaggregate_tempo(data_subset, input$carte_tempo) %>%
      filter(periode == date_cible)
    
    shiny::validate(need(nrow(data_final_meteo) > 0, paste("Pas de données pour", date_cible)))
    
    # 5. Jointure & Carte
    map_final <- map_geo %>% left_join(data_final_meteo, by = key_col)
    if (!inherits(map_final, "sf")) map_final <- st_as_sf(map_final)
    
    pal <- colorNumeric("RdYlBu", domain = map_final$Temperature_moyenne, reverse = TRUE, na.color = "#808080")
    
    leaflet(map_final) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(Temperature_moyenne),
        color = "#2c3e50", weight = 1, opacity = 1, fillOpacity = 0.6,
        label = ~paste0(get(key_col), ": ", round(Temperature_moyenne, 1), "°C"),
        highlightOptions = highlightOptions(weight = 3, color = "#e74c3c", bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = ~Temperature_moyenne, title = "Temp. Moy (°C)", position = "bottomright") %>%
      setView(2.21, 46.22, 6)
  })
  # ---- Tab Demain ----
  
  # 1. Chargement des données DRIAS
  drias_data <- reactive({
    load_drias_projections()
  })
  
  # 2. Description
  output$desc_scenario <- renderText({
    switch(input$scenario_giec,
           "rcp26" = "🟢 Scénario Optimiste (Accord de Paris) : Fortes réductions d'émissions. La température se stabilise vers 2050.",
           "rcp45" = "🟠 Scénario Intermédiaire : Les émissions plafonnent vers 2040. Le réchauffement ralentit mais continue.",
           "rcp85" = "🔴 Scénario Pessimiste : Aucune régulation ('Business as Usual'). Hausse brutale et continue des températures."
    )
  })
  
  # 3. Le Graphique de Projection
  output$plot_projection <- renderPlot({
    req(input$demain_region)
    
    # --- A. Données Historiques ---
    data_hist <- aggregate_meteo(
      data = global_data$meteo,
      granularite_temps = "annee",
      niveau_geo = "Régionale",
      choix_geo = input$demain_region
    ) %>%
      mutate(
        annee = year(periode), # Conversion date -> année pour alignement
        scenario = "Historique"
      )
    
    # --- B. Données Projections(DRIAS) ---
    raw_proj <- drias_data()
    
    shiny::validate(
      need(nrow(raw_proj) > 0, "Les données de projections sont introuvables.")
    )
    
    # --- C. Calcul du décalage (Offset) ---
    # Moyenne Historique (1976-2005) vs Moyenne Modèle (2005)
    ref_hist <- mean(data_hist$Temperature_moyenne[data_hist$annee %in% 1976:2005], na.rm = TRUE)
    # Si pas assez de données historiques, on prend toute la moyenne dispo
    if(is.na(ref_hist)) ref_hist <- mean(data_hist$Temperature_moyenne, na.rm = TRUE)
    
    ref_proj <- mean(raw_proj$Temp_moy[raw_proj$annee == 2005], na.rm = TRUE)
    
    offset <- ref_hist - ref_proj
    
    # --- D. Préparation des données Projections ---
    # On harmonise les noms de colonnes avec ceux de aggregate_meteo
    data_proj_final <- raw_proj %>%
      mutate(
        Temperature_moyenne = Temp_moy + offset,
        Temperature_min     = Temp_min + offset,
        Temperature_max     = Temp_max + offset
      ) %>%
      filter(annee <= input$horizon_annee)
    
    # Séparation : le scénario choisi vs les autres (pour le fond)
    data_proj_selected <- data_proj_final %>% filter(Contexte == input$scenario_giec)
    data_proj_back     <- data_proj_final 
    
    # --- E. Graphique ---
    ggplot() +
      # tous les scénarios en pointillé
      geom_line(data = data_proj_back, 
                aes(x = annee, y = Temperature_moyenne, group = Contexte), 
                color = "grey60", linetype = "dashed", alpha = 0.5) +
      
      # L'historique
      geom_line(data = data_hist, 
                aes(x = annee, y = Temperature_moyenne, color = "Historique"), 
                linewidth = 1) +
      # geom_ribbon(data = data_hist,
      #             aes(x=annee, ymin =Temperature_min, ymax=Temperature_max, color="Historique"),
      #             alpha=0.2)+
      
      # Le Scénario choisi
      geom_ribbon(data = data_proj_selected,
                  aes(x = annee, ymin = Temperature_min, ymax = Temperature_max, fill = Contexte),
                  alpha = 0.2) +
      
      # geom_line(data = data_proj_selected,
      #           aes(x = annee, y = Temperature_moyenne, color = Contexte),
      #           linewidth = 1.5) +
      
      # Esthétique
      scale_color_manual(values = c("Historique" = "#2c3e50", "rcp26" = "#2ecc71", "rcp45" = "#f39c12", "rcp85" = "#e74c3c")) +
      scale_fill_manual(values = c("rcp26" = "#2ecc71", "rcp45" = "#f39c12", "rcp85" = "#e74c3c")) +
      geom_vline(xintercept = 2024, linetype = "dotted") +
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Trajectoire :", input$demain_region),
        subtitle = paste("Ajustement (biais) appliqué :", round(offset, 1), "°C"),
        y = "Température (°C)", x = NULL, fill = "Scénario", color = "Scénario"
      )
  })
    
}

# app ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)