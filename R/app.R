# load libraries ----------------------------------------------------------
library(arrow)
library(httr)
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
            choices = c("Jour" = "jour", 
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
          h1("Sidebar"),
          # granularité
          radioButtons(
            inputId = "carte_ratio",
            label = "Granularité",
            choices = c("Nationale", "Régionale", "Départementale", "Station Météo"),
            selected = "Nationale"
          ),
          uiOutput("carte_gran_ui")  # Corrigé : uiOutput au lieu de output
        ), # sidebarPanel
        
        mainPanel(
          h1("Carte")
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
    
    # --- 1. SÉCURITÉ & VALIDATION ---
    # C'est ici qu'on limite la plage "Max"
    
    # Calcul de la durée en jours
    duree_jours <- as.numeric(difftime(input$plage_dates[2], input$plage_dates[1], units = "days"))
    duree_annees <- duree_jours / 365
    
    if (input$situation_tempo == "mois") {
      shiny::validate(
        need(duree_annees <= 5, "⚠️ La période est trop longue pour un affichage mensuel. Veuillez sélectionner moins de 5 ans.")
      )
    } else if (input$situation_tempo == "jour") {
      shiny::validate(
        need(duree_jours <= 180, "⚠️ La période est trop longue pour un affichage mensuel. Veuillez sélectionner moins de 6 mois.")
      )
    }
    
    
    # --- 2. FILTRAGE DES DONNÉES ---
    # On filtre les données Arrow AVANT de les envoyer au plot
    # Cela rend l'appli beaucoup plus rapide
    date_debut <- input$plage_dates[1]
    date_fin   <- input$plage_dates[2]
    
    if (input$situation_tempo == "annee") {
      date_fin <- as.Date(paste0(year(date_fin), "-12-31"))
    }
    
    data_filtree <- global_data$meteo %>%
      filter(
        DATE >= date_debut,
        DATE <= date_fin
      )
    
    # --- 3. GÉNÉRATION DU GRAPHIQUE ---
    if(input$situation_plot == "Temperature"){
      switch(input$situation_gran,
             "Communale"       = plot_temp(data_filtree, "Communale", input$situation_commune, input$situation_tempo, input$situation_temp_choix),
             "Départementale"  = plot_temp(data_filtree, "Départementale", input$situation_dep, input$situation_tempo, input$situation_temp_choix),
             "Régionale"       = plot_temp(data_filtree, "Régionale", input$situation_reg, input$situation_tempo, input$situation_temp_choix),
             "Nationale"       = plot_temp(data_filtree, "Nationale", NA, input$situation_tempo, input$situation_temp_choix)
      )
    }else if(input$situation_plot == "Precipitation"){
      switch(input$situation_gran,
             "Communale"       = plot_prec(data_filtree, "Communale", input$situation_commune, input$situation_tempo),
             "Départementale"  = plot_prec(data_filtree, "Départementale", input$situation_dep, input$situation_tempo),
             "Régionale"       = plot_prec(data_filtree, "Régionale", input$situation_reg, input$situation_tempo),
             "Nationale"       = plot_prec(data_filtree, "Nationale", NA, input$situation_tempo)
      )
    }
  })
  
  
  # ---- Tab Carte ----
  output$carte_gran_ui <- renderUI({
    switch(input$carte_ratio,
           "Communale" = selectInput("carte_commune", "Choisir la commune", vec_commune),
           "Départementale" = selectInput("carte_dep", "Choisir le département", vec_dep),
           "Régionale" = selectInput("carte_reg", "Choisir la région", vec_region),
           "Nationale" = NULL
    )
  })
  
  # ---- Tab Demain ----
  # 1. Chargement des données DRIAS (s'exécute une seule fois au lancement)
  drias_data <- reactive({
    load_drias_projections()
  })
  
  # 2. Description dynamique du scénario sélectionné
  output$desc_scenario <- renderText({
    switch(input$scenario_giec,
           "rcp26" = "🟢 Scénario Optimiste (Accord de Paris) : Fortes réductions d'émissions. La température se stabilise vers 2050.",
           "rcp45" = "🟠 Scénario Intermédiaire : Les émissions plafonnent vers 2040. Le réchauffement ralentit mais continue.",
           "rcp85" = "🔴 Scénario Pessimiste : Aucune régulation ('Business as Usual'). Hausse brutale et continue des températures."
    )
  })
  
  # 3. Le Graphique de Projection (AVEC DÉCALAGE RÉGIONAL)
  output$plot_projection <- renderPlot({
    req(input$demain_region)
    
    # --- A. Données Historiques (Réalité Locale) ---
    # On récupère toute l'histoire disponible pour cette région
    data_hist <- global_data$meteo %>%
      filter(NOM_REGION == input$demain_region) %>%
      mutate(annee = year(DATE)) %>%
      group_by(annee) %>%
      summarise(temp_moy = mean(TM, na.rm = TRUE), .groups = "drop") %>%
      collect() %>%
      mutate(scenario = "Historique")
    
    # --- B. Données DRIAS (Modèle National) ---
    raw_proj <- drias_data()
    validate(need(!is.null(raw_proj), "Chargement DRIAS échoué."))
    
    # --- C. CALCUL DU DÉCALAGE (Le Delta) ---
    # 1. On cherche la température moyenne de la région dans le passé (référence)
    # On essaie de viser la période 1976-2005 si on a les données, sinon on prend tout ce qu'on a
    ref_region <- data_hist %>%
      filter(annee >= 1976, annee <= 2005) %>%
      summarise(m = mean(temp_moy, na.rm = TRUE)) %>%
      pull(m)
    
    # Si on n'a pas assez de vieux historique (ex: mode Light), on prend la moyenne globale dispo
    if (is.na(ref_region) || is.nan(ref_region)) {
      ref_region <- mean(data_hist$temp_moy, na.rm = TRUE)
    }
    
    # 2. On cherche la référence dans le fichier DRIAS (c'est le point 1990)
    ref_drias <- raw_proj %>%
      filter(annee == 1990) %>%
      summarise(m = mean(temp_moy, na.rm = TRUE)) %>%
      pull(m)
    
    # 3. Le Delta : De combien faut-il monter/descendre la courbe ?
    # Si Region (15°C) - Drias (12°C) = +3°C d'ajustement
    offset <- ref_region - ref_drias
    
    # Sécurité si calcul impossible
    if (is.na(offset)) offset <- 0
    
    # --- D. APPLICATION DU DÉCALAGE ---
    data_proj_shifted <- raw_proj %>%
      mutate(
        temp_moy = temp_moy + offset,
        temp_min = temp_min + offset,
        temp_max = temp_max + offset
      )
    
    # On sépare pour l'affichage
    data_proj_fond <- data_proj_shifted # Pour les pointillés
    
    data_selected <- data_proj_shifted %>% 
      filter(scenario == input$scenario_giec, annee <= input$horizon_annee)
    
    # --- E. GRAPHIQUE ---
    p <- ggplot() +
      # Historique
      geom_line(data = data_hist, aes(x = annee, y = temp_moy, color = "Historique"), size = 1, alpha = 0.8) +
      
      # Lignes de fond (Pointillés ajustés à la région)
      geom_line(data = data_proj_fond, aes(x = annee, y = temp_moy, color = scenario, group = scenario), 
                linetype = "dashed", alpha = 0.4) +
      geom_point(data = data_proj_fond, aes(x = annee, y = temp_moy, color = scenario), size = 2, alpha = 0.4)
    
    if (nrow(data_selected) > 0) {
      if (nrow(data_selected) > 1) {
        p <- p +
          geom_ribbon(data = data_selected, aes(x = annee, ymin = temp_min, ymax = temp_max, fill = scenario), alpha = 0.2) +
          geom_line(data = data_selected, aes(x = annee, y = temp_moy, color = scenario), size = 1.5)
      }
      p <- p + geom_point(data = data_selected, aes(x = annee, y = temp_moy, color = scenario), size = 4)
      
      # Petit texte pour afficher la température en 2085
      val_fin <- tail(data_selected, 1)
      p <- p + geom_label(data = val_fin, aes(x = annee, y = temp_max, label = paste0("+", round(val_fin$temp_moy - ref_region, 1), "°C")), 
                          vjust = -0.5, size = 3, fontface = "bold", show.legend = FALSE)
    }
    
    p +
      scale_color_manual(values = c("Historique" = "#2c3e50", "rcp26" = "#2ecc71", "rcp45" = "#f39c12", "rcp85" = "#e74c3c")) +
      scale_fill_manual(values = c("rcp26" = "#2ecc71", "rcp45" = "#f39c12", "rcp85" = "#e74c3c")) +
      geom_vline(xintercept = 2024, linetype = "dotted", color = "gray50") +
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Trajectoire pour :", input$demain_region),
        subtitle = paste("Ajustement local de", round(offset, 1), "°C par rapport au modèle national"),
        y = "Température Moyenne (°C)", x = NULL
      )
  })
    
}

# app ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)