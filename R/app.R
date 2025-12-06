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
  
  # 3. Le Graphique de Projection
  output$plot_projection <- renderPlot({
    req(input$demain_region)
    
    # --- A. Données Historiques (Météo-France) ---
    data_hist <- global_data$meteo %>%
      filter(NOM_REGION == input$demain_region) %>%
      mutate(annee = year(DATE)) %>%
      group_by(annee) %>%
      summarise(temp_moy = mean(TM, na.rm = TRUE), .groups = "drop") %>%
      collect() %>%
      mutate(scenario = "Historique")
    
    # --- B. Données de Projection (DRIAS) ---
    data_proj_all <- drias_data() %>%
      filter(annee <= input$horizon_annee)
    
    # --- C. Construction du Graphique ---
    p <- ggplot() +
      
      # 1. Courbe Historique (Trait Noir épais)
      geom_line(data = data_hist, aes(x = annee, y = temp_moy, color = "Historique"), size = 1.2) +
      
      # 2. Toutes les Projections en fond (Pointillés discrets)
      # Cela permet de comparer le scénario choisi aux autres
      geom_line(data = data_proj_all, 
                aes(x = annee, y = temp_moy, color = scenario, group = scenario), 
                linetype = "dashed", size = 0.8, alpha = 0.5)
    
    # 3. Mise en valeur du Scénario sélectionné (Ruban + Trait plein)
    data_selected <- data_proj_all %>% filter(scenario == input$scenario_giec)
    
    if (nrow(data_selected) > 0) {
      p <- p +
        # Ruban d'incertitude (Zone colorée)
        geom_ribbon(data = data_selected,
                    aes(x = annee, ymin = temp_min, ymax = temp_max, fill = scenario),
                    alpha = 0.3) +
        # La courbe principale du scénario choisi (plus épaisse)
        geom_line(data = data_selected,
                  aes(x = annee, y = temp_moy, color = scenario),
                  size = 1.5)
    }
    
    # --- D. Design et Couleurs ---
    p +
      # Définition manuelle des couleurs pour respecter les codes du GIEC
      scale_color_manual(values = c(
        "Historique" = "#2c3e50", # Gris foncé
        "rcp26"      = "#2ecc71", # Vert
        "rcp45"      = "#f39c12", # Orange
        "rcp85"      = "#e74c3c"  # Rouge
      )) +
      scale_fill_manual(values = c(
        "rcp26" = "#2ecc71",
        "rcp45" = "#f39c12",
        "rcp85" = "#e74c3c"
      )) +
      # Ligne verticale "Aujourd'hui"
      geom_vline(xintercept = 2024, linetype = "dotted", color = "gray50") +
      annotate("text", x = 2024, y = min(data_hist$temp_moy, na.rm=TRUE), 
               label = "Aujourd'hui", vjust = -0.5, angle = 90, size = 3, color = "gray50") +
      
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Trajectoire climatique :", input$demain_region),
        subtitle = "Confrontation Historique vs Projections du GIEC",
        y = "Température Moyenne (°C)",
        x = NULL,
        color = "Scénarios",
        fill = "Incertitude"
      ) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", color = "#2c3e50")
      )
  })
    
}

# app ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)