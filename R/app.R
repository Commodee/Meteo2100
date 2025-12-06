# load libraries ----------------------------------------------------------
library(arrow)
library(httr)
library(jsonlite)
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



# load data ----------------------------------------------------------------
global_data <- load_raw_data()


# process data ------------------------------------------------------------
vec_dep <- global_data$meteo %>% 
  distinct(NOM_DEPT) %>% 
  collect() %>%
  pull(NOM_DEPT)
# Pour les régions
vec_region <- global_data$meteo %>% 
  distinct(NOM_REGION) %>%
  collect() %>%
  pull(NOM_REGION)

vec_commune <- global_data$meteo %>%
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
          h1("Sidebar"),
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
            selected = "mois"
          ),
          uiOutput("date_range_ui")
        ), # sidebarPanel
        
        mainPanel(
          h1("Graphs et indicateurs"),
          textOutput("text"),
          plotOutput("plot1")
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
  
  # Texte qui affiche la sélection
  output$text <- renderText({
    req(input$situation_gran)
    switch(input$situation_gran,
           "Communale" = input$situation_commune,
           "Départementale" = input$situation_dep,
           "Régionale" = input$situation_reg,
           "Nationale" = "France entière"
    )
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
        value = c("1990-01-01", "2023-12-31")
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
    
    data_filtree <- global_data$meteo %>%
      filter(
        DATE >= input$plage_dates[1],
        DATE <= input$plage_dates[2]
      )
    
    # --- 3. GÉNÉRATION DU GRAPHIQUE ---
    switch(input$situation_gran,
           "Communale"       = plotplot(data_filtree, "Communale", input$situation_commune, input$situation_tempo),
           "Départementale"  = plotplot(data_filtree, "Départementale", input$situation_dep, input$situation_tempo),
           "Régionale"       = plotplot(data_filtree, "Régionale", input$situation_reg, input$situation_tempo),
           "Nationale"       = plotplot(data_filtree, "Nationale", NA, input$situation_tempo)
    )
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
    
}

# app ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)