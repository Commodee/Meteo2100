# load libraries ----------------------------------------------------------
library(shiny)
library(waiter)
library(sf)
library(tidyverse)
source("data_loader.R")

# load data ----------------------------------------------------------------
if (file.exists("../data/global.RDS")) {
  global_data <- readRDS("../data/global.RDS")
} else{
  global_data <- load_raw_data()
  saveRDS(global_data, "../data/global.RDS")
}

# print(names(global_data$meteo))

vec_dep <- unique(global_data$meteo$NOM_DEPT)
vec_region <- unique(global_data$meteo$NOM_REGION)
vec_commune <- unique(global_data$meteo$NOM_USUEL)

# ui ----------------------------------------------------------------------
ui <- fluidPage(
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
            choices = c("Nationale", "Régionale", "Départementale", "Communale"),
            selected = "Nationale"
          ),
          uiOutput("situation_gran_ui")  # Changé le nom pour éviter confusion
        ), # sidebarPanel
        
        mainPanel(
          h1("Graphs et indicateurs"),
          textOutput("text")  # Ajouté pour afficher le texte
        ) # mainPanel
      ) # sidebarLayout
    ), # tab_situation
    
    # tab_carte
    tabPanel(
      "Carte en folie",
      sidebarLayout(
        sidebarPanel(
          h1("Sidebar"),
          # Selection precipitations, vent, temp (max/min/moy) etc...
          # granularité
          radioButtons(
            inputId = "carte_ratio",
            label = "Granularité",
            choices = c("Nationale", "Régionale", "Départementale", "Communale"),
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
      "Et demain ?"
    ) # tab_demain
  ) # tabsetPanel
) # fluidPage

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  # Tab Situation
  output$situation_gran_ui <- renderUI({  # Nom cohérent avec l'UI
    switch(input$situation_gran,
           "Communale" = selectInput("situation_commune", "Choisir la commune", vec_commune),
           "Départementale" = selectInput("situation_dep", "Choisir le département", vec_dep),
           "Régionale" = selectInput("situation_reg", "Choisir la région", vec_region),  # Corrigé : vec_region
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
  
  # Tab Carte
  output$carte_gran_ui <- renderUI({
    switch(input$carte_ratio,
           "Communale" = selectInput("carte_commune", "Choisir la commune", vec_commune),
           "Départementale" = selectInput("carte_dep", "Choisir le département", vec_dep),
           "Régionale" = selectInput("carte_reg", "Choisir la région", vec_region),
           "Nationale" = NULL
    )
  })
  
  # Tab Demain
}

# app ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)