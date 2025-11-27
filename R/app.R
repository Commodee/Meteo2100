# load libraries ----------------------------------------------------------
library(shiny)


# load tab ----------------------------------------------------------------
source("mod_situation.R")
source("mod_carte.R")
source("mod_demain.R")


# ui ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Météo2100"),
  tabsetPanel(type = "tab",
              tabPanel("Où en est on ?", mod_situation_ui("situation_tab")), 
              tabPanel("Carte en folie", mod_carte_ui("carte_tab")),
              tabPanel("Et demain ?", mod_demain_ui("demain_tab"))
  )
)


# server ------------------------------------------------------------------
server <- function(input, output, session){
  mod_situation_server("situation_tab")
  mod_carte_server("carte_tab")
  mod_demain_server("demain_tab")
}


# app ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)