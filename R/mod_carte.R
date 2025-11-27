mod_carte_ui <- function(id) {
  ns <- NS(id)

  tagList(
    sidebarLayout(
      sidebarPanel(
        h1("Sidebar")
        # Selection precipitations, vent, temp (max/min/moy) etc...
        # granularité
      ), # sidebarPanel
      mainPanel(
        h1("Carte")
      ) # mainPanel
    ) # sidebarLayout
  )
}

mod_carte_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}