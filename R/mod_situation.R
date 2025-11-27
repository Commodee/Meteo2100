mod_situation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        h1("Sidebar")
      ), # sidebarPanel
      mainPanel(
        h1("Graphs et indicateurs")
      ) # mainPanel
    ) # sidebarLayout
  )
}

mod_situation_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}