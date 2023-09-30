# MainModule.R
library(shiny)

# Load the UI from ui.R
source("ui.R", local = TRUE)

# Define the main module
MainModule <- function() {
  ui <- fluidPage(
    tags$head(
      tags$style(HTML(".title-panel {text-align: center;}"))
    ),
    div(class = "title-panel",
        titlePanel("Test 2 UI geoportales")),
    
    sidebarLayout(
      createSidebar(),  # Use the same sidebar function
      mainPanel(
        h3("Pesos ingresados:"),
        verbatimTextOutput("numbersList"),
        h3("Suma de pesos:"),
        textOutput("sumOutput")
      )
    )
  )

  server <- function(input, output, session) {
    # ... server logic ...
  }

  shinyApp(ui = ui, server = server)
}

# Run the main module
MainModule()
