# Load the Shiny library
library(shiny)

# Define the user interface
source("modules/main_module.R")

ui <- fluidPage(
  # Application title with CSS styling
  tags$head(
    tags$style(HTML(".title-panel {text-align: center;}"))
  ),
  div(class = "title-panel",
      titlePanel("Goportal - prueba pesos")),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    # Sidebar panel
    sidebarPanel(
      numericInput("number", "Enter a number (1-9):", value = 0.1, min = 0.1, max = 0.9, step = 1),
      actionButton("addButton", "Add Number")
    ),
    
    # Main panel
    mainModuleUI("main_module")
  )
)
