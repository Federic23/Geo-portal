# ui.R
library(shiny)
library(shinythemes)

source("ui_modules/variable_side_bar.R")

ui <- fluidPage(
  theme = shinytheme("darkly"),
  tags$head(
    tags$style(HTML(".title-panel {text-align: center;}"))
  ),
  div(class = "title-panel",
      titlePanel("Test 2 UI geoportales")),
  
  sidebarLayout(
    createSidebar(),
    
    mainPanel(
      h3("Pesos ingresados:"),
      verbatimTextOutput("numbersList"),
      h3("Suma de pesos:"),
      uiOutput("sumOutput")
    )
  ),
  
)
