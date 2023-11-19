library(shiny)
library(shinythemes)
library(ggplot2)

source("ui_modules/variable_side_bar.R")



ui <- fluidPage(
  theme = shinytheme("paper"),
  includeCSS("styles.css"),
  
  fluidRow(
    column(2, createSidebar()),
    column(10,
           div(class = "title-panel",
               # titlePanel("Test 2 UI geoportales"),
               img(src = "logo.png", height = "250px", width = "auto")),
           
           div(
             div(class = "metrics-panel", uiOutput("dynamicContent"),),
           )
    )
  )
)

# Define metricsGroups as a list of groups and their metrics


# 
# ui <- fluidPage(
#   theme = shinytheme("darkly"),
#   includeCSS("styles.css"),
#   
#   fluidRow(
#     column(3, createSidebar()),
#     column(9,
#            div(class = "title-panel",
#                titlePanel("Test 2 UI geoportales")),
#            mainPanel(
#              h3("Pesos ingresados:"),
#              verbatimTextOutput("numbersList"),
#              h3("Weight sum:"),
#              uiOutput("sumOutput")
#            )
#     )
#   ),
#   #uiOutput("metricGroupInputs")
# )