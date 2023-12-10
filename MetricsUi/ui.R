library(shiny)
library(shinythemes)
library(ggplot2)
library(shinyjs)
library(shinycssloaders)


source("ui_modules/variable_side_bar.R")



ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = shinytheme("paper"),
  includeCSS("styles.css"),
  
  fluidRow(
    column(2, createSidebar()),
    column(10,
           div(class = "title-panel",
               # titlePanel("Test 2 UI geoportales"),
               img(src = "logo.png", height = "250px", width = "auto")),
           div(class = "metric-group-div",
               h5("Current CSV Data"),
               uiOutput("csvDataDisplay")  # This is where the conditional UI will be displayed
           ),
           div(
             div(class = "metrics-panel", withSpinner(uiOutput("dynamicContent")),),
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