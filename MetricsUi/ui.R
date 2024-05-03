if (!require(shiny)) {
  install.packages("shiny")
}

if (!require(shinythemes)) {
  install.packages("shinythemes")
}

if (!require(ggplot2)) {
  install.packages("ggplot2")
}

if (!require(shinyjs)) {
  install.packages("shinyjs")
}

if (!require(shinycssloaders)) {
  install.packages("shinycssloaders")
}

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
               img(src = "logo.png", height = "250px", width = "auto")),
           div(class = "metric-group-div",
               h5("Current CSV Data"),
               uiOutput("csvDataDisplay")
           ),
           div(
             div(class = "metrics-panel", withSpinner(uiOutput("dynamicContent")),),
           )
    )
  )
)