# Load the Shiny library
library(shiny)

# Define the server logic
source("modules/main_module.R")

server <- function(input, output, session) {
  mainModuleServer(input, output, session)
}
