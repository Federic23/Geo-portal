# Define the UI for the main module
mainModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    h3("Numbers Entered:"),
    verbatimTextOutput(ns("numbersList")),
    h3("Sum:"),
    textOutput(ns("sumOutput"))
  )
}

# Define the server logic for the main module
mainModuleServer <- function(input, output, session) {
  # Create reactive values to store the list of numbers and the sum
  numbers <- reactiveValues(numbersList = c(), sum = 0)
  
  # Add the number when the "Add Number" button is clicked
  observeEvent(input$addButton, {
    if (numbers$sum + input$number <= 10) {
      numbers$numbersList <- c(numbers$numbersList, input$number)
      numbers$sum <- numbers$sum + input$number
    }
  })
  
  # Display the list of numbers and the sum
  output$numbersList <- renderPrint({
    numbers$numbersList
  })
  
  output$sumOutput <- renderText({
    paste("Sum: ", numbers$sum)
  })
}
