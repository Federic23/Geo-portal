
source("modules/handle_sum.R")

server <- function(input, output, session) {
  numbers <- reactiveValues(numbersList = c(), numericValues = numeric(0), sum = 0)
  
  observeEvent(input$addButton, {
    selected_var <- input$var
    input_number <- input$number
    
    # Call the function to handle the logic
    handleAddButton(selected_var, input_number, numbers)
  })
  
  output$numbersList <- renderPrint({
    paste(numbers$numbersList, ": ", numbers$numericValues)
  })
  
  output$sumOutput <- renderText({
    paste("Suma de pesos: ", numbers$sum)
  })
}
