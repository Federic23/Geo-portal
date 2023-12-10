metrics <- c("Number of users based on IP field", 
             "Visits per day average",
             "Failed requests",
             "404 errors",
             "Average by day of the week",
             "Mean time of the session",
             "Clicks per session"
)

metrics2 <- c("Metric 1",
             "Metric 2", 
             "Metric 3",
             "Metric 4", 
             "Metric 5", 
             "Metric 6"
)


createSidebar <- function() {
  div(
    class = "sidebar",
    fileInput("file1", "Choose CSV File",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    div(actionButton(class = "button", "CSVHistoryButton", "Calculate")),
    div(actionButton(class = "button", "progressionButton", "Progression")),
    div(actionButton(class = "button", "progressionButton", "CSV converter")),
    div(actionButton(class = "button", "CSVHistoryButton", "CSV History")),
  )
}



# createSidebar <- function() {
#   div(
#     class = "sidebar",
#     selectInput("var", "Variable", choices = metrics),  # Use the 'metrics' vector as choices
#     numericInput("number", "Ingrese el peso para la metrica (0.1-0.9):", value = 0.1, min = 0.1, max = 1.0, step = 0.1),
#     actionButton("addButton", "Añadir peso")
#   )
# }