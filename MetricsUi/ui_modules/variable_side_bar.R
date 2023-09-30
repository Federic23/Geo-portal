metrics <- c("Number of users based on IP field", 
             "Visits per day average",
             "Failed requests",
             "404 errors",
             "Average by day of the week",
             "Mean time of the session",
             "Clicks per session"
)

createSidebar <- function() {
  sidebarPanel(
    selectInput("var", "Variable", choices = metrics),  # Use the 'metrics' vector as choices
    numericInput("number", "Ingrese el peso para la metrica (0.1-0.9):", value = 0.1, min = 0.1, max = 1.0, step = 0.1),
    actionButton("addButton", "Añadir peso")
  )
}