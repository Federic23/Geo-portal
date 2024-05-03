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
    div(actionButton(class = "button", "select_file", "Select File")),
    div(actionButton(class = "button", "CSVHistoryButton", "Calculate")),
    div(actionButton(class = "button", "progressionButton", "Progression")),
  )
}
