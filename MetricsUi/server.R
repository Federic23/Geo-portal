
source("modules/handle_sum.R")

source("modules/handle_sum.R")


library(ggplot2)

#############################################################################################
group1_metrics <- list(
  list(name = "Pageviews", id = "trafficMetricA"),
  list(name = "Unique Visitors", id = "trafficMetricB"),
  list(name = "Sessions", id = "trafficMetricC"),
  list(name = "Bounce Rate", id = "trafficMetricD")
)

group1 <- list(groupName = "Traffic Metrics", metrics = group1_metrics)

group2_metrics <- list(
  list(name = "Ip únicas", id = "visitorMetricA"),
  list(name = "Average Amount of pages visited per user", id = "visitorMetricB"),
  list(name = "Average Amount of pages visited per session", id = "visitorMetricC"),
  list(name = "Recurring visitors: amount of visitors that came back", id = "visitorMetricD"),
  list(name = "Individual Resource Loading Times (average)", id = "visitorMetricE"),
  list(name = "visits per day average", id = "visitorMetricF")
)

group2 <- list(groupName = "Visitor Statistics", metrics = group2_metrics)

group3_metrics <- list(
  list(name = "failed requests (amount)", id = "errorMetricA"),
  list(name = "failed requests (percentage)", id = "errorMetricB"),
  list(name = "amount of 404 errors", id = "errorMetricC"),
  list(name = "other errors", id = "errorMetricD")
)

group3 <- list(groupName = "Error Metrics", metrics = group3_metrics)

metricsGroups <- list(group1, group2, group3)


results <- list(
  list(value = 5.8, date = "15/10/2023"),
  list(value = 5.9, date = "16/10/2023"),
  list(value = 5.6, date = "17/10/2023"),
  list(value = 5.6, date = "18/10/2023"),
  list(value = 5.9, date = "19/10/2023"),
  list(value = 6.8, date = "20/10/2023"),
  list(value = 4.9, date = "21/10/2023"),
  list(value = 5.5, date = "22/10/2023")
)

df <- data.frame(
  value = sapply(results, function(x) x$value),
  date = as.Date(sapply(results, function(x) as.Date(x$date, format = "%d/%m/%Y"))
  )
)

####################################################################################################  
  
server <- function(input, output, session) {
  #######################################################
  content1_visible <- reactiveVal(TRUE)
  content2_visible <- reactiveVal(FALSE)
  
  observeEvent(input$CsvFormatButton, {
    content1_visible(TRUE)
    content2_visible(FALSE)
  })
  
  observeEvent(input$progressionButton, {
    content1_visible(FALSE)
    content2_visible(TRUE)
  })
  
  #####################################################
  
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
  
  # Define your metric groups and metrics
  metricgroups <- list(
    group1 = list(
      groupName = "Traffic Metrics",
      metrics = list(
        list(name = "Metric 1", id = "trafficMetric1"),
        list(name = "Metric 2", id = "trafficMetric2")
      )
    ),
    group2 = list(
      groupName = "Visitor Statistics",
      metrics = list(
        list(name = "Metric 1", id = "visitorMetric1"),
        list(name = "Metric 2", id = "visitorMetric2")
      )
    ),
    group3 = list(
      groupName = "Error Metrics",
      metrics = list(
        list(name = "Metric 1", id = "errorMetric1"),
        list(name = "Metric 2", id = "errorMetric2")
      )
    )
  )
  
  # Pass metric groups to the UI
  output$metricGroups <- renderPrint({
    metricgroups
  })
  
  observe({
    if (!is.null(input$file1)) {
      file_path <- input$file1$datapath
      cat("Uploaded File Path: ", file_path, "\n")
    }
  })
  
  filtered_data <- reactive({
    start_date <- input$startDate
    end_date <- input$endDate
    subset(df, date >= start_date & date <= end_date)
  })
  
  dynamicContent <- reactive({
    if (content1_visible()) {
      # Initial content
      fluidRow(
        lapply(metricsGroups, function(group) {
          div(class = "metric-group-div",
              h4(group$groupName),
              lapply(split(group$metrics, (seq_along(group$metrics) - 1) %/% 2), function(pair) {
                fluidRow(
                  lapply(pair, function(metric) {
                    column(6, metric$name, random_number <- round(runif(1), 2))
                  })
                )
              })
          )
        })
      )
    } else if (content2_visible()){
      div(renderPlot({
        p <- ggplot(filtered_data(), aes(x = date, y = value)) +
          geom_line() +
          labs(
            title = "Line Plot of Group 1 Metrics",
            x = "Date",
            y = "Value"
          ) +
          theme_minimal()
        
        p
      }),
      div(dateInput("startDate", "Select a start date:", value = "2023-10-15")),
      dateInput("endDate", "Select an end date:", value = "2023-10-22"))
      
    }
  })
  
  output$dynamicContent <- renderUI({
    dynamicContent()
  })
}



# server <- function(input, output, session) {
#   numbers <- reactiveValues(numbersList = c(), numericValues = numeric(0), sum = 0)
#   
#   observeEvent(input$addButton, {
#     selected_var <- input$var
#     input_number <- input$number
#     
#     # Call the function to handle the logic
#     handleAddButton(selected_var, input_number, numbers)
#   })
#   
#   output$numbersList <- renderPrint({
#     paste(numbers$numbersList, ": ", numbers$numericValues)
#   })
#   
#   output$sumOutput <- renderText({
#     paste("Suma de pesos: ", numbers$sum)
#   })
#   
#   # Define your metric groups and metrics
#   metric_groups <- list(
#     group1 = c("Metric A", "Metric B", "Metric C"),
#     group2 = c("Metric D", "Metric E"),
#     group3 = c("Metric F")
#   )
#   
#   # Define your metric groups and metrics
#   metricgroups <- list(
#     group1 = list(
#       groupName = "Traffic Metrics",
#       metrics = list(
#         list(name = "Metric 1", id = "trafficMetric1"),
#         list(name = "Metric 2", id = "trafficMetric2")
#       )
#     ),
#     group2 = list(
#       groupName = "Visitor Statistics",
#       metrics = list(
#         list(name = "Metric 1", id = "visitorMetric1"),
#         list(name = "Metric 2", id = "visitorMetric2")
#       )
#     ),
#     group3 = list(
#       groupName = "Error Metrics",
#       metrics = list(
#         list(name = "Metric 1", id = "errorMetric1"),
#         list(name = "Metric 2", id = "errorMetric2")
#       )
#     )
#   )
#   
#   # Pass metric_groups to the UI
#   output$metricGroups <- renderPrint({
#     metricgroups
#   })
#   
#   # Generate input fields for metric groups
#   output$metricGroupInputs <- renderUI({
#     metric_group_inputs <- lapply(names(metric_groups), function(group_name) {
#       metric_group <- metric_groups[[group_name]]
#       group_inputs <- lapply(metric_group, function(metric_name) {
#         fluidRow(
#           column(4, metric_name),
#           column(8, numericInput(
#             inputId = paste0(group_name, "_", metric_name, "_weight"),
#             label = NULL,
#             min = 0.0, 
#             max = 1.0,
#             step = 0.1,
#             value = 0,
#             width = "100%"
#           ))
#         )
#       })
#       tagList(
#         h3(group_name),
#         group_inputs
#       )
#     })
#     do.call(tagList, metric_group_inputs)
#   })
#   
#   observe({
#     if (!is.null(input$file1)) {
#       file_path <- input$file1$datapath
#       cat("Uploaded File Path: ", file_path, "\n")
#     }
#   })
# }
