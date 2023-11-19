
source("modules/metric_groups.R")
library(ggplot2)

server <- function(input, output, session) {
  source("modules/route_page_control.R", local = TRUE)
  
  #####################################################
  
  ########CSV CHOOSER CONTROLLER
  observe({
    if (!is.null(input$file1)) {
      file_path <- input$file1$datapath
      cat("Uploaded File Path: ", file_path, "\n")
    }
  })
  
  ########CSV CHOOSER CONTROLLER
  
  filtered_data <- reactive({
    start_date <- input$startDate
    end_date <- input$endDate
    subset(df, date >= start_date & date <= end_date)
  })
  
  ########DYNAMIC CONTROL
  dynamicContent <- reactive({
    if (metric_content_visible()) {
      fluidRow(
        lapply(metricsGroups, function(group) {
          div(class = "metric-group-div",
              h4(group$groupName),
              lapply(split(group$metrics, (seq_along(group$metrics) - 1) %/% 2), function(pair) {
                fluidRow(
                  lapply(pair, function(metric) {
                    column(class = "metric-group-divs",6, h6( metric$name, random_number <- round(runif(1), 2)))
                  })
                )
              })
          )
        })
      )
      
      # fluidRow(
      #   lapply(metricsGroups, function(group) {
      #     div(class = "metric-group-div",
      #         h4(group$groupName),
      #         lapply(split(group$metrics, (seq_along(group$metrics) - 1) %/% 2), function(pair) {
      #           fluidRow(
      #             lapply(pair, function(metric) {
      #               column(class = "metric-group-divs", 6,
      #                      h6(metric$name),
      #                      numericInput(inputId = paste("input", metric$name, sep="_"), 
      #                                   label = NULL, 
      #                                   value = 0, 
      #                                   min = 0, 
      #                                   max = 1, 
      #                                   step = 0.01))
      #             })
      #           )
      #         })
      #     )
      #   })
      # )
    } else if (overall_metric_content_visible()){
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


# Initial content
# fluidRow(
#   lapply(metricsGroups, function(group) {
#     div(class = "metric-group-div",
#         h4(group$groupName),
#         lapply(split(group$metrics, (seq_along(group$metrics) - 1) %/% 2), function(pair) {
#           fluidRow(
#             lapply(pair, function(metric) {
#               column(class = "metric-group-divs",6, h6( metric$name, random_number <- round(runif(1), 2)))
#             })
#           )
#         })
#     )
#   })
# )




##########################DEPRECATED#############################
# numbers <- reactiveValues(numbersList = c(), numericValues = numeric(0), sum = 0)
# 
# observeEvent(input$addButton, {
#   selected_var <- input$var
#   input_number <- input$number
#   
#   # Call the function to handle the logic
#   handleAddButton(selected_var, input_number, numbers)
# })
# 
# output$numbersList <- renderPrint({
#   paste(numbers$numbersList, ": ", numbers$numericValues)
# })
# 
# output$sumOutput <- renderText({
#   paste("Suma de pesos: ", numbers$sum)
# })
# 
# # Pass metric groups to the UI
# output$metricGroups <- renderPrint({
#   metricgroups
# })


#######################################################

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
