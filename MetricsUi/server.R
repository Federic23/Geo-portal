
source("modules/metric_groups.R")
library(ggplot2)

server <- function(input, output, session) {
  shinyjs::useShinyjs()
  source("modules/route_page_control.R", local = TRUE)
  
  
  ################################
  ########DATE CHOOSER CONTROLLER
  ################################
  filtered_data <- reactive({
    start_date <- input$startDate
    end_date <- input$endDate
    subset(df, date >= start_date & date <= end_date)
  })
  
  ################################
  ########DYNAMIC CONTROL VIEW
  ################################
  dynamicContent <- reactive({
    if (metric_content_visible()) {
      Sys.sleep(2)
      div(
        div(class = "metric-group-div",
            div(h5("Daily Average Total Result :"),
                h3("6.2")),
        ),
      fluidRow(
        lapply(metricsGroups, function(group) {
          div(class = "metric-group-div",
              h4(group$groupName, group$value),
              lapply(split(group$metrics, (seq_along(group$metrics) - 1) %/% 2), function(pair) {
                fluidRow(
                  lapply(pair, function(metric) {
                    column(class = "metric-group-divs",6, h6( metric$name, random_number <- round(runif(1), 2)))
                  })
                )
              })
          )
        }), 
      ))
    } else if (overall_metric_content_visible()){
      div(class = "plot", renderPlot({
        p <- ggplot(filtered_data(), aes(x = date, y = value)) +
          geom_line(linewidth = 1.5) +  # Use 'linewidth' for a thicker line
          labs(
            title = "Overall rating per day",
            x = "Date",
            y = "Value"
          ) +
          theme_minimal(base_size = 14) +  # Increase the base font size
          theme(
            plot.title = element_text(size = 28),  # Larger title font
            axis.title = element_text(size = 16),  # Larger axis title font
            axis.text = element_text(size = 14)    # Larger axis text font
          )
        p
      }),
      div(dateInput("startDate", "Select a start date:", value = "2023-10-15")),
      dateInput("endDate", "Select an end date:", value = "2023-10-22"))
      
    } else if (weight_content_visible()){
      div(generateDynamicInputs(metricsGroups),
          actionButton(class = "metric-button", "CsvFormatButton", "Generate Metrics"))
    }
  })
  
  output$dynamicContent <- renderUI({
    dynamicContent()
  })
  
  ################################
  ###################GENERATE INPUTS FOR WEIGHT
  ################################
  
  generateDynamicInputs <- function(metricsGroups) {
    input_list <- lapply(metricsGroups, function(group, group_idx) {
      # Create a unique ID for the group numeric input
      group_input_id <- paste("group_num_input", gsub("[^A-Za-z0-9]", "", group$groupName), sep="_")
      total_metrics <- length(group$metrics)
      div(class = "metric-group-div",
          fluidRow(
            column(1, h4(group$groupName)),
            column(11, numericInput(inputId = group_input_id, 
                                   label = NULL, 
                                   value = 0.33, 
                                   min = 0,
                                   max = 1, 
                                   step = 0.01), class = "parent-metric-input",)
          ),
            # Get the total number of metrics in the group
          lapply(split(group$metrics, (seq_along(group$metrics) - 1) %/% 2), function(pair) {
            fluidRow(
              lapply(pair, function(metric) {
                value <- if (total_metrics > 0) round(1 / total_metrics, 2) else 0 # Round to two decimal places
                column(class = "metric-group-divs", 6,
                       fluidRow(
                         column(3, h6(metric$name)),
                         column(9, numericInput(inputId = paste("input", gsub("[^A-Za-z0-9]", "", metric$name), sep="_"),
                                                label = NULL, 
                                                value = value,  # Value rounded to two decimal places
                                                min = 0, 
                                                max = 1, 
                                                step = 0.01))
                       ))
              })
            )
          })
      )
      
    })
    do.call(tagList, input_list)
  }
  
  observe({
    lapply(metricsGroups, function(group) {
      # Observer for group numeric input
      group_num_input_id <- paste("group_num_input", gsub("[^A-Za-z0-9]", "", group$groupName), sep="_")
      if (group_num_input_id %in% names(input)) {
        observeEvent(input[[group_num_input_id]], {
          # Handle the numeric input for the group
          cat(paste("Group numeric input", group_num_input_id, "changed to", input[[group_num_input_id]], "\n"))
        }, ignoreInit = TRUE)
      }
      
      # Observers for metric numeric inputs
      lapply(group$metrics, function(metric) {
        metric_input_id <- paste("input", gsub("[^A-Za-z0-9]", "", metric$name), sep="_")
        if (metric_input_id %in% names(input)) {
          observeEvent(input[[metric_input_id]], {
            # Handle the metric input
            cat(paste("Metric input", metric_input_id, "changed to", input[[metric_input_id]], "\n"))
          }, ignoreInit = TRUE)
        }
      })
    })
  })
  ################################
  ############################GENERATE CSV DATA 
  ################################
  
  show_csv_data <- reactiveVal(FALSE)
  
  observeEvent(input$file1, {
    # Print a message to the console when a file is uploaded
    print(paste("File", input$file1$name, "has been uploaded."))
    show_csv_data(TRUE)
  })
  
  # Render the conditional UI for the CSV data display
  output$csvDataDisplay <- renderUI({
    if (show_csv_data()) {  # This will now correctly reference the reactive value
      # Display the CSV data-related UI components
      fluidRow(
        column(class = "metric-group-divs", 3, h6("Size:", " 118KB")),
        column(class = "metric-group-divs", 3, h6("Starting Date:", "14/10/23 12:42")),
        column(class = "metric-group-divs", 3, h6("Finish Date: ", "15/10/23 15:31")),
        column(class = "metric-group-divs", 3, h6("Row count: ", "84478"))
      )
    } else {
      # Display placeholders or alternative content when CSV data isn't shown
      fluidRow(
        column(class = "metric-group-divs", 3, h6("Size: ", " -")),
        column(class = "metric-group-divs", 3, h6("Starting Date: ", " -")),
        column(class = "metric-group-divs", 3, h6("Finish Date: ", " -")),
        column(class = "metric-group-divs", 3, h6("Row count: ", " -"))
      )
    }
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
