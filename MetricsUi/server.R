
source("../WeightAnalysis V2.R")
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
  
  ##################################
  #########GetValues##############
  ##################################
  
  result <- getGroupsAndWeightList()
  total <- result[[1]]
  groups <- result[[2]]
  metricsGroups <- groups
  reactiveMetricsGroups <- reactiveVal(groups)
  fileChooserOpen <- reactiveVal(FALSE)
  
  ################################
  ########DYNAMIC CONTROL VIEW
  ################################
  dynamicContent <- reactive({
    if (metric_content_visible()) {
      Sys.sleep(2)
      div(
        div(class = "metric-group-div",
            div(h5("Daily Average Total Result :"),
                h3(total)),
        ),
      fluidRow(
        lapply(metricsGroups, function(group) {
          div(class = "metric-group-div",
              h4(group$name, group$value),
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
      group_input_id <- paste( gsub("[^A-Za-z0-9]", "", group$name), sep="_")
      total_metrics <- length(group$metrics)
      div(class = "metric-group-div",
          fluidRow(
            column(1, h4(group$name)),
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
                         column(9, numericInput(inputId = paste(gsub("[^A-Za-z0-9]", " ", metric$name), sep="_"),
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
      group_num_input_id <- paste(gsub("[^A-Za-z0-9]", "", group$name), sep="_")
      if (group_num_input_id %in% names(input)) {
        observeEvent(input[[group_num_input_id]], {
          # Handle the numeric input for the group
          cat(paste("Group numeric input", group_num_input_id, "changed to", input[[group_num_input_id]], "\n"))
        }, ignoreInit = TRUE)
      }
      
      # Observers for metric numeric inputs
      lapply(group$metrics, function(metric) {
        metric_input_id <- paste(gsub("[^A-Za-z0-9]", " ", metric$name), sep="_")
        if (metric_input_id %in% names(input)) {
          observeEvent(input[[metric_input_id]], {
            # Handle the metric input
            currentData <- reactiveMetricsGroups()
            
            # Reconstruct the metric name from metric_input_id by adding spaces
            metric_name_with_spaces <- gsub("_", " ", metric_input_id)
            
            # Directly update the metric in currentData
            # Assuming currentData is a list of groups, and each group has a list of metrics
            for (group_idx in seq_along(currentData)) {
              group <- currentData[[group_idx]]
              if (metric_name_with_spaces %in% lapply(group$metrics, `[[`, "name")) {
                idx <- which(lapply(group$metrics, `[[`, "name") == metric_name_with_spaces)
                
                # Update the weight of the specific metric directly in currentData
                currentData[[group_idx]]$metrics[[idx]]$weight <- input[[metric_input_id]]
              }
            }
            # Update the reactive variable with the modified data
            reactiveMetricsGroups(currentData)
            
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
    print(paste("File", input$file1, "has been uploaded."))
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
  
  observeEvent(input$select_file, {
    if (!fileChooserOpen()) {
      
      printPath()
    }
  })
}


# Leer el path porque se complica




