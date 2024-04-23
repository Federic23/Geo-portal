
source("../03_weight_analysis.R")
source("../02_metrics.R")
source("../00_preprocessing.R")
source("../01_processing.R")
library(ggplot2)

server <- function(input, output, session) {
  shinyjs::useShinyjs()
  source("modules/route_page_control.R", local = TRUE)
  
  
  ################################
  ########DATE CHOOSER CONTROLLER
  ################################
  metricsData <- reactiveValues(daily_metrics = data.frame())
  
  filtered_data <- reactive({
    req(input$startDate, input$endDate)  # Ensure that both dates are selected
    start_date <- as.Date(input$startDate)
    end_date <- as.Date(input$endDate)
    # Assuming 'date' is a column in your metricsData that contains date information
    dplyr::filter(metricsData$daily_metrics, date >= start_date & date <= end_date)
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
    details <- fileDetails() 
    if (metric_content_visible()) {
      updatedGroupsInfo <- calculateMetrics(reactiveMetricsGroups)
      calculatedMetrics <- calculateMetricsDaily(reactiveMetricsGroups)
      metricsData$daily_metrics <- calculatedMetrics
      updatedGroups <- updatedGroupsInfo$groups
      total <- updatedGroupsInfo$total
      div(
        div(class = "metric-group-div",
            div(h5("Total Result :"),
                h3(total)),
        ),
      fluidRow(
        lapply(updatedGroups, function(group) {
          div(class = "metric-group-div",
              h4(group$name, group$result), 
              lapply(split(group$metrics, (seq_along(group$metrics) - 1) %/% 2), function(pair) {
                fluidRow(
                  lapply(pair, function(metric) {
                    result_display <- if(is.null(metric$result)) { 0 } else { metric$result }
                    column(class = "metric-group-divs", 6, h6(metric$name, result_display))
                  })
                )
              })
          )
        }), 
      ))
    } else if (overall_metric_content_visible()){
      div(class = "plot", renderPlot({
        p <- ggplot(filtered_data(), aes(x = date, y = value)) +
          geom_line(linewidth = 1.5) + 
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
      # Progression Date Selector
      div(dateInput("startDate", "Select a start date:", value = details$firstDate, max = details$lastDate, min = details$firstDate)),
      dateInput("endDate", "Select an end date:", value = details$lastDate,  min = details$firstDate, max = details$lastDate))
      
    } else if (weight_content_visible()){
      div(
        actionButton(class = "metric-button", "CsvFormatButton", "Generate Metrics"),
        generateDynamicInputs(metricsGroups)
         )
    }
  })
  
  output$dynamicContent <- renderUI({
    dynamicContent()
  })
  
  ################################
  ###################GENERATE INPUTS FOR WEIGHT
  ################################
  
  generateDynamicInputs <- function(metricsGroups) {
    total_groups <- length(metricsGroups)
    group_value <- if (total_groups > 0) round(1 / total_groups, 2) else 0
    input_list <- lapply(metricsGroups, function(group, group_idx) {
      # Create a unique ID for the group numeric input
      group_input_id <- paste( gsub("[^A-Za-z0-9]", "", group$name), sep="_")
      total_metrics <- length(group$metrics)
      div(class = "metric-group-div",
          fluidRow(
            column(1, h4(group$name)),
            column(11, numericInput(inputId = group_input_id, 
                                   label = NULL, 
                                   value = group$weight, 
                                   min = 0,
                                   max = 1, 
                                   step = 0.01), class = "parent-metric-input",)
          ),
            # Get the total number of metrics in the group
          lapply(split(group$metrics, (seq_along(group$metrics) - 1) %/% 2), function(pair) {
            fluidRow(
              lapply(pair, function(metric) {
                value <- if (total_metrics > 0) round(1 / total_metrics, 2) else 0 
                column(class = "metric-group-divs", 6,
                       fluidRow(
                         column(3, h6(metric$name)),
                         column(9, numericInput(inputId = paste(gsub("[^A-Za-z0-9]", " ", metric$name), sep="_"),
                                                label = NULL, 
                                                value = metric$weight, 
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
          
          currentData <- reactiveMetricsGroups()
          
          # Find the corresponding group in currentData
          for (group_idx in seq_along(currentData)) {
            if (group$name == currentData[[group_idx]]$name) {
              currentData[[group_idx]]$weight <- input[[group_num_input_id]]
            }
          }
          
          # Update the reactive variable with the modified data
          reactiveMetricsGroups(currentData)
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
  
  observeEvent(input$select_file, {
    # Print a message to the console when a file is uploaded
    print(paste("File", input$file1, "has been uploaded."))
    show_csv_data(TRUE)
  })
  
  
  output$csvDataDisplay <- renderUI({
    if (show_csv_data()) { 
      details <- fileDetails() 
      fluidRow(
        column(class = "metric-group-divs", 3, h6("Size:", details$fileSize, " Bytes")),
        column(class = "metric-group-divs", 3, h6("Starting Date:", details$firstDate)),
        column(class = "metric-group-divs", 3, h6("Finish Date: ", details$lastDate)),
        column(class = "metric-group-divs", 3, h6("Row count: ", details$rowCount))
      )
    } else {
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



