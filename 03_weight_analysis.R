library(shiny)
source("../02_metrics.R")


getGroupsAndWeightList <- function() {
  
  # Define metrics and groups as before
  trafficMetrics <- list(
    list(name="Page Views", result=NULL),
    list(name="Unique Page Views", result=NULL),
    list(name="Unique Visitors", result=NULL),
    list(name="Bounce Rate", result=NULL)
  )
  
  visitorStatisticsMetrics <- list(
    list(name="Unique IPs", result=NULL),
    list(name="Average Amount of Pages Visited Per User", result=NULL),
    list(name="Recurring Visitors", result=NULL),
    list(name="Individual Resource Loading Times", result=NULL),
    list(name="Visits Per Day Average", result=NULL)
  )
  
  errorMetrics <- list(
    list(name="Failed Requests", result=NULL),
    list(name="Amount of 404 Errors", result=NULL)
  )
  
  activityMetrics <- list(
    list(name="Average Time Spent", result=NULL),
    list(name="Mean Time Per Session", result=NULL)
  )
  
  trafficSourcesMetrics <- list(
    list(name="Direct Traffic", result=NULL)
  )
  
  userBehaviorMetrics <- list(
    list(name="Average Time Spent Per Page", result=NULL)
  )
  
  # Initial groups setup
  groups <- list(
    list(name = "Traffic Metrics", metrics = trafficMetrics),
    list(name = "Visitor Statistics", metrics = visitorStatisticsMetrics),
    list(name = "Error Metrics", metrics = errorMetrics),
    list(name = "Activity Metrics", metrics = activityMetrics),
    list(name = "Traffic Sources", metrics = trafficSourcesMetrics),
    list(name = "User Behavior Metrics", metrics = userBehaviorMetrics)
  )
  
  # Calculate group weight
  totalGroups <- length(groups)
  groupWeight <- if (totalGroups > 0) round(1 / totalGroups, 2) else 0
  
  # Iterate over groups to set group weight and calculate metric weights
  for (i in seq_along(groups)) {
    groups[[i]]$weight <- groupWeight # Set group weight
    
    totalMetrics <- length(groups[[i]]$metrics)
    metricWeight <- if (totalMetrics > 0) round(1 / totalMetrics, 2) else 0
    
    # Iterate over metrics within each group to set metric weight
    for (j in seq_along(groups[[i]]$metrics)) {
      groups[[i]]$metrics[[j]]$weight <- metricWeight
    }
  }
  
  
  total <- 5
  
  return(list(total, groups))
}

appendFilePathToLog <- function(path) {
  logFilePath <- "selected_files_history.txt"

  # Open a connection in append mode
  con <- file(logFilePath, open = "a")

  # Write the path and a newline, then close the connection
  cat(path, "\n", file = con)
  close(con)
}

 calculateMetrics <- function(reactiveMetricsGroups) {
   groups <- reactiveMetricsGroups()
   data <- dataForMetrics()
   
   resultsPerMetric <- list(
     "Page Views" = calculate_total_pages_viewed(data),
     "Unique Page Views" = calculate_total_unique_pages_viewed(data),
     "Unique Visitors" = 1,  # TODO: FALTA CALCULAR ESTA METRICA
     "Bounce Rate" = calculate_bounce_rate(data),
     "Unique IPs" = calculate_number_of_users(data),
     "Average Amount of Pages Visited Per User" = calculate_average_pages_per_user(data),
     "Recurring Visitors" = calculate_recurring_visitors(data),
     "Individual Resource Loading Times" = calculate_average_loading_time(data)
     # "Visits Per Day Average" = visit_average,
     # "Failed Requests" = failed_requests_amount,
     # "Amount of 404 Errors" = error_404_amount,
     # "Average Time Spent" = average_time_per_visitor,
     # "Mean Time Per Session" = average_time_per_session,
     # "Direct Traffic" = direct_traffic,
     # "Average Time Spent Per Page" = average_time_per_page
   )
   
   totalGroupResult <- 0
   for (i in seq_along(groups)) {
    metrics <- groups[[i]]$metrics
    

    for (j in seq_along(metrics)) {
      metric <- metrics[[j]]
      metricName <- metric$name
      metricWeight <- metric$weight

      if (metricName %in% names(resultsPerMetric)) {
        metricResult <- resultsPerMetric[[metricName]]
        calculatedResult <- metricResult * metricWeight

        # Update the metric's result within the group
        groups[[i]]$metrics[[j]]$result <- calculatedResult

        totalGroupResult <- totalGroupResult + calculatedResult
      }
    }
  }
   total <- totalGroupResult
   
   return(list("groups" = groups, "total" = total))
   
   # separar por dia pa la grafica
   # fechas
   # asignar los pesos
 }



# ##################################################################################################
# ############################################### TESTS ##########################################
# ##################################################################################################

# trafficMetrics <- list(
#   list(name="Page Views", weight = 0.25, result=NULL),
#   list(name="Unique Page Views", weight = 0.25, result=NULL),
#   list(name="Unique Visitors", weight = 0.25, result=NULL),
#   list(name="Bounce Rate", weight = 0.25, result=NULL)
# )

# visitorStatisticsMetrics <- list(
#   list(name="Unique IPs", weight = 0.25, result=NULL),
#   list(name="Average Amount of Pages Visited Per User", weight = 0.25, result=NULL),
#   list(name="Recurring Visitors", weight = 0.20, result=NULL),
#   list(name="Individual Resource Loading Times", weight = 0.15, result=NULL),
#   list(name="Visits Per Day Average", weight = 0.15, result=NULL)
# )

# errorMetrics <- list(
#   list(name="Failed Requests", weight = 0.8, result=NULL),
#   list(name="Amount of 404 Errors", weight = 0.2, result=NULL)
# )

# activityMetrics <- list(
#   list(name="Average Time Spent", weight = 0.1, result=NULL),
#   list(name="Mean Time Per Session", weight = 0.9, result=NULL)
# )

# trafficSourcesMetrics <- list(
#   list(name="Direct Traffic", weight = 1, result=NULL)
# )

# userBehaviorMetrics <- list(
#   list(name="Average Time Spent Per Page", weight = 1, result=NULL)
# )

# groupsExample <- list(
#   list(name = "Traffic Metrics" , metrics = trafficMetrics, weight = 0.2, result=NULL),
#   list(name = "Visitor Statistics" , metrics = visitorStatisticsMetrics, weight = 0.1, result=NULL),
#   list(name = "Error Metrics" , metrics = errorMetrics, Weight = 0.3, result=NULL),
#   list(name = "Activity Metrics" , metrics = activityMetrics, weight = 0.1, result=NULL),
#   list(name = "Traffic Sources" , metrics = trafficSourcesMetrics, weight = 0.2, result=NULL),
#   list(name = "Average Time Spent Per Page" , metrics = userBehaviorMetrics, weight = 0.1, result=NULL)
# )

# resultGroups <- calculateMetrics(groupsExample)

# print(resultGroups)

# # results
# for (group in resultGroups) {
#   cat("Group:", group$name, "Weight:", group$weight, "Result:", ifelse(is.null(group$result), "NULL", group$result), "\n")
  
#   for (metric in group$metrics) {
#     cat("  Metric:", metric$name, "Metric Result:", ifelse(is.null(metric$result), "NULL", metric$result), "\n")
#   }
  
#   cat("\n")
# }