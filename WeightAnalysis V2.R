getGroupsAndWeightList <- function() {
  
  trafficMetrics <- list(
    list(name="Page Views", weight = 0, result=NULL),
    list(name="Unique Page Views", weight = 0, result=NULL),
    list(name="Unique Visitors", weight = 0, result=NULL),
    list(name="Bounce Rate", weight = 0, result=NULL)
  )
  
  visitorStatisticsMetrics <- list(
    list(name="Unique IPs", weight = 0, result=NULL),
    list(name="Average Amount of Pages Visited Per User", weight = 0, result=NULL),
    list(name="Recurring Visitors", weight = 0, result=NULL),
    list(name="Individual Resource Loading Times", weight = 0, result=NULL),
    list(name="Visits Per Day Average", weight = 0, result=NULL)
  )
  
  errorMetrics <- list(
    list(name="Failed Requests", weight = 0, result=NULL),
    list(name="Amount of 404 Errors", weight = 0, result=NULL)
  )
  
  activityMetrics <- list(
    list(name="Average Time Spent", weight = 0, result=NULL),
    list(name="Mean Time Per Session", weight = 0, result=NULL)
  )
  
  trafficSourcesMetrics <- list(
    list(name="Direct Traffic", weight = 0, result=NULL)
  )
  
  userBehaviorMetrics <- list(
    list(name="Average Time Spent Per Page", weight = 0, result=NULL)
  )
  
  groups <- list(
    list(name = "Traffic Metrics" , metrics = trafficMetrics, weight = 0, result=NULL),
    list(name = "Visitor Statistics" , metrics = visitorStatisticsMetrics, weight = 0, result=NULL),
    list(name = "Error Metrics" , metrics = errorMetrics, Weight = 0, result=NULL),
    list(name = "Activity Metrics" , metrics = activityMetrics, weight = 0, result=NULL),
    list(name = "Traffic Sources" , metrics = trafficSourcesMetrics, weight = 0, result=NULL),
    list(name = "Average Time Spent Per Page" , metrics = userBehaviorMetrics, weight = 0, result=NULL)
  )
  
  #PONER EL PESO DEFAULT PARA MANDARLO
  
  
  return(groups)
}

calculateMetrics <- function(groups) {
  resultsPerMetric <- list(
    "Page Views" = total_pages_viewed,
    "Unique Page Views" = total_unique_urls,
    "Unique Visitors" = 1,  # TODO: FALTA CALCULAR ESTA METRICA
    "Bounce Rate" = bounce_rate,
    "Unique IPs" = number_of_users,
    "Average Amount of Pages Visited Per User" = average_pages_per_user,
    "Recurring Visitors" = recurring_visitors,
    "Individual Resource Loading Times" = average_loading_time,
    "Visits Per Day Average" = visit_average,
    "Failed Requests" = failed_requests_amount,
    "Amount of 404 Errors" = error_404_amount,
    "Average Time Spent" = average_time_per_visitor,
    "Mean Time Per Session" = average_time_per_session,
    "Direct Traffic" = direct_traffic,
    "Average Time Spent Per Page" = average_time_per_page
  )
  
  for (group in groups) {
    metrics <- group$metrics
    totalGroupResult <- 0
    
    for (metric in metrics) {
      metricName <- metric$name
      metricWeight <- metric$weight
      
      cat("Processing metric:", metricName, " with weight:", metricWeight, "\n")
      
      if (metricName %in% names(resultsPerMetric)) {
        metricResult <- resultsPerMetric[[metricName]]
        metric$result <- metricResult * metricWeight
        cat("Calculating for", group$name, "-", metricName, ":", metricResult, " * ", metricWeight, "\n")
        
        totalGroupResult <- totalGroupResult + metric$result
        cat("After adding metric result to totalGroupResult:", totalGroupResult, "\n")
      }
      
    }
    
    cat("Before group$result calculation:", totalGroupResult, "\n")
    group$result <- totalGroupResult * group$weight
    cat("After group$result calculation:", totalGroupResult, " * ", group$weight, "\n\n")
    cat("Final group$result:", group$result, "\n\n")
  }
  
  #browser()
  
  return(groups)
}



##################################################################################################
############################################### PRUEBAS ##########################################
##################################################################################################

trafficMetrics <- list(
  list(name="Page Views", weight = 0.25, result=NULL),
  list(name="Unique Page Views", weight = 0.25, result=NULL),
  list(name="Unique Visitors", weight = 0.25, result=NULL),
  list(name="Bounce Rate", weight = 0.25, result=NULL)
)

visitorStatisticsMetrics <- list(
  list(name="Unique IPs", weight = 0.25, result=NULL),
  list(name="Average Amount of Pages Visited Per User", weight = 0.25, result=NULL),
  list(name="Recurring Visitors", weight = 0.20, result=NULL),
  list(name="Individual Resource Loading Times", weight = 0.15, result=NULL),
  list(name="Visits Per Day Average", weight = 0.15, result=NULL)
)

errorMetrics <- list(
  list(name="Failed Requests", weight = 0.8, result=NULL),
  list(name="Amount of 404 Errors", weight = 0.2, result=NULL)
)

activityMetrics <- list(
  list(name="Average Time Spent", weight = 0.1, result=NULL),
  list(name="Mean Time Per Session", weight = 0.9, result=NULL)
)

trafficSourcesMetrics <- list(
  list(name="Direct Traffic", weight = 1, result=NULL)
)

userBehaviorMetrics <- list(
  list(name="Average Time Spent Per Page", weight = 1, result=NULL)
)

groupsExample <- list(
  list(name = "Traffic Metrics" , metrics = trafficMetrics, weight = 0.2, result=NULL),
  list(name = "Visitor Statistics" , metrics = visitorStatisticsMetrics, weight = 0.1, result=NULL),
  list(name = "Error Metrics" , metrics = errorMetrics, Weight = 0.3, result=NULL),
  list(name = "Activity Metrics" , metrics = activityMetrics, weight = 0.1, result=NULL),
  list(name = "Traffic Sources" , metrics = trafficSourcesMetrics, weight = 0.2, result=NULL),
  list(name = "Average Time Spent Per Page" , metrics = userBehaviorMetrics, weight = 0.1, result=NULL)
)

resultGroups <- calculateMetrics(groupsExample)

print(resultGroups)

# results
for (group in resultGroups) {
  cat("Group:", group$name, "Weight:", group$weight, "Result:", ifelse(is.null(group$result), "NULL", group$result), "\n")
  
  for (metric in group$metrics) {
    cat("  Metric:", metric$name, "Metric Result:", ifelse(is.null(metric$result), "NULL", metric$result), "\n")
  }
  
  cat("\n")
}