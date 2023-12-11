
getGroupsAndWeightList <- function() {
  
  trafficMetrics <- list(
    list(name="Page Views", weight = NULL, result=NULL),
    list(name="Unique Page Views", weight = NULL, result=NULL),
    list(name="Unique Visitors", weight = NULL, result=NULL),
    list(name="Bounce Rate", weight = NULL, result=NULL)
  )
  
  visitorStatisticsMetrics <- list(
    list(name="Unique IPs", weight = NULL, result=NULL),
    list(name="Average Amount of Pages Visited Per User", weight = NULL, result=NULL),
    list(name="Recurring Visitors", weight = NULL, result=NULL),
    list(name="Individual Resource Loading Times", weight = NULL, result=NULL),
    list(name="Visits Per Day Average", weight = NULL, result=NULL)
  )
  
  errorMetrics <- list(
    list(name="Failed Requests", weight = NULL, result=NULL),
    list(name="Amount of 404 Errors", weight = NULL, result=NULL)
  )
  
  activityMetrics <- list(
    list(name="Average Time Spent", weight = NULL, result=NULL),
    list(name="Mean Time Per Session", weight = NULL, result=NULL)
  )
  
  trafficSourcesMetrics <- list(
    list(name="Direct Traffic", weight = NULL, result=NULL)
  )
  
  userBehaviorMetrics <- list(
    list(name="Average Time Spent Per Page", weight = NULL, result=NULL)
  )
  
  groups <- list(
    list(name = "Traffic Metrics" , metrics = trafficMetrics, weight = NULL, result=NULL),
    list(name = "Visitor Statistics" , metrics = visitorStatisticsMetrics, weight = NULL, result=NULL),
    list(name = "Error Metrics" , metrics = errorMetrics, Weight = NULL, result=NULL),
    list(name = "Activity Metrics" , metrics = activityMetrics, weight = NULL, result=NULL),
    list(name = "Traffic Sources" , metrics = trafficSourcesMetrics, weight = NULL, result=NULL),
    list(name = "Average Time Spent Per Page" , metrics = userBehaviorMetrics, weight = NULL, result=NULL)
  )
  
  #PONER EL PESO DEFAULT PARA MANDARLO
  

  return(groups)
}




calculateMetrics <- function(groups) {
  resultsPerMetric <- list(
    list(name="Page Views", metricResult = total_pages_viewed),
    list(name="Unique Page Views", metricResult = total_unique_urls),
    list(name="Unique Visitors", metricResult = 1), # TODO: FALTA CALCULAR ESTA METRICA
    list(name="Bounce Rate", metricResult = bounce_rate),
    list(name="Unique IPs", metricResult = number_of_users),
    list(name="Average Amount of Pages Visited Per User", metricResult = average_pages_per_user),
    list(name="Recurring Visitors", metricResult = recurring_visitors),
    list(name="Individual Resource Loading Times", metricResult = average_loading_time),
    list(name="Visits Per Day Average", metricResult = visit_average),
    list(name="Failed Requests", metricResult = failed_requests_amount),
    list(name="Amount of 404 Errors", metricResult = error_404_amount),
    list(name="Average Time Spent", metricResult = average_time_per_visitor),
    list(name="Mean Time Per Session", metricResult = average_time_per_session),
    list(name="Direct Traffic", metricResult = direct_traffic),
    list(name="Average Time Spent Per Page", metricResult = average_time_per_page)
  )
  
  for (group in groups) {
    metrics <- group$metrics
    totalGroupResult <- 0
    
    for (i in seq_along(metrics)) {
      metric <- metrics[[i]]
      metricName <- metric$name
      
      resultEntry <- resultsPerMetric[[which(sapply(resultsPerMetric, function(x) x$name == metricName))]]
      
      if (!is.null(resultEntry)) {
        metric$result <- resultEntry$metricResult * group$weight
        totalGroupResult <- totalGroupResult + resultEntry$metricResult * group$weight
        
        metric$result <- metric$result
        
      }
    }
    
    group$result <- totalGroupResult * group$weight
  }
  
  #TODO: calculate final number and return it with groups list
  
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

# Print the results
for (group in resultGroups) {
  cat("Group:", group$name, "Weight:", group$weight, "Result:", ifelse(is.null(group$result), "NULL", group$result), "\n")
  
  # If you want to print the metrics within each group
  for (metric in group$metrics) {
    cat("  Metric:", metric$name, "Metric Result:", ifelse(is.null(metric$result), "NULL", metric$result), "\n")
  }
  
  cat("\n")
}
