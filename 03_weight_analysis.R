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

 daily_metrics_df <- data.frame()
 calculateMetrics <- function(reactiveMetricsGroups) {
   groups <- reactiveMetricsGroups()
   data <- dataForMetrics()
   
   
   resultsPerMetric <- list(
     "Page Views" = calculate_total_pages_viewed(data),
     "Unique Page Views" = calculate_total_unique_pages_viewed(data),
     "Unique Visitors" = calculate_unique_visitors(data),
     "Bounce Rate" = calculate_bounce_rate(data),
     "Unique IPs" = calculate_number_of_users(data),
     "Average Amount of Pages Visited Per User" = calculate_average_pages_per_user(data),
     "Recurring Visitors" = calculate_recurring_visitors(data),
     "Individual Resource Loading Times" = calculate_average_loading_time(data),
     "Visits Per Day Average" = calculate_visits_a_day_average(data),
     "Failed Requests" = calculate_failed_requests(data),
     "Amount of 404 Errors" = calculate_404_errors_and_percentage(data),
     "Average Time Spent" = calculate_average_time_per_visitor(data),
     "Mean Time Per Session" = calculate_average_time_per_session(data),
     "Direct Traffic" = calculate_direct_traffic(data),
     "Average Time Spent Per Page" = calculate_average_time_per_page(data)
   )
   
   # Replace the manual processing with a call to calculate_group_results
   results <- calculate_group_results(groups, resultsPerMetric)
   
   # Return the results directly
   return(results)
 }
 
 calculateMetricsDaily <- function(reactiveMetricsGroups) {
   groups <- reactiveMetricsGroups()
   data <- dataForMetrics()
   data$date <- as.Date(data$datetime, format="%Y-%m-%d %H:%M:%S")
   unique_days <- unique(data$date)
   daily_results <- list()

   for (i in seq_along(unique_days)){
     day <- unique_days[i]  # Get the date for the current index
     day_data <- subset(data, date == day)

     # Calculate metrics for the day
     resultsPerMetric <- list(
       "Page Views" = calculate_total_pages_viewed(day_data),
       "Uniue Page Views" = calculate_total_unique_pages_viewed(day_data),
       "Unique Visitors" = calculate_unique_visitors(day_data),
       "Bounce Rate" = calculate_bounce_rate(day_data),
       "Unique IPs" = calculate_number_of_users(day_data),
       "Average Amount of Pages Visited Per User" = calculate_average_pages_per_user(day_data),
       "Recurring Visitors" = calculate_recurring_visitors(day_data),
       "Individual Resource Loading Times" = calculate_average_loading_time(day_data),
       "Visits Per Day Average" = calculate_visits_a_day_average(day_data),
       "Failed Requests" = calculate_failed_requests(day_data),
       "Amount of 404 Errors" = calculate_404_errors_and_percentage(day_data),
       "Average Time Spent" = calculate_average_time_per_visitor(day_data),
       "Mean Time Per Session" = calculate_average_time_per_session(day_data),
       "Direct Traffic" = calculate_direct_traffic(day_data),
       "Average Time Spent Per Page" = calculate_average_time_per_page(day_data)
     )

     # Process the groups with the day's metrics
     day_key <- format(day, "%Y-%m-%d")
     # Calculate your day's results
     day_results <- calculate_group_results(groups, resultsPerMetric)
     # Store the results for the day using 'day_key'
     daily_results[[day_key]] <- day_results$total
   }

   daily_metrics_df <- data.frame(
     date = as.Date(names(daily_results), format="%Y-%m-%d"),
     value = unlist(daily_results)
   )
   
   # Return the results directly
   return(daily_metrics_df)
 }
 
 calculate_group_results <- function(groups, resultsPerMetric) {
   totalGroupsResult <- 0
   
   for (i in seq_along(groups)) {
     metrics <- groups[[i]]$metrics
     group_weight <- groups[[i]]$weight
     totalGroupResult <- 0
     
     for (j in seq_along(metrics)) {
       metric <- metrics[[j]]
       metricName <- metric$name
       metricWeight <- metric$weight
       
       if (metricName %in% names(resultsPerMetric)) {
         metricResult <- resultsPerMetric[[metricName]]
         calculatedResult <- round(metricResult * metricWeight, 1)
         # Update the metric's result within the group
         groups[[i]]$metrics[[j]]$result <- calculatedResult
         
         totalGroupResult <- totalGroupResult + calculatedResult
       }
     }
     resultWithWeight <- round(totalGroupResult * group_weight, 1)
     totalGroupsResult <- (totalGroupsResult + resultWithWeight) 
     groups[[i]]$result <- resultWithWeight
   }
   
   return(list(groups = groups, total = totalGroupsResult))
 }