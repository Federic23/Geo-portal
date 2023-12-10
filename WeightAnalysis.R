####START USER INTERACTION####
metrics <- c("Metric 1: number of users based on IP field", 
             "Metric 2: visits per day average",
             "Metric 3: failed requests",
             "Metric 4: 404 errors",
             "Metric 5: Average by day of the week",
             "Metric 6: mean time of the session",
             "Metric 7: Clicks per session"
)
weights <- numeric(length(metrics))  # Vector to store weights
remaining_weight <- 1

cat("List of available metrics:\n")
for (i in 1:length(metrics)) {
  cat(i, ": ", metrics[i], "\n")
}

while (TRUE) {
  for (i in 1:length(metrics)) {
    selected_metric <- as.integer(readline(paste("Select a metric (", i, " of ", length(metrics), "): ")))
    
    if (is.na(selected_metric) || selected_metric < 1 || selected_metric > length(metrics)) {
      cat("Invalid selection.\n")
      break
    }
    
    cat("Selected metric:", metrics[selected_metric], "\n")
    
    repeat {
      weight <- as.numeric(readline(paste("Enter the weight for '", metrics[selected_metric], "' (between 0 and ", remaining_weight, " inclusive): ")))
      
      if (!is.na(weight) && weight >= 0 && weight <= remaining_weight) {
        weights[selected_metric] <- weight
        remaining_weight <- remaining_weight - weight
        break
      } else {
        cat("The entered weight is invalid or exceeds the remaining weight. Please enter a valid weight.\n")
      }
    }
    
    if (remaining_weight <= 0) {
      break
    }
    
    if (i < length(metrics)) {
      cat("Remaining weight:", remaining_weight, "\n")
      cont <- as.character(readline("Do you want to continue? (y/n): "))
      if (cont != "y") {
        break
      }
    }
  }
  
  if (sum(weights) == 1) {
    cat("Assigned weights:\n")
    for (i in 1:length(metrics)) {
      cat(metrics[i], " Weight:", weights[i], "\n")
    }
    break
  } else {
    cat("Weights must sum up to 1. Please review your values.\n")
    remaining_weight <- 1
  }
}
####END USER INTERACTION####

#metricsResults <- numeric(length(metrics))  # Vector to store metrics results
metricsResults <- list(
  "1" = number_of_users,
  "2" = visit_average,
  "3" = failed_requests_amount
)

metricsResults

results <- list() #weight x metricresult


for (i in 1:length(metricsResults)) {
  key <- as.character(i)
  result <- metricsResults[[key]] * weights[i]
  results[[key]] <- result
}

results

#sumar todos
resultsSum <- sum(unlist(results))
resultsSum
