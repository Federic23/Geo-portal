# Define a function to handle the logic
group1_metrics <- list(
  list(name = "Pageviews", id = "trafficMetricA"),
  list(name = "Unique Visitors", id = "trafficMetricB"),
  list(name = "Sessions", id = "trafficMetricC"),
  list(name = "Bounce Rate", id = "trafficMetricD")
)

group1 <- list(groupName = "Traffic Metrics", metrics = group1_metrics, value = 3.0)

group2_metrics <- list(
  list(name = "Unique IPs", id = "visitorMetricA"),
  list(name = "Average Amount of pages visited per user", id = "visitorMetricB"),
  list(name = "Average Amount of pages visited per session", id = "visitorMetricC"),
  list(name = "Recurring visitors: amount of visitors that came back", id = "visitorMetricD"),
  list(name = "Individual Resource Loading Times (average)", id = "visitorMetricE")
  # list(name = "visits per day average", id = "visitorMetricF")
)

group2 <- list(groupName = "Visitor Statistics", metrics = group2_metrics, value = 3.8)

group3_metrics <- list(
  list(name = "Failed requests (amount)", id = "errorMetricA"),
  list(name = "Failed requests (percentage)", id = "errorMetricB"),
  list(name = "Amount of 404 errors", id = "errorMetricC"),
  list(name = "Other errors", id = "errorMetricD")
)

group3 <- list(groupName = "Error Metrics", metrics = group3_metrics, value = -0.6)

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



