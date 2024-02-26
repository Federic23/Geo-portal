if (!require("ggplot2")) {
  options(repos = c(CRAN = "https://cloud.r-project.org/"))
  install.packages("ggplot2")
  library(ggplot2)
}
library(ggplot2)

# install.packages("stringr") #for metrics using strings
library(stringr)

library(lubridate)

library(shiny)

# 
# # Read the CSV file
# output_path <- file.path("TestCases", "output_with_session.csv")
# data <- read.csv(output_path)
# 
# #Metric 1: number of users based on IP field
# number_of_users <- length(unique(data$ip))
# 
# cat("La cantidad de usuarios distintos es:", number_of_users, "\n")
# 
# #Metric 2: visits a day average
# data$datetime <- as.Date(data$datetime)
# 
# visit_average <- length(unique(data$ip)) / length(unique(data$datetime))
# 
# cat("El promedio de visitantes por día es:", visit_average, "\n")
# 
# #Metric 3: failed requests
# failed_requests <- subset(data, httpcode >= 400)
# failed_requests_amount <- nrow(failed_requests)
# 
# cat("La cantidad de solicitudes fallidas es:", failed_requests_amount, "\n")
# 
# # Percentage of successful requests (status code < 400)
# successful_requests <- subset(data, httpcode < 400)
# successful_requests_amount <- nrow(successful_requests)
# 
# total_requests <- nrow(data) #total number of requests
# percentage_successful_requests <- (successful_requests_amount / total_requests) * 100 # % of successful requests
# 
# cat("Percentage of successful requests is:", percentage_successful_requests, "%\n")
# 
# 
# #Metric 4: 404 errors
# errors_404 <- subset(data, httpcode == 404)
# cat("404 Errors:\n")
# cat(errors_404$url, "\n", sep = "\n")
# 
# error_404_amount <- length(errors_404)
# 
# cat("Cantidad de errores 404: ", error_404_amount, "\n")
# 
# total_requests <- nrow(data) #total number of requests
# 
# percentage_non404_requests <- ((total_requests - error_404_amount) / total_requests) * 100
# 
# cat("Percentage of non 404 error requests: ", percentage_non404_requests, "\n")
# 
# 
# #Metric 5: Average by day of week
# data$datetime <- as.Date(data$datetime)
# average_by_day <- tapply(data$httpcode, format(data$datetime, "%A"), length)
# cat("Average Requests by Day of the Week:\n")
# print(average_by_day)
# 
# average_by_day <- data.frame(Day = names(average_by_day), AverageRequests = as.numeric(average_by_day))
# 
# #Metric 6: Search engines
# search_engines <- c("Google", "Bing", "Yahoo", "DuckDuckGo", "Firefox")
# data$engine <- "Other" #initialization
# 
# for (engine in search_engines) {
#   data$engine[str_detect(tolower(data$useragent), tolower(engine))] <- engine
# }
# engine_counts <- table(data$engine)
# print(engine_counts)
# 
# # Metric 7: The total number of pages viewed on your website.
# total_pages_viewed <- nrow(data)
# cat("Total pages viewed: ",total_pages_viewed)
# 
# # Metric: The total of unique pages viewed on your website.
# total_unique_urls <- length(unique(data$url))
# cat("Total of unique pages viewed: ",total_unique_urls)
# 
# # Metric: Bounce Rate: The percentage of single-page sessions (users who leave your site after viewing only one page).
# bounce_rate <- with(data, sum(table(session) == 1) / length(unique(session)) * 100)
# cat("Bounce Rate:", format(bounce_rate, digits = 2), "%\n")
# 
# #Metric: Average Amount of pages visited per user
# average_pages_per_user <- length(unique(data$url)) / length(unique(data$session))
# print(paste("Average Amount of pages visited per user: ", sprintf("%.2f", average_pages_per_user)))
# 
# #Metric: Recurring visitors: amount of visitors that came back
# recurring_visitors <- length(unique(data$ip)) - length(unique(data$session))
# print(paste("Recurring Visitors:", recurring_visitors))
# 
# #Metric: Individual Resource Loading Times (average): to help identify elements that slow down page performance.
# average_loading_time <- mean(data$time_diff)
# print(paste("Individual Resource Loading Times (average):", sprintf("%.2f", average_loading_time)))
# 
# #Metric 15: average time spent per visitor
# average_time_per_visitor <- mean(data$time_diff, na.rm = TRUE)
# print(paste("Average Time Spent per Visitor:", sprintf("%.2f", average_time_per_visitor), "seconds"))
# 
# #Metric 15: average time spent per session
# unique_sessions <- length(unique(data$session))
# average_time_per_session <- total_time_on_site / unique_sessions
# print(paste("Average Time Spent per Visitor per session:", sprintf("%.2f", average_time_per_session), "seconds"))
# 
# # Metric: Direct Traffic
# direct_traffic <- sum(grepl("^\\s*-$", data$referer, perl = TRUE, ignore.case = TRUE))
# print(paste("Direct Traffic:", direct_traffic, "users"))
# 
# # Metric: Average Time Spent per Page
# average_time_per_page <- mean(data$time_diff, na.rm = TRUE)
# print(paste("Average Time Spent per Page:", sprintf("%.2f", average_time_per_page), "seconds"))


######################################################################

# Function to calculate the number of unique users #
calculate_number_of_users <- function(data) {
  unique_users <- length(unique(data$ip))
  return(unique_users)
}

# Function to calculate the average visits per day #
calculate_visits_a_day_average <- function(data) {
  data$datetime <- as.Date(data$datetime)
  visit_average <- length(unique(data$ip)) / length(unique(data$datetime))
  return(visit_average)
}

# Function to calculate the number of failed requests#
calculate_failed_requests <- function(data) {
  failed_requests <- subset(data, httpcode >= 400)
  failed_requests_amount <- nrow(failed_requests)
  return(failed_requests_amount)
}

# Function to calculate the percentage of successful requests#
calculate_percentage_successful_requests <- function(data) {
  successful_requests <- subset(data, httpcode < 400)
  successful_requests_amount <- nrow(successful_requests)
  total_requests <- nrow(data)
  percentage_successful_requests <- (successful_requests_amount / total_requests) * 100
  return(percentage_successful_requests)
}

# Function to calculate 404 errors and their percentage
calculate_404_errors_and_percentage <- function(data) {
  errors_404 <- subset(data, httpcode == 404)
  error_404_amount <- nrow(errors_404)
  total_requests <- nrow(data)
  percentage_non404_requests <- ((total_requests - error_404_amount) / total_requests) * 100
  return(error_404_amount)
}

# Function to calculate average requests by day of the week
calculate_average_by_day_of_week <- function(data) {
  data$datetime <- as.Date(data$datetime)
  average_by_day <- tapply(data$httpcode, format(data$datetime, "%A"), length)
  average_by_day_df <- data.frame(Day = names(average_by_day), AverageRequests = as.numeric(average_by_day))
  return(average_by_day_df)
}

# Function to identify counts of visits from different search engines
calculate_search_engine_visits <- function(data) {
  search_engines <- c("Google", "Bing", "Yahoo", "DuckDuckGo", "Firefox")
  data$engine <- "Other" # Initialization
  
  for (engine in search_engines) {
    data$engine[str_detect(tolower(data$useragent), tolower(engine))] <- engine
  }
  
  engine_counts <- table(data$engine)
  return(engine_counts)
}

# Function to calculate the total number of pages viewed
calculate_total_pages_viewed <- function(data) {
  total_pages_viewed <- nrow(data)
  return(total_pages_viewed)
}

# Function to calculate the total of unique pages viewed
calculate_total_unique_pages_viewed <- function(data) {
  total_unique_urls <- length(unique(data$url))
  return(total_unique_urls)
}

# Function to calculate bounce rate
calculate_bounce_rate <- function(data) {
  bounce_rate <- with(data, sum(table(session) == 1) / length(unique(session)) * 100)
  return(bounce_rate)
}

# Function to calculate the average amount of pages visited per user
calculate_average_pages_per_user <- function(data) {
  average_pages_per_user <- length(unique(data$url)) / length(unique(data$session))
  result <- sprintf("%.2f", average_pages_per_user)
  return(as.numeric(result))
}

# Function to calculate the number of recurring visitors
calculate_recurring_visitors <- function(data) {
  recurring_visitors <- length(unique(data$ip)) - length(unique(data$session))
  return(recurring_visitors)
}

# Function to calculate the average individual resource loading times
calculate_average_loading_time <- function(data) {
  average_loading_time <- mean(data$time_diff)
  result <- sprintf("%.2f", average_loading_time)
  return(as.numeric(result))
}

# Function to calculate the average time spent per visitor
calculate_average_time_per_visitor <- function(data) {
  average_time_per_visitor <- mean(data$time_diff, na.rm = TRUE)
  result <- sprintf("%.2f", average_time_per_visitor)
  return(as.numeric(result))
}

calculate_average_time_per_session <- function(data) {
  # Ensure the 'datetime' column exists
  if(!"datetime" %in% names(data)) {
    stop("Column 'datetime' not found in the dataset.")
  }
  
  # Convert 'datetime' to POSIXct for accurate date-time operations
  data$datetime <- as.POSIXct(data$datetime)
  
  # Aggregate to find the start and end times for each session
  session_starts <- aggregate(datetime ~ session, data, min)
  session_ends <- aggregate(datetime ~ session, data, max)
  
  # Merge the start and end times into a single data frame
  session_durations <- merge(session_starts, session_ends, by = "session")
  names(session_durations) <- c("session", "start", "end")
  
  # Calculate the duration of each session
  session_durations$duration <- as.numeric(difftime(session_durations$end, session_durations$start, units = "secs"))
  
  # Calculate the average session duration
  average_session_duration_secs <- mean(session_durations$duration)
  
  # Convert the average duration to minutes
  average_session_duration_minutes <- average_session_duration_secs / 60
  
  return(average_session_duration_minutes)
}

# Function to calculate direct traffic
calculate_direct_traffic <- function(data) {
  direct_traffic <- sum(grepl("^\\s*-$", data$referer, perl = TRUE, ignore.case = TRUE))
  return(direct_traffic)
}

# Function to calculate the average time spent per page
calculate_average_time_per_page <- function(data) {
  average_time_per_page <- mean(data$time_diff, na.rm = TRUE)
  result <- sprintf("%.2f", average_time_per_page)
  return(as.numeric(result))
}

calculate_unique_visitors <- function(data) {
  unique_sessions <- unique(data$sessions)
  unique_visitor_count <- length(unique_sessions)
  return(unique_visitor_count)
}


# 
# ################################################################
# 
# 
# # Ensure datetime is in Date format
# data$datetime <- as.Date(data$datetime)
# 
# # Metric 1: Daily number of unique visitors
# daily_unique_visitors <- aggregate(x = list(UniqueVisitors = data$ip), by = list(Date = data$datetime), FUN = function(x) length(unique(x)))
# 
# # Metric 2: Daily visits
# daily_visits <- aggregate(x = list(Visits = data$ip), by = list(Date = data$datetime), FUN = length)
# 
# # Metric 3: Daily failed requests
# daily_failed_requests <- aggregate(x = list(FailedRequests = data$httpcode), by = list(Date = data$datetime), FUN = function(x) sum(x >= 400))
# 
# # Metric 4: Daily total pages viewed
# daily_pages_viewed <- aggregate(x = list(PagesViewed = data$url), by = list(Date = data$datetime), FUN = length)
# 
# # Metric 5: Daily average loading time
# daily_avg_loading_time <- aggregate(x = list(AvgLoadingTime = data$time_diff), by = list(Date = data$datetime), FUN = mean, na.rm = TRUE)
# 
# # Print or save the results
# print(daily_unique_visitors)
# print(daily_visits)
# print(daily_failed_requests)
# print(daily_pages_viewed)
# print(daily_avg_loading_time)
# 
# # Optionally, save the daily metrics to a CSV file
# write.csv(daily_unique_visitors, "daily_unique_visitors.csv")
# write.csv(daily_visits, "daily_visits.csv")
# write.csv(daily_failed_requests, "daily_failed_requests.csv")
# write.csv(daily_pages_viewed, "daily_pages_viewed.csv")
# write.csv(daily_avg_loading_time, "daily_avg_loading_time.csv")

