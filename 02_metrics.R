if (!require("ggplot2")) {
  options(repos = c(CRAN = "https://cloud.r-project.org/"))
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require(stringr)) {
  install.packages("stringr")
}

if (!require(lubridate)) {
  install.packages("lubridate")
}


if (!require(dplyr)) {
  install.packages("dplyr")
}

library(ggplot2)

library(stringr)

library(lubridate)

library(shiny)

library(dplyr)

# Function to calculate the total number of pages viewed 1
calculate_total_pages_viewed <- function(data) {
  total_pages_viewed <- nrow(data)
  return(total_pages_viewed)
}

# Function to calculate the total of unique pages viewed 2
calculate_total_unique_pages_viewed <- function(data) {
  total_unique_urls <- length(unique(data$url))
  return(total_unique_urls)
}

calculate_unique_visitors <- function(data) {
  unique_sessions <- unique(data$session)
  unique_visitor_count <- length(unique_sessions)
  return(unique_visitor_count)
}

# Function to calculate bounce rate
calculate_bounce_rate <- function(data) {
  bounce_rate <- with(data, sum(table(session) == 1) / length(unique(session)) * 100)
  return(bounce_rate)
}

# Function to calculate the number of unique users #
calculate_number_of_users <- function(data) {
  unique_users <- length(unique(data$ip))
  return(unique_users)
}

# Function to calculate the average amount of pages visited per user
calculate_average_pages_per_user <- function(data) {
  average_pages_per_user <- length(unique(data$url)) / length(unique(data$session))
  result <- sprintf("%.2f", average_pages_per_user)
  return(as.numeric(result))
}

# Function to calculate the number of recurring visitors
calculate_recurring_visitors <- function(data) {
  recurring_visitors <- data %>%
    group_by(ip) %>%
    summarise(num_sessions = n_distinct(session)) %>%
    filter(num_sessions > 1) %>%
    nrow()
  
  return(recurring_visitors)
}

# Function to calculate the average individual resource loading times
calculate_average_loading_time <- function(data) {
  average_loading_time <- mean(data$time_diff)
  result <- sprintf("%.2f", average_loading_time)
  return(as.numeric(result))
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

# Function to calculate 404 errors and their percentage
calculate_404_errors_and_percentage <- function(data) {
  errors_404 <- subset(data, httpcode == 404)
  error_404_amount <- nrow(errors_404)
  total_requests <- nrow(data)
  percentage_non404_requests <- ((total_requests - error_404_amount) / total_requests) * 100
  return(error_404_amount)
}

# Function to calculate the average time spent per visitor
calculate_average_time_per_visitor <- function(data) {
  average_time_per_visitor <- mean(data$time_diff, na.rm = TRUE)
  result <- sprintf("%.2f", average_time_per_visitor)
  return(as.numeric(result))
}

calculate_average_time_per_session <- function(data) {
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
  session_starts <- !duplicated(data$session)
  empty_referer_starts <- data$referer[session_starts] == "-" | is.na(data$referer[session_starts])
  
  direct_traffic <- sum(empty_referer_starts)
  
  return(direct_traffic)
}

# Function to calculate the average time spent per page
calculate_average_time_per_page <- function(data) {
  average_time_per_page <- mean(data$time_diff, na.rm = TRUE)
  result <- sprintf("%.2f", average_time_per_page)
  return(as.numeric(result))
}
