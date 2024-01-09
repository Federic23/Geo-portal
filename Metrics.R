install.packages(c("ggplot2"))
library(ggplot2)

install.packages("stringr") #for metrics using strings
library(stringr)

library(lubridate)


# Read the CSV file
output_path <- file.path("TestCases", "output_with_session.csv")
data <- read.csv(output_path)


#Metric 1: number of users based on IP field
number_of_users <- length(unique(data$ip))

cat("La cantidad de usuarios distintos es:", number_of_users, "\n")

#Metric 2: visits a day average
data$datetime <- as.Date(data$datetime)

visit_average <- length(unique(data$ip)) / length(unique(data$datetime))

cat("El promedio de visitantes por día es:", visit_average, "\n")

#Metric 3: failed requests
failed_requests <- subset(data, httpcode >= 400)
failed_requests_amount <- nrow(failed_requests)

cat("La cantidad de solicitudes fallidas es:", failed_requests_amount, "\n")

# Percentage of successful requests (status code < 400)
successful_requests <- subset(data, httpcode < 400)
successful_requests_amount <- nrow(successful_requests)

total_requests <- nrow(data) #total number of requests
percentage_successful_requests <- (successful_requests_amount / total_requests) * 100 # % of successful requests

cat("Percentage of successful requests is:", percentage_successful_requests, "%\n")


#Metric 4: 404 errors
errors_404 <- subset(data, httpcode == 404)
cat("404 Errors:\n")
cat(errors_404$url, "\n", sep = "\n")

error_404_amount <- length(errors_404)

cat("Cantidad de errores 404: ", error_404_amount, "\n")

total_requests <- nrow(data) #total number of requests

percentage_non404_requests <- ((total_requests - error_404_amount) / total_requests) * 100

cat("Percentage of non 404 error requests: ", percentage_non404_requests, "\n")


#Metric 5: Average by day of week
data$datetime <- as.Date(data$datetime)
average_by_day <- tapply(data$httpcode, format(data$datetime, "%A"), length)
cat("Average Requests by Day of the Week:\n")
print(average_by_day)

average_by_day <- data.frame(Day = names(average_by_day), AverageRequests = as.numeric(average_by_day))

#Metric 6: Search engines
search_engines <- c("Google", "Bing", "Yahoo", "DuckDuckGo", "Firefox")
data$engine <- "Other" #initialization

for (engine in search_engines) {
  data$engine[str_detect(tolower(data$useragent), tolower(engine))] <- engine
}
engine_counts <- table(data$engine)
print(engine_counts)

# Metric 7: The total number of pages viewed on your website.
total_pages_viewed <- nrow(data)
cat("Total pages viewed: ",total_pages_viewed)

# Metric: The total of unique pages viewed on your website.
total_unique_urls <- length(unique(data$url))
cat("Total of unique pages viewed: ",total_unique_urls)

# Metric: Bounce Rate: The percentage of single-page sessions (users who leave your site after viewing only one page).
bounce_rate <- with(data, sum(table(session) == 1) / length(unique(session)) * 100)
cat("Bounce Rate:", format(bounce_rate, digits = 2), "%\n")

#Metric: Average Amount of pages visited per user 
average_pages_per_user <- length(unique(data$url)) / length(unique(data$session))
print(paste("Average Amount of pages visited per user: ", sprintf("%.2f", average_pages_per_user)))

#Metric: Recurring visitors: amount of visitors that came back
recurring_visitors <- length(unique(data$ip)) - length(unique(data$session))
print(paste("Recurring Visitors:", recurring_visitors))

#Metric: Individual Resource Loading Times (average): to help identify elements that slow down page performance.
average_loading_time <- mean(data$time_diff)
print(paste("Individual Resource Loading Times (average):", sprintf("%.2f", average_loading_time)))

#Metric 15: average time spent per visitor
average_time_per_visitor <- mean(data$time_diff, na.rm = TRUE)
print(paste("Average Time Spent per Visitor:", sprintf("%.2f", average_time_per_visitor), "seconds"))

#Metric 15: average time spent per session
unique_sessions <- length(unique(data$session))
average_time_per_session <- total_time_on_site / unique_sessions
print(paste("Average Time Spent per Visitor per session:", sprintf("%.2f", average_time_per_session), "seconds"))

# Metric: Direct Traffic
direct_traffic <- sum(grepl("^\\s*-$", data$referer, perl = TRUE, ignore.case = TRUE))
print(paste("Direct Traffic:", direct_traffic, "users"))

# Metric: Average Time Spent per Page
average_time_per_page <- mean(data$time_diff, na.rm = TRUE)
print(paste("Average Time Spent per Page:", sprintf("%.2f", average_time_per_page), "seconds"))
