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

ggplot(average_by_day, aes(x = Day, y = AverageRequests)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Day of the Week") +
  ylab("Average Requests") +
  ggtitle("Average Requests per Day of the Week") +
  theme_minimal()

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
unique_sessions <- length(unique(df$session))
total_sessions <- nrow(df)
bounce_rate <- (total_sessions - unique_sessions) / total_sessions * 100
cat('Bounce Rate:', sprintf('%.2f%%', bounce_rate), '\n')
