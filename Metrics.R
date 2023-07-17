install.packages(c("ggplot2"))
library(ggplot2)


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

#Metric 4: 404 errors
errors_404 <- subset(data, httpcode == 404)
cat("404 Errors:\n")
cat(errors_404$url, "\n", sep = "\n")

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