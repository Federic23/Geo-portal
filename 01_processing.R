# Install the necessary packages if not already installed
install.packages(c("dplyr", "lubridate"))

# Load the required libraries
library(dplyr)
library(lubridate)


# Read the CSV file
output_path <- file.path("TestCases", "output_with_session.csv")
data <- read.csv(output_path)

# Convert datetime column to POSIXct format
data$datetime <- as.POSIXct(data$datetime, format = "%Y-%m-%d %H:%M:%S")

# Calculate the time difference between consecutive rows
data$time_diff <- c(0, diff(data$datetime))

# Identify session changes based on IP-Agent tuple and 15-minute threshold
session_change <- c(TRUE, (data$ip[-1] != data$ip[-length(data$ip)]) | 
                      (data$useragent[-1] != data$useragent[-length(data$useragent)]) | 
                      (data$time_diff[-1] > 900))

# Remove missing or incomplete rows
data <- data[complete.cases(data), ]

# Assign session IDs based on session changes
data <- data %>%
  mutate(session_id = cumsum(session_change))

# Calculate the mean time of the session
mean_session_time <- data %>%
  group_by(session_id) %>%
  summarise(mean_time = mean(time_diff))

# Draw a plot of mean session times
plot(mean_session_time$session_id, mean_session_time$mean_time, type = "l",
     main = "Mean Session Time", xlab = "Session ID", ylab = "Mean Time (seconds)")

# Metric 13: average time spent (mean time among all visitors)
overall_mean_time <- mean(mean_session_time$mean_time) # Calculate the overall mean session time
cat("Overall mean time: ", overall_mean_time, "\n")


# Calculate the mean time and lines per session
session_summary <- data %>%
  group_by(session_id) %>%
  summarise(mean_time = mean(time_diff),
            lines_per_session = n())

# Calculate the average lines per session
average_lines_per_session <- mean(session_summary$lines_per_session)

# Draw a plot of mean session times
plot(session_summary$session_id, session_summary$mean_time, type = "l",
     main = "Mean Session Time", xlab = "Session ID", ylab = "Mean Time (seconds)")

# Print the average lines per session
cat("Average Lines per Session: ", average_lines_per_session, "\n")

