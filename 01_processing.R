install.packages(c("dplyr", "lubridate"))

library(dplyr)
library(lubridate)

process_log_data <- function(filePath) {
  requiredPackages <- c("dplyr", "lubridate")
  newPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
  if(length(newPackages)) install.packages(newPackages)
  lapply(requiredPackages, library, character.only = TRUE)
  
  # Reading the CSV file
  data <- read.csv(filePath)
  
  # Pre-processing steps
  data <- data[complete.cases(data), ]
  data$datetime <- as.POSIXct(data$datetime, format = "%Y-%m-%d %H:%M:%S")
  
  # Calculating time difference between consecutive rows
  data <- data %>%
    arrange(ip, useragent, datetime) %>%
    mutate(time_diff = c(0, diff(datetime)))
  
  # Identifying session changes
  session_change <- c(TRUE, (data$ip[-1] != data$ip[-length(data$ip)]) | 
                        (data$useragent[-1] != data$useragent[-length(data$useragent)]) | 
                        (data$time_diff > 900))
  
  # Assigning session IDs
  data$session_id <- cumsum(session_change)
  
  return(data)
}



# # Reading the CSV file
# output_path <- file.path("TestCases", "output_with_session.csv")
# data <- read.csv(output_path)
# 
# # Converting datetime column to POSIXct format
# data$datetime <- as.POSIXct(data$datetime, format = "%Y-%m-%d %H:%M:%S")
# 
# # Calculating the time difference between consecutive rows
# data$time_diff <- c(0, diff(data$datetime))
# 
# # Identifying session changes based on IP-Agent tuple and 15-minute threshold
# session_change <- c(TRUE, (data$ip[-1] != data$ip[-length(data$ip)]) | 
#                       (data$useragent[-1] != data$useragent[-length(data$useragent)]) | 
#                       (data$time_diff[-1] > 900))
# 
# # Removing missing or incomplete rows
# data <- data[complete.cases(data), ]
# 
# # Assigning session IDs based on session changes
# data <- data %>%
#   mutate(session_id = cumsum(session_change))
# 
# # Calculating the mean time of the session
# mean_session_time <- data %>%
#   group_by(session_id) %>%
#   summarise(mean_time = mean(time_diff))
# 
# # Metric: average time spent (mean time among all visitors)
# overall_mean_time <- mean(mean_session_time$mean_time) # Calculate the overall mean session time
# cat("Overall mean time: ", overall_mean_time, "\n")
# 
# # Calculate the mean time and lines per session
# session_summary <- data %>%
#   group_by(session_id) %>%
#   summarise(mean_time = mean(time_diff),
#             lines_per_session = n())
# 
# # Calculate the average lines per session
# average_lines_per_session <- mean(session_summary$lines_per_session)
# 
# # Draw a plot of mean session times
# plot(session_summary$session_id, session_summary$mean_time, type = "l",
#      main = "Mean Session Time", xlab = "Session ID", ylab = "Mean Time (seconds)")
# 
# # Print the average lines per session
# cat("Average Lines per Session: ", average_lines_per_session, "\n")