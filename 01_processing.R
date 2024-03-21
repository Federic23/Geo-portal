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

