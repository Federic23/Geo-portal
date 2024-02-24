############ New version
# Load Packages --------------------------------------------------------

if (!require(jsonlite)) {
  install.packages("jsonlite")
}

if (!require(ApacheLogProcessor)) {
  install.packages("ApacheLogProcessor")
}

if (!require(dplyr)) {
  install.packages("dplyr")
}

library(jsonlite)
library(ApacheLogProcessor)
library(dplyr)

fileChooserOpen <- reactiveVal(FALSE)
selectedFilePath <- reactiveVal(NULL)
dataForMetrics <- reactiveVal(NULL)

printPath <- function() {
  if (!fileChooserOpen()) {
    fileChooserOpen(TRUE) # Set the state to open

    tryCatch({
      path <- file.choose()
      if (nzchar(path)) { # Check if path ends with .csv
        print(path)
        selectedFilePath(path)
        # appendFilePathToLog(path)
        df5 <- read_and_print_log(path)
        df5 <- filter_crawlers(df5)
        df5 <- order_log_data(df5)
        df5 <- identify_sessions(df5)
        dataForMetrics(df5)
        write_log_to_csv(df5, "identify_sessions.csv")
      } else {
        if (nzchar(path)) {
          print("Selected file is not a CSV.")
        } else {
          print("No file selected.")
        }
      }
    }, error = function(e) {
      cat(paste("File selection was cancelled.\nError message:", e$message))
    })

    fileChooserOpen(FALSE) # Reset the state to closed
  }
}

read_and_print_log <- function(logPath) {
  df <- read.apache.access.log(logPath, columns = c("ip", "url", "datetime", "useragent"))
  str(df)
  return(df)
}

order_log_data <- function(data) {
  ordered_data <- data[order(data$ip, data$datetime), ]
  return(ordered_data)
}

filter_crawlers <- function(data, patternPath = "../pattern.txt") {
  CrawlersPattern <- readLines(patternPath)
  Crawlerspattern <- paste(CrawlersPattern, collapse = "|")
  filtered_data <- data[!grepl(Crawlerspattern, data$useragent, ignore.case = TRUE), ]
  return(filtered_data)
}

identify_sessions <- function(data) {
  data$datetime <- as.POSIXct(data$datetime, format = "%d/%b/%Y:%H:%M:%S", tz = "UTC")
  
  data$unique_id <- paste(data$ip, data$useragent, sep = "_")
  
  data$session <- 0
  data$time_diff <- c(0, diff(data$datetime))
  
  current_session <- 1
  data$session[1] <- current_session
  
  for (i in 2:nrow(data)) {
    # Check if unique_id has changed or time_diff is greater than 15 minutes
    if (data$unique_id[i] != data$unique_id[i-1] || data$time_diff[i] > 900) {
      current_session <- current_session + 1
    }
    data$session[i] <- current_session
  }
  
  data$session <- paste0("session", data$session)
  
  return(data)
}

write_log_to_csv <- function(data, filename, directory = "../TestCases") {
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  output_path <- file.path(directory, filename)
  write.csv(data, output_path, row.names = FALSE)
}
