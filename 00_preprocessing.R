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
      if (nzchar(path)) {
        selectedFilePath(path)
        
        # appendFilePathToLog(path)
        print("0")
        df5 <- read_and_print_log(path)
        print("1")
        # df5 <- filter_crawlers(df5)
        # print("2")
        # df5 <- order_log_data(df5)
        # print("3")
        # df5 <- identify_sessions(df5)
        # print("4")
        # dataForMetrics(df5)
        # print("5")
        # write_log_to_csv(df5, "identify_sessions.csv")
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

transform_log_to_df <- function(logPath) {
  df <- read.apache.access.log(logPath, columns = c("ip", "url", "datetime", "httpcode", "useragent", "referer"), num_cores=2)
  str(df)
  return(df)
}

read_and_print_log <- function(logPath) {
  result <- tryCatch({
    print("try")
    read_sort_and_write_logs(logPath)
    df <- transform_log_to_df(logPath)
    return(df)
  }, error = function(e) {
    print("catch")
    fix_log_format(logPath)
    print("fix_log_format_out")
    read_sort_and_write_logs(logPath)
    print("read_sort_and_write_logs_out")
    df <- transform_log_to_df(logPath)
    return(df)
  })
  
  print("ENDING")
  return(result)
}

fix_log_format <- function(logPath) {
  temp_path <- tempfile()
  
  # Open the original file for reading and the temporary file for writing
  file_conn <- file(logPath, "r")
  temp_conn <- file(temp_path, "w")
  
  print("enter_fix_log_format")
  while(TRUE) {
    line <- readLines(file_conn, n = 1, warn = FALSE)
    if(length(line) == 0) break # Exit loop if end of file
    
    # Remove the first quote
    line <- sub('"', '', line, fixed = TRUE)
    
    # Reversing the string to handle the last double quote uniquely
    line_rev <- stringi::stri_reverse(line)
    
    # Replace the first occurrence of double quotes in the reversed line (which corresponds to the last in the original)
    # Then reverse it back before final write, ensuring we only do this if there's at least one pair of double quotes
    if (grepl('""', line_rev)) {
      line_rev <- sub('""', '##TEMP##', line_rev, fixed = TRUE) # Temporarily mark the last double quote
      line_rev <- gsub('""', '"', line_rev, fixed = TRUE) # Replace all other double quotes with single quotes
      line_rev <- sub('##TEMP##', '""', line_rev, fixed = TRUE) # Restore the last double quote
      line <- stringi::stri_reverse(line_rev)
    } else {
      line <- gsub('"', "'", line, fixed = TRUE) # If no double quotes, just replace normally
    }
    
    writeLines(line, temp_conn) # Write to temp file
  }
  print("exit_fix_log_format")
  
  close(file_conn)
  close(temp_conn)
  
  # Replace the original file with the temporary file
  file.rename(temp_path, logPath)
}

read_sort_and_write_logs <- function(logPath) {
  # Read the entire log file into a vector, each line as an element
  
  print("enter_read_sort_and_write_logs")
  logEntries <- readLines(logPath)
  
  # Extract datetime strings from the log entries
  datetimeStrings <- regmatches(logEntries, gregexpr("\\[\\d{2}/\\w+/\\d{4}:\\d{2}:\\d{2}:\\d{2} -\\d{4}\\]", logEntries))
  
  # Convert the datetime strings to POSIXct objects
  datetimes <- as.POSIXct(strptime(datetimeStrings, format = "[%d/%b/%Y:%H:%M:%S %z]"))
  
  # Order the logEntries by datetime
  orderedLogs <- logEntries[order(datetimes)]
  
  # Trim the log to the first 200,000 lines if it exceeds that number
  print("enter_cut_length")
  if(length(orderedLogs) > 200000) {
    orderedLogs <- orderedLogs[1:200000]
  }
  
  # Write the ordered logs back to the original file
  print("writing_read_sort_and_write_logs")
  writeLines(orderedLogs, logPath)
}


order_log_data <- function(data) {
  ordered_data <- data[order(data$ip, data$datetime), ]
  return(ordered_data)
}

filter_crawlers <- function(data, patternPath = "../pattern.txt") {
  CrawlersPattern <- readLines(patternPath)
  Crawlerspattern <- paste(CrawlersPattern, collapse = "|")
  print(Crawlerspattern)
  filtered_data <- data[!grepl(Crawlerspattern, data$useragent, ignore.case = TRUE), ]
  return(filtered_data)
}

identify_sessions <- function(data) {
  data$datetime <- as.POSIXct(data$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  data$unique_id <- paste(data$ip, data$useragent, sep = "_")
  
  data$session <- 0
  # Initialize time_diff with NA or zeros; NA might be more indicative of "not applicable"
  data$time_diff <- rep(NA, nrow(data))
  
  current_session <- 1
  data$session[1] <- current_session
  data$time_diff[1] <- 0 # Assuming the first entry of the dataset does not have a predecessor
  
  for (i in 2:nrow(data)) {
    # Calculate time difference only within the same unique_id
    if (data$unique_id[i] == data$unique_id[i-1]) {
      data$time_diff[i] <- as.numeric(difftime(data$datetime[i], data$datetime[i-1], units = "secs"))
    } else {
      data$time_diff[i] <- 0 # Reset for the first entry of a new unique_id
    }
    
    # Check if unique_id has changed or time_diff is greater than 900 seconds (15 minutes)
    if (data$unique_id[i] != data$unique_id[i-1] || data$time_diff[i] > 900) {
      current_session <- current_session + 1
      data$time_diff[i] <- 0 # Reset for the first entry of a new session
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
