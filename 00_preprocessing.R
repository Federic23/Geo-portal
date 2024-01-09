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

# Load Config ------------------------------------------------------

## Config File

json_path <- file.path("config.json")

json_content <- readLines(json_path, warn = FALSE)
config <- fromJSON(paste(json_content, collapse = ""))

logPath <- config$logPath
crawlersPath <- config$crawlersPath

log_data <- read.apache.access.log(logPath,num_cores=2)

## Crawlers file

CrawlersPattern_path <- file.path("pattern.txt")
CrawlersPattern <- readLines(CrawlersPattern_path)
Crawlerspattern <- paste(CrawlersPattern, collapse = "|")
log_data <- log_data[!grepl(Crawlerspattern, log_data$useragent, ignore.case = TRUE), ]

# Load data --------------------------------------------------------

input_file <- "/Users/paulaareco/Desktop/ORT/tesis/Geo-portal/Logs/access.log.8"

#read file lines
input_data <- readLines(input_file) 

#main list, contains ips and each ips corresponding line
ip_logs <- list() 

#regex to find lines ip (ipv4 format)
ip_pattern <- "\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}" 

# Transform log ----------------------------------------------------

for (line in input_data) { 
    ip <- regmatches(line, regexpr(ip_pattern, line))
    
     #if an ip is found
     if (length(ip) > 0) { 
     
        #first ips element, it is assumed that for each line there is only one ip as regmatches returns a vectors list
        ip <- ip[[1]] 
         
        #checks if the ip is already on the list, names returns the ip with the matching name that is in the list
        if (ip %in% names(ip_logs)) { 
             # in the ips position, the log line is added
             ip_logs[[ip]] <- c(ip_logs[[ip]], line) 
         } else {
            #if not, a list with the new ip is added, and the line is added to it
            ip_logs[[ip]] <- list(line) 
        }
     }
}

# Proccess data -------------------------------------------------------------

#### prints log data #####

output_path <- file.path("TestCases", "outputPlano.csv") 

write.csv(log_data, output_path, row.names = FALSE)

df5 = read.apache.access.log(logPath, columns=c("ip", "url", "datetime"))
str(df5)

output_path <- file.path( "TestCases", "outputPlano.csv") 
write.csv(df5, output_path, row.names = FALSE)

## Filter Crawlers accounts 

### Write log data to a CSV file
output_path <- file.path("TestCases", "outputPlano.csv")
write.csv(log_data, output_path, row.names = FALSE)

## Group by IP and order by data

log_data_ordered <- log_data[order(log_data$ip, log_data$datetime), ]

str(log_data_ordered)

output_path <- file.path("TestCases", "ordered_output.csv")
write.csv(log_data_ordered, output_path , row.names = FALSE)

## Identify Agents

CrawlersPattern_path <- file.path("pattern.txt")
CrawlersPattern <- readLines(CrawlersPattern_path)
Crawlerspattern <- paste(CrawlersPattern, collapse = "|")
log_data <- log_data[!grepl(Crawlerspattern, log_data$useragent, ignore.case = TRUE), ]


### Create a new column for unique identifier (IP + agent)
log_data$unique_id <- paste(log_data$ip, log_data$useragent, sep = "_")


### Identify sessions based on time difference
log_data$datetime <- as.POSIXct(log_data$datetime, format = "%d/%b/%Y:%H:%M:%S", tz = "UTC")
log_data <- log_data %>%
  group_by(unique_id) %>%
  mutate(time_diff = difftime(datetime, lag(datetime, default = first(datetime)), units = "mins"),
         session = cumsum(ifelse(is.na(time_diff) | time_diff > 15, 1, 0))) %>%
  ungroup()

### Format session numbers as "session0", "session1", etc.
log_data$session <- paste0("session", log_data$session)

### Print the updated log_data with the session column
print(log_data)

### Write the updated log_data to a CSV file
output_path <- file.path("TestCases", "output_with_session.csv")
write.csv(log_data, output_path, row.names = FALSE)
