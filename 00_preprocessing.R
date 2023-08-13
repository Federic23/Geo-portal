############ Nueva version


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

#json_path <- file.path("Geo-portal", "config.json")
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

#leo las lineas del archivo
input_data <- readLines(input_file) 

#seria la lista principal, en donde van las ip y las lineas de cada ip
ip_logs <- list() 

#regex para encontrar las ip en cada linea (formato ipv4)
ip_pattern <- "\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}" 

# Transform log ----------------------------------------------------

#recorro por cada linea
for (line in input_data) { 
    ip <- regmatches(line, regexpr(ip_pattern, line))
    
     #si encuentro una ip
     if (length(ip) > 0) { 
     
        #es el primer elemento de ip, porque por linea suponemos que va a haber solo una ip, porque regmatches devuelve una lista de vectores
        ip <- ip[[1]] 
         
        #me fijo si la ip ya esta en la lista, names devuelve las ip con ese nombre que esten en la lista
        if (ip %in% names(ip_logs)) { 
        
             #en la posicion de la ip, agrego la linea del log
             ip_logs[[ip]] <- c(ip_logs[[ip]], line) 
         } else {
         
            #sino creo una lista nueva con la ip que no estaba, y agrego esa linea
            ip_logs[[ip]] <- list(line) 
        }
     }
}

# Proccess data -------------------------------------------------------------

#### imprime log data #####

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





