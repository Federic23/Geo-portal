############ Nueva version
input_file <- "/Users/Usuario/Documents/Geo-portal/logsejemplo.txt"

#leo las lineas del archivo
input_data <- readLines(input_file) 

#seria la lista principal, en donde van las ip y las lineas de cada ip
ip_logs <- list() 

#regex para encontrar las ip en cada linea (formato ipv4)
ip_pattern <- "\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}" 

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

#una vez que tengo el archivo ordenado, puedo hacer cosas con las lineas de cada ip
for (ip in names(ip_logs)) {


}

crawler_ips <- c()
for (ip in names(ip_logs)) {
  user_agents <- unique(sapply(ip_logs[[ip]], function(line) {
    regmatches(line, regexpr("\"([^\"]+)\"", line))[[1]]
  }))
  
  # Check if any user agent or pattern suggests a crawler
  if (any(grepl("bot", user_agents, ignore.case = TRUE)) ||
      any(grepl("spider", user_agents, ignore.case = TRUE)) ||
      any(grepl("crawl", user_agents, ignore.case = TRUE)) ||
      any(grepl("wget", user_agents, ignore.case = TRUE))) {
    crawler_ips <- c(crawler_ips, ip)  # Add the IP address to the list of crawler IPs
  }
}



#si quiero imprimit para cer que hizo
for (ip in names(ip_logs)) {
  cat("IP:", ip, "\n")
  cat("Entradas de log:\n")
  for (line in ip_logs[[ip]]) {
    cat(line, "\n")
  }
  cat("\n")
}





############

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

############

json_path <- file.path("Geo-portal", "config.json")
json_content <- readLines(json_path, warn = FALSE)
config <- fromJSON(paste(json_content, collapse = ""))

logPath <- config$logPath
crawlersPath <- config$crawlersPath

print(logPath)
print(crawlersPath)

log_data <- read.apache.access.log(logPath,num_cores=2)

#### imprime log data #####

str(log_data)

output_path <- file.path("Geo-portal", "TestCases", "outputPlano.csv") 

write.csv(log_data, output_path, row.names = FALSE)


df5 = read.apache.access.log(logPath, columns=c("ip", "url", "datetime"))
str(df5)

output_path <- file.path("Geo-portal", "TestCases", "outputPlano.csv") 
write.csv(df5, output_path, row.names = FALSE)

#### Crawlers #####

CrawlersPattern_path <- file.path("Geo-portal", "pattern.txt")
CrawlersPattern <- readLines(CrawlersPattern_path)
Crawlerspattern <- paste(CrawlersPattern, collapse = "|")
log_data <- log_data[!grepl(Crawlerspattern, log_data$useragent, ignore.case = TRUE), ]

# Print log data
str(log_data)

# Write log data to a CSV file
output_path <- file.path("Geo-portal", "TestCases", "outputPlano.csv")
write.csv(log_data, output_path, row.names = FALSE)

#### Juntar/Ordenar por ip - Ord Fecha #####

log_data_ordered <- log_data[order(log_data$ip, log_data$datetime), ]

str(log_data_ordered)

output_path <- file.path("Geo-portal", "TestCases", "ordered_output.csv")
write.csv(log_data_ordered, output_path , row.names = FALSE)


#### Identificar Agentes #####

CrawlersPattern_path <- file.path("Geo-portal", "pattern.txt")
CrawlersPattern <- readLines(CrawlersPattern_path)
Crawlerspattern <- paste(CrawlersPattern, collapse = "|")
log_data <- log_data[!grepl(Crawlerspattern, log_data$useragent, ignore.case = TRUE), ]


# Create a new column for unique identifier (IP + agent)
log_data$unique_id <- paste(log_data$ip, log_data$useragent, sep = "_")


# Identify sessions based on time difference
log_data$datetime <- as.POSIXct(log_data$datetime, format = "%d/%b/%Y:%H:%M:%S", tz = "UTC")
log_data <- log_data %>%
  group_by(unique_id) %>%
  mutate(time_diff = difftime(datetime, lag(datetime, default = first(datetime)), units = "mins"),
         session = cumsum(ifelse(is.na(time_diff) | time_diff > 15, 1, 0))) %>%
  ungroup()

# Format session numbers as "session0", "session1", etc.
log_data$session <- paste0("session", log_data$session)

# Print the updated log_data with the session column
print(log_data)

# Write the updated log_data to a CSV file
output_path <- file.path("Geo-portal", "TestCases", "output_with_session.csv")
write.csv(log_data, output_path, row.names = FALSE)










# Print the updated log_data with the unique identifier
print(log_data)








# Write the updated log_data to a CSV file
output_path <- file.path("Geo-portal", "TestCases", "output_with_identifier.csv")
write.csv(log_data, output_path, row.names = FALSE)

#unique_ids <- unique(log_data$unique_id)

# Print the list of unique identifiers
#print(unique_ids)

#### Identificar Sesiones

# Subset log_data to include referrals different from "-"
#referrals <- log_data$referer[log_data$referer != "-"]

# Print the referrals
#print(referrals)






