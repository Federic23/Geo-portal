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















if (!require(ApacheLogProcessor)) {
  install.packages("ApacheLogProcessor")
}

library(ApacheLogProcessor)
###### path <- "//Users/paulaareco/Desktop/ORT/tesis/Geo-portal/Logs/access.log.9"
path <- "C:/Users/Usuario/Desktop/Geo-portal-procesamiento-en-r/Geo-portal-procesamiento-en-r/Logs/access.log.8"

log_data <- read.apache.access.log(path,num_cores=2)

#imprime log data
str(log_data)

write.csv(log_data, "C:/Users/Usuario/Desktop/Geo-portal-procesamiento-en-r/Geo-portal-procesamiento-en-r/Logs/outputPlano.csv", row.names = FALSE)


#df5 = read.apache.access.log(path, columns=c("ip", "url", "datetime"))
#str(df5)

write.csv(df5, "C:/Users/Usuario/Desktop/Geo-portal-procesamiento-en-r/Geo-portal-procesamiento-en-r/Logs/output.csv", row.names = FALSE)

# Filter out crawlers based on user_agent
log_data <- log_data[!grepl("bot|crawler", log_data$useragent, ignore.case = TRUE), ]

# Print log data
str(log_data)

# Write log data to a CSV file
write.csv(log_data, "C:/Users/Usuario/Desktop/Geo-portal-procesamiento-en-r/Geo-portal-procesamiento-en-r/Logs/outputPlano.csv", row.names = FALSE)



#### Crawlers

# Filter out crawlers based on user_agent --- HAY QUE VER QUE PALABRAS CLAVES HAY , EL STRING DE ESTHER
log_data <- log_data[!grepl("bot|crawler|spider", log_data$useragent, ignore.case = TRUE), ]

# Print log data
str(log_data)

# Write log data to a CSV file
write.csv(log_data, "C:/Users/Usuario/Desktop/Geo-portal-procesamiento-en-r/Geo-portal-procesamiento-en-r/Logs/outputPlano.csv", row.names = FALSE)

#### Juntar/Ordenar por ip - Ord Fecha

log_data_ordered <- log_data[order(log_data$ip, log_data$datetime), ]

str(log_data_ordered)

write.csv(log_data_ordered, "C:/Users/Usuario/Desktop/Geo-portal-procesamiento-en-r/Geo-portal-procesamiento-en-r/Logs/ordered_output.csv", row.names = FALSE)

#### Identificar Agentes

# Filter out crawlers based on user_agent
log_data <- log_data[!grepl("bot|crawler", log_data$useragent, ignore.case = TRUE), ]

# Identify unique agents using distinct
unique_agents <- distinct(log_data, useragent)

# Print the unique agents
print(unique_agents$useragent)

#### Identificar Sesiones









