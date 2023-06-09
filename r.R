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