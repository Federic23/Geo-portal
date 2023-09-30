#install.packages("ApacheLogProcessor")
library(ApacheLogProcessor)


input_file <- "/Users/paulaareco/Desktop/ORT/tesis/Geo-portal/logsejemplo.txt"

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

#funcion encargada de ordenarlos por hora
hora_log <- function(line) {
  hour_regex <- "\\[(\\d{2}/\\w+/(\\d{4}|\\d{2}):(\\d{2}:\\d{2}:\\d{2}) -\\d{4})\\]"
  matches <- regmatches(line, regexpr(hour_regex, line))
  if (length(matches) > 0) {
    hour <- gsub("\\[|\\]", "", matches[1])
    return(hour)
  } else {
    return(NULL)
  }
}

ip_logs_sorted_by_hour <- list()
for (ip in names(ip_logs)) {
  logs <- ip_logs[[ip]]
  sorted_logs <- logs[order(sapply(logs, hora_log))]
  ip_logs_sorted_by_hour[[ip]] <- sorted_logs
}


#una vez que tengo el archivo ordenado, puedo hacer cosas con las lineas de cada ip
for (ip in names(ip_logs_sorted_by_hour)) {


}


for (ip in names(ip_logs_sorted_by_hour)) {
  cat("IP:", ip, "\n")
  cat("Entradas de log ordenadas por hora:\n")
  for (line in ip_logs_sorted_by_hour[[ip]]) {
    cat(line, "\n")
  }
  cat("\n")
}


###############################################################################################################################

library(ApacheLogProcessor)
path <- "//Users/paulaareco/Desktop/ORT/tesis/Geo-portal/Logs/access.log.9"
log_data <- read.apache.access.log(path,num_cores=2)

#imprime log data
str(log_data)

df5 = read.apache.access.log(path, columns=c("ip", "url", "datetime"))
str(df5)





