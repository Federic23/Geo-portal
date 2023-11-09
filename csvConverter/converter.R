# Load the CSV file
csv_data <- read.csv("filebeat-geoportal-access.csv")

# Open a log file for writing
log_file <- file("output.log", "w")

# Write data to the log file
cat("This is the header of the log file\n", file = log_file)

# Loop through each row of the CSV data and write it to the log file
for (i in 1:nrow(csv_data)) {
  row <- csv_data[i, ]
  cat("Row ", i, ": ", row$Column1, " - ", row$Column2, "\n", file = log_file)
}

# Close the log file
close(log_file)

# Inform the user that the conversion is complete
cat("CSV to log file conversion is complete.\n")