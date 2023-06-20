if (!require(dplyr)) {
  install.packages("dplyr")
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
}

library(dplyr)
library(ggplot2)

file_path <- "C:/Users/Usuario/Documents/Geo-portal/exam-scores.csv"

data <- read.csv(file_path)


summary <- data %>%
  summarize(mean_score = mean(Score),
            sd_score = sd(Score))

cat("Mean Score:", summary$mean_score, "\n")
cat("Standard Deviation:", summary$sd_score, "\n")

ggplot(data, aes(x = Score)) +
  geom_histogram(fill = "blue", color = "black") +
  labs(title = "Exam Scores", x = "Score", y = "Frequency")