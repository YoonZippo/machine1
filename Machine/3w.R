library(magrittr)

data <- c(1, 10, 15, 20, 25, 30, 15, 20, 25, 25, 10, 40)
range_value <- max(data) - min(data)
df <- data.frame(values = c(5, 7, 10, 12, 14, 18, 20, 22, 25, 27, 30, 40))
Q1 <- quantile(data, 0.25)
Q3 <- quantile(data, 0.75)
iqr_value <- Q3 - Q1



data <- c( 20, 25, 20, 25, 25)
mean_data <- mean(data)
squared_diff <- (data - mean_data)^2
variance <- sum(squared_diff) / length(data)
std_dev <- sqrt(variance)

gc()


city <- c("Seoul", "Busan", "Daegu", "Seoul", "Busan", "Daegu", "Ulsan")
pm25 <- c(18, 21, 21, 17, 8, 11, 25)
df <- data.frame(city = city, pm25 = pm25)

write.csv(df, "/Users/kimzippo/Desktop/Code/2025/Machine/data.csv", row.names=TRUE)


data <- read.csv("/Users/kimzippo/Desktop/Code/2025/Machine/data.csv")

