data <- read_csv("/Users/kimzippo/Desktop/KNU/3rdYear/Machine/Dataset/laptop_prices.csv")
df <- data.frame(
  CPU_company = data$CPU_company,
  Price_euros = data$Price_euros
)

set_a <- subset(df, CPU_company =="AMD")$Price_euros
set_b <- subset(df, CPU_company =="Intel")$Price_euros

result <- var.test(set_a, set_b)
print(result)


var.test(set_a, set_b, alternative = "less")
var.test(set_a, set_b, alternative = "greater")