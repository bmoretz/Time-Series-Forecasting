library(forecast)
library(zoo)

Amtrak.data <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)
ridership.24.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, 24))

# Figure 7-2
Acf(ridership.24.ts, lag.max = 12, main = "")

