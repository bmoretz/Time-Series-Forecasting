library("forecast")

walmart.data <- read.csv("WalMartStock.csv")
walmart.data$Date <- as.Date(walmart.data$Date, format="%d-%b-%y")
close.ts <- ts(walmart.data$Close)

# Figure 7.10
par(mfrow = c(1,1))
plot(walmart.data$Date, walmart.data$Close, type = "l", ylab = "Close Price ($)", xlab = "Time", xaxt = "n")
axis.Date(side = 1, walmart.data$Date, format = "%b-%y")

# Figure 7.11
par(mfrow = c(1,2))
Acf(close.ts, lag.max = 10, main = "ACF Plot for Close")
Acf(diff(close.ts,1), lag.max = 10, main = "ACF Plot for Differenced Series")

# Table 7.4
Arima(close.ts, order = c(1,0,0))
Arima(diff(close.ts,1), order = c(1,0,0))
