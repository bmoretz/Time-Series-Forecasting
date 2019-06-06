library(forecast)
library(zoo)

Amtrak.data <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)

# Figure 5-4
par(mfrow = c(2, 2))
plot(ridership.ts, ylab = "Ridership", xlab = "Time", bty = "l", xlim = c(1991, 2004.25), main = "Ridership")
plot(diff(ridership.ts, lag = 12), ylab = "Lag-12", xlab = "Time", bty = "l", xlim = c(1991, 2004.25), main = "Lag-12 Difference")
plot(diff(ridership.ts, lag = 1), ylab = "Lag-1", xlab = "Time", bty = "l", xlim = c(1991, 2004.25), main = "Lag-1 Difference")
plot(diff(diff(ridership.ts, lag = 12), lag = 1), ylab = "Lag-12, then Lag-1", xlab = "Time", bty = "l", xlim = c(1991, 2004.25), main = "Twice-Differenced (Lag-12, Lag-1)")
dev.off()

ridership.deseasonalized <- diff(ridership.ts, lag = 12)
summary(tslm(ridership.deseasonalized ~ trend))
