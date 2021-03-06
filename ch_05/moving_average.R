library(forecast)
library(zoo)

Amtrak.data <- read.csv("datasets/Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), frequency = 12)

ma.trailing <- rollmean(ridership.ts, k = 12, align = "right")
ma.centered <- ma(ridership.ts, order = 12)

plot(ridership.ts, ylim = c(1300, 2200), ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n",
     xlim = c(1991, 2004.25), main = "")
axis(l, at = seq(1991, 2004.25, main = ""))
lines(ma.centered, lwd = 2)
lines(ma.trailing, lwd = 2, lty = 2)
legend(1994, 2200, c("Ridership", "Centered Moving Average", "Trailing Moving Average"), lty = c(1, 1, 2),
       lwd = c(1, 2, 2), bty = "n")

