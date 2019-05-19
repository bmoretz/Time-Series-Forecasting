library(forecast)
library(zoo)

Amtrak.data <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)

nValid <- 36
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))

train.lm.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)
train.res.arima <- Arima(train.lm.trend.season$residuals, order = c(1,0,0))
train.res.arima.pred <- forecast(train.res.arima, h = nValid)

# Figure 7-4
plot(train.lm.trend.season$residuals, ylim = c(-250, 250),  ylab = "Residuals", 
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train.res.arima.pred$fitted, lwd = 2, col = "blue")
lines(c(2004.25 - 3, 2004.25 - 3), c(-500, 3500))
lines(c(2004.25, 2004.25), c(-500, 3500))
text(1996.25, 225, "Training")
text(2002.75, 225, "Validation")
text(2005.25, 225, "Future")
arrows(2004 - 3, 200, 1991.25, 200, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5 - 3, 200, 2004, 200, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5, 200, 2006, 200, code = 3, length = 0.1, lwd = 1, angle = 30)
dev.off()

summary(train.res.arima)

# Figure 7-5
Acf(train.res.arima$residuals, lag.max = 12, main = "")
