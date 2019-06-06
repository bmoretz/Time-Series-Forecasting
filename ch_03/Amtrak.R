library(forecast)

Amtrak.data <- read.csv("datasets/Amtrak data.csv")
par(mfrow = c(1,1))
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)
plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")

ridership.lm <- tslm(ridership.ts ~ trend + I(trend ^ 2))

par(mfrow = c(2, 1))
plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")
lines(ridership.lm$fitted.values, lwd = 2)
ridership.ts.zoom <- window(ridership.ts, start = c(1997, 1), end = c(2000, 12))
plot(ridership.ts.zoom, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")

nValid <- 36
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))
ridership.lm <- tslm(train.ts ~ trend + I(trend ^ 2))
ridership.lm.pred <- forecast(ridership.lm, h = nValid, level = 95)

par(mfrow = c(2,1))

plot(ridership.lm.pred, ylim = c(1300, 2600), ylab = "Ridership", xlab = "Time", bty = "l",
     xaxt = "n", xlim = c(1991, 2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ridership.lm$fitted, lwd = 2)
lines(valid.ts)

hist(ridership.lm.pred$residuals, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")

accuracy(ridership.lm.pred$mean, valid.ts)

fixed.nValid <- 36
fixed.nTrain <- length(ridership.ts) - fixed.nValid
stepsAhead <- 1
error <- rep(0, fixed.nValid - stepsAhead + 1)
percent.error <- rep(0, fixed.nValid - stepsAhead + 1)

for (j in fixed.nTrain:(fixed.nTrain + fixed.nValid - stepsAhead)) {
  train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, j))
  valid.ts <- window(ridership.ts, start = c(1991, j + stepsAhead), end = c(1991, j + stepsAhead))
  naive.pred <- naive(train.ts, h = stepsAhead)
  error[j - fixed.nTrain + 1] <- valid.ts - naive.pred$mean[stepsAhead]
  percent.error[j - fixed.nTrain + 1] <- error[j - fixed.nTrain + 1] / valid.ts
}

mean(abs(error))
sqrt(mean(error ^ 2))
mean(abs(percent.error))