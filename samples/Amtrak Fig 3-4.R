library(forecast)

Amtrak.data <- read.csv("datasets/Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)

stepsAhead <- 36
nTrain <- length(ridership.ts) - stepsAhead
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + stepsAhead))
ridership.lm <-  tslm(train.ts ~ trend + I(trend^2))
ridership.lm.pred <- forecast(ridership.lm, h = stepsAhead, level = 0)
names(ridership.lm.pred)

# Figure 3-4
hist(ridership.lm.pred$residuals,  ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")
