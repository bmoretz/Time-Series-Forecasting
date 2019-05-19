library(forecast)
library(zoo)

Amtrak.data <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)

nValid <- 36
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))

train.lm.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# Figure 7-3
Acf(train.lm.trend.season$residuals, lag.max = 12, main = "")

Acf(train.lm.trend.season$residuals, lag.max = 12, main = "")$acf
# At lag-1 the acf is 0.60405883.