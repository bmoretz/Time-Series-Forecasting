library(forecast)

Amtrak.data <- read.csv("datasets/Amtrak data.csv")

ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), frequency = 12)

fixed.nValid <- 36
fixed.nTrain <- length(ridership.ts) - fixed.nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, fixed.nTrain))
valid.ts <- window(ridership.ts, start = c(1991, fixed.nTrain + 1),
                   end = c(1991, fixed.nTrain + fixed.nValid))
naive.pred <- naive(train.ts, h = fixed.nValid)
snaive.pred <- snaive(train.ts, h = fixed.nValid)
accuracy(naive.pred, valid.ts)
accuracy(snaive.pred, valid.ts)

