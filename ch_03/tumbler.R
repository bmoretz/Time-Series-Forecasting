library(data.table)
library(forecast)

tumblr.data <- read.csv("datasets/Tumblr.csv")
people.ts <- ts(tumblr.data$People.Worldwide) / 1000000

people.ets.ANN <- ets(people.ts, model = "ANN") # Fit Model 1 to time series
people.ets.MMN <- ets(people.ts, model = "MNN", damped = F) # Fit Model #2
people.ets.MMdN <- ets(people.ts, model = "MMN", damped = T) # Fit Model #3

people.ets.ANN.pred <- forecast(people.ets.ANN, h = 115, level = c(0.2, 0.4, 0.6, 0.8))
people.ets.MMN.pred <- forecast(people.ets.MMN, h = 115, level = c(0.2, 0.4, 0.6, 0.8))
people.ets.MMdN.pred <- forecast(people.ets.MMdN, h = 115, level = c(0.2, 0.4, 0.6, 0.8))

par(mfrow = c(1, 3))
plot(people.ets.ANN.pred, xlab = "Month", ylab = "People (in millions)", ylim = c(0, 1000))
plot(people.ets.MMdN.pred, xlab = "Month", ylab = "People (in millions)", ylim = c(0, 1000))
plot(people.ets.MMdN.pred, xlab = "Month", ylab = "People (in millions)", ylim = c(0, 1000))
