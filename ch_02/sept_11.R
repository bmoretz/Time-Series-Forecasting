library(data.table)
library(XLConnect)
library(forecast)
library(xts)

source.path <- "datasets/Sept11Travel.xls"
travel.wb <- loadWorkbook(source.path)

travel.data <- as.data.table(readWorksheet(travel.wb, "Sheet1"))
colnames(travel.data) <- c("Date", "Air", "Rail", "Auto")

travel.data$Date <- as.Date(travel.data$Date)
travel.data$Air <- as.numeric(travel.data$Air)
travel.data$Rail <- as.numeric(travel.data$Rail)
travel.data$Auto <- as.numeric(travel.data$Auto)

travel.data <- xts(travel.data, travel.data$Date)

rm(source.path)
rm(travel.wb)

head(travel.data)

min(travel.data$Date)
max(travel.data$Date)

# Travel Data
par(mfrow = c(3, 1))

airline.ts <- ts(travel.data$Air, start = c(1990, 1), end = c(2004, 1), frequency = 12)
airline.lm <- tslm(airline.ts ~ trend + I(trend ^ 2))

plot(airline.ts, xlab = "Time", ylab = "Airline Travel", bty = "l")
lines(airline.lm$fitted, lwd = 2)

rail.ts <- ts(travel.data$Rail, start = c(1990, 1), end = c(2004, 1), frequency = 12)
rail.lm <- tslm(rail.ts ~ trend + I(trend ^ 2))

plot(rail.ts, xlab = "Time", ylab = "Rail Travel", bty = "l")
lines(rail.lm$fitted, lwd = 2)

auto.ts <- ts(travel.data$Auto, start = c(1990, 1), end = c(2004, 1), frequency = 12)
auto.lm <- tslm(auto.ts ~ trend + I(trend ^ 2))

plot(auto.ts, xlab = "Time", ylab = "Auto Travel", bty = "l")
lines(auto.lm$fitted, lwd = 2)

# Yearly Avg. (ignore seasonality)

ep <- endpoints(travel.data, on = "years")
travel.yearly <- period.apply(travel.data, ep, mean)

par(mfrow = c(3, 1))
plot(travel.yearly$Air)
plot(travel.yearly$Rail)
plot(travel.yearly$Auto)

# Log scale


travel.data$AirLog <- log(travel.data$Air)
travel.data$RailLog <- log(travel.data$Rail)
travel.data$AutoLog <- log(travel.data$Auto)

par(mfrow = c(3, 1))

airline.log.ts <- ts(travel.data$AirLog, start = c(1990, 1), end = c(2004, 1), frequency = 12)
airline.log.lm <- tslm(airline.log.ts ~ trend + I(trend ^ 2))

plot(airline.log.ts, xlab = "Time", ylab = "Airline Travel", bty = "l")
lines(airline.log.lm$fitted, lwd = 2)

rail.log.ts <- ts(travel.data$RailLog, start = c(1990, 1), end = c(2004, 1), frequency = 12)
rail.log.lm <- tslm(rail.log.ts ~ trend + I(trend ^ 2))

plot(rail.log.ts, xlab = "Time", ylab = "Rail Travel", bty = "l")
lines(rail.log.lm$fitted, lwd = 2)

auto.ts <- ts(travel.data$Auto, start = c(1990, 1), end = c(2004, 1), frequency = 12)
auto.lm <- tslm(auto.ts ~ trend + I(trend ^ 2))

plot(auto.ts, xlab = "Time", ylab = "Auto Travel", bty = "l")
lines(auto.lm$fitted, lwd = 2)
