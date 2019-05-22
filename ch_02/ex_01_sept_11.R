library(data.table)
library(XLConnect)
library(forecast)
library(xts)

convert_col_types <- function(dt, cols, FUN) {

  assertive::is_data.table(dt)

  dt[, (cols) := lapply(.SD, FUN), .SDcols = cols][]
}

as.char = function(x, na.strings = c("NA", "NULL")) {
  na = x %in% na.strings
  x[na] = 0
  x = as.character(x)
  x[na] = NA_character_
  x
}

as.num = function(x, na.strings = c("NA", "NULL")) {
  na = x %in% na.strings
  x[na] = 0
  x = as.numeric(x)
  x[na] = NA_real_
  x
}

source.path <- "datasets/Sept11Travel.xls"
travel.wb <- loadWorkbook(source.path)

travel.data <- as.data.table(readWorksheet(travel.wb, "Sheet1"))
colnames(travel.data) <- c("Date", "Air", "Rail", "Auto")

travel.data$Date <- as.Date(travel.data$Date)

col.numeric <- c("Air", "Rail", "Auto")

travel.data <- convert_col_types(travel.data, col.numeric, as.numeric)

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
par(mfrow = c(3, 1))

airline.ts <- ts(travel.data$Air, start = c(1990, 1), end = c(2004, 1), frequency = 12)
airline.lm <- tslm(airline.ts ~ trend + I(trend ^ 2))

plot(airline.ts, xlab = "Time", ylab = "Airline Travel", bty = "l", log = "y")
lines(airline.lm$fitted, lwd = 2)

rail.ts <- ts(travel.data$Rail, start = c(1990, 1), end = c(2004, 1), frequency = 12)
rail.lm <- tslm(rail.ts ~ trend + I(trend ^ 2))

plot(rail.ts, xlab = "Time", ylab = "Rail Travel", bty = "l", log = "y")
lines(rail.lm$fitted, lwd = 2)

auto.ts <- ts(travel.data$Auto, start = c(1990, 1), end = c(2004, 1), frequency = 12)
auto.lm <- tslm(auto.ts ~ trend + I(trend ^ 2))

plot(auto.ts, xlab = "Time", ylab = "Auto Travel", bty = "l", log = "y")
lines(auto.lm$fitted, lwd = 2)

