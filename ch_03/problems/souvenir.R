library(data.table)
library(XLConnect)
library(forecast)
library(xts)
library(ggplot2)

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

source.path <- "datasets/SouvenirSales.xls"
souvenir.wb <- loadWorkbook(source.path)

souvenir.data <- as.data.table(readWorksheet(souvenir.wb, "Sheet1"))
souvenir.data[, 3:4] <- NULL
colnames(souvenir.data) <- c("Date", "Sales")

col.numeric <- c("Sales")

souvenir.data <- convert_col_types(souvenir.data, col.numeric, as.numeric)

rm(source.path)
rm(souvenir.wb)

souvenir.ts <- ts(souvenir.data$Sales, start = c(1995, 1), frequency = 12)

fixed.nValid <- 12
fixed.nTrain <- 72

training.ts <- window(souvenir.ts, start = c(1995, 1), end = c(1995, fixed.nTrain))
valid.ts <- window(souvenir.ts, start = c(1995, fixed.nTrain + 1),
                   end = c(1995, fixed.nTrain + fixed.nValid))

autoplot.zoo(training.ts)
autoplot.zoo(valid.ts)

# Naive Forecast
naive.pred <- naive(training.ts, h = 24)
accuracy(naive.pred, valid.ts)

par(mfrow=c(1,1))
hist(naive.pred$residuals)

