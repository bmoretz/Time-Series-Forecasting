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

source.path <- "datasets/ShampooSales.xls"
shampoo.wb <- loadWorkbook(source.path)

shampoo.data <- as.data.table(readWorksheet(shampoo.wb, "Sheet1"))
colnames(shampoo.data) <- c("Month", "Sales")

shampoo.data$Month <- as.Date(shampoo.data$Month)
col.numeric <- c("Sales")

shampoo.data <- convert_col_types(shampoo.data, col.numeric, as.numeric)

rm(source.path)
rm(shampoo.wb)

shampoo.ts <- ts(shampoo.data$Sales, start = c(1995, 1), frequency = 12)

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

par(mfrow = c(1, 1))
hist(naive.pred$residuals)
