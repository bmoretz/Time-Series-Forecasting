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
souvenir.lm <- tslm(souvenir.ts ~ trend + I(trend ^ 2))

par(mfrow = c(2, 1))
plot(souvenir.ts, xlab = "Year", ylab = "Sales", bty = "l")
lines(souvenir.lm$fitted, lwd = 2)

plot(souvenir.ts, xlab = "Year", ylab = "Log Sales", log = "y", bty = "l")
lines(souvenir.lm$fitted, lwd = 2)