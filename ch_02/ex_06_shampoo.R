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
shampoo.lm <- tslm(shampoo.ts ~ trend + I(trend ^ 2))

par(mfrow = c(2, 1))
plot(shampoo.ts, xlab = "Year", ylab = "Sales", bty = "l")
lines(shampoo.lm$fitted, lwd = 2)

plot(shampoo.ts, xlab = "Year", ylab = "Log Sales", log = "y", bty = "l")
lines(shampoo.lm$fitted, lwd = 2)