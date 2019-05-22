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

source.path <- "datasets/DepartmentStoreSales.xls"
sales.wb <- loadWorkbook(source.path)

sales.data <- as.data.table(readWorksheet(sales.wb, "Sheet1"))
colnames(sales.data) <- c("Quarter", "Sales")

col.numeric <- c("Quarter", "Sales")

sales.data <- convert_col_types(sales.data, col.numeric, as.numeric)

sales.data <- xts(travel.data, sales.data$Quarter)

rm(source.path)
rm(travel.wb)

sales.ts <- ts(sales.data$Sales, frequency = 12)
sales.lm <- tslm(sales.ts ~ trend + I(trend ^ 2))

plot(sales.ts, xlab = "Quarter", ylab = "Sales", bty = "l")
lines(sales.lm$fitted, lwd = 2)