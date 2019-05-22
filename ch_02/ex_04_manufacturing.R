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

source.path <- "datasets/CanadianWorkHours.xls"
hours.wb <- loadWorkbook(source.path)

hours.data <- as.data.table(readWorksheet(hours.wb, "Sheet1"))
colnames(hours.data) <- c("Year", "Hours")

col.numeric <- c("Year", "Hours")

hours.data <- convert_col_types(hours.data, col.numeric, as.numeric)

rm(source.path)
rm(hours.wb)

hours.ts <- ts(hours.data$Hours, start = c(1985, 1), frequency = 1)
hours.lm <- tslm(hours.ts ~ trend + I(trend ^ 2))

plot(hours.ts, xlab = "Year", ylab = "Hours", bty = "l")
lines(hours.lm$fitted, lwd = 2)