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

source.path <- "datasets/ApplianceShipments.xls"
shipments.wb <- loadWorkbook(source.path)

shipments.data <- as.data.table(readWorksheet(shipments.wb, "Data"))
shipments.data[, 3:4] <- NULL
colnames(shipments.data) <- c("Quarter", "Shipments")

col.numeric <- c("Shipments")

shipments.data <- convert_col_types(shipments.data, col.numeric, as.numeric)

rm(source.path)
rm(shipments.wb)

shipments.ts <- ts(shipments.data$Shipments, start = c(1985, 1), frequency = 4)
shipments.lm <- tslm(shipments.ts ~ trend + I(trend ^ 2))

plot(shipments.ts, xlab = "Quarter", ylab = "Shipments", bty = "l")
lines(shipments.lm$fitted, lwd = 2)