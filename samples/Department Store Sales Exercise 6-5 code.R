library(forecast)

dept.store.data <- read.csv("DepartmentStoreSales.csv")
sales.ts <- ts(dept.store.data$Sales[1:20], freq = 4)
sales.all.ts <- ts(dept.store.data$Sales, freq = 4)
sales.all.ts
lm.expo.trend <- tslm(sales.ts ~ trend + season, lambda = 0)

# Table 6.8
summary(lm.expo.trend)

# Figure 6.24
par(mfrow = c(1,1))
plot(sales.all.ts, xlim = c(1,7), ylim = c(40000, 110000),  ylab = "Sales", xlab = "Quarter", bty = "l", main = "", yaxt ="n", xaxt ="n", type = "o")
axis(2, at = seq(40000, 100000, 20000), labels = format(seq(40000, 100000, 20000), scientific=FALSE))
axis(1, at = seq(1.25, 6.75, 0.5), labels = seq(2, 24, 2))

# Figure 6.25
par(mfrow = c(2,1))
plot(sales.ts, xlim = c(1,6), ylim = c(40000, 100000),  ylab = "Sales", xlab = "Quarter", bty = "l", main = "", yaxt ="n", xaxt ="n", type = "o")
axis(2, at = seq(40000, 100000, 10000), labels = format(seq(40000, 100000, 10000), scientific=FALSE))
axis(1, at = seq(1.25, 5.75, 0.5), labels = seq(2, 20, 2))
lines(lm.expo.trend$fitted, lwd = 2, col = "blue")
plot(sales.ts - lm.expo.trend$fitted, xlim = c(1,6), ylim = c(-4000, 4000),  ylab = "Residuals", xlab = "Quarter", bty = "l", main = "", xaxt ="n", type = "o")
axis(1, at = seq(1.25, 5.75, 0.5), labels = seq(2, 20, 2))