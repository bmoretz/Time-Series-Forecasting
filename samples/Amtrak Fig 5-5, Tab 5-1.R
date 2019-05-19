library(forecast)
library(zoo)

Amtrak.data <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)

diff.twice.ts <- diff(diff(ridership.ts, lag = 12), lag = 1)
nValid <- 36
nTrain <- length(diff.twice.ts) - nValid
train.ts <- window(diff.twice.ts, start = c(1992, 2), end = c(1992, nTrain + 1))
valid.ts <- window(diff.twice.ts, start = c(1992, nTrain + 2), end = c(1992, nTrain + 1 + nValid))

ses <- ets(train.ts, model = "ANN", alpha = 0.2)
ses.pred <- forecast(ses, h = nValid, level = 0)

# Figure 5-5
plot(ses.pred, ylim = c(-250, 300),  ylab = "Ridership (Twice-Differenced)", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ses.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)
lines(c(2004.25 - 3, 2004.25 - 3), c(-250, 350)) 
lines(c(2004.25, 2004.25), c(-250, 350))
text(1996.25, 275, "Training")
text(2002.75, 275, "Validation")
text(2005.25, 275, "Future")
arrows(2004 - 3, 245, 1991.5, 245, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5 - 3, 245, 2004, 245, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5, 245, 2006, 245, code = 3, length = 0.1, lwd = 1, angle = 30)
dev.off()


# Table 5.1
ses.opt <- ets(train.ts, model = "ANN")
ses.opt.pred <- forecast(ses.opt, h = nValid, level = 0)
ses.opt
accuracy(ses.pred, valid.ts)
accuracy(ses.opt.pred, valid.ts)


# Miscellaneous calculations below.
ar1 <- auto.arima(train.ts)
ar1
arima.pred <- forecast(ar1, h = nValid)

diff.once.ts <- diff(ridership.ts, lag = 12)
diff.df <- data.frame("Time" = as.vector(time(ridership.ts)), "None" = as.vector(ridership.ts), "Once" = c(rep(NA, 12), as.vector(diff.once.ts)), "Twice" = c(rep(NA, 13), as.vector(diff.twice.ts)), "Pred" = c(rep(NA, length(ridership.ts) - nValid), as.vector(ses.pred$mean)))

rts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
rets <- ets(rts)
rets
rets.pred <- forecast(rets, h = nValid)
plot(rets.pred)

initial.diff.once <- window(diff.once.ts, start = c(2001, 3), end = c(2001, 3))
once.pred <- as.vector(diffinv(arima.pred$mean, lag = 1, xi = initial.diff.once))
once.pred[2:37]
initial.values <- window(ridership.ts, start = c(2000, 4), end = c(2001, 3))
as.vector(diffinv(once.pred[2:37], lag = 12, xi = initial.values))
final.pred <- diffinv(once.pred[2:37], lag = 12, xi = initial.values)
final.pred[13:48]
accuracy(final.pred[13:48], ridership.ts[124:159])

ets(ridership.ts)$states
