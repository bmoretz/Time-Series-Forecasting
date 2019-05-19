library(forecast)

Amtrak.data <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)

stepsAhead <- 36
nTrain <- length(ridership.ts) - stepsAhead
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + stepsAhead))
ridership.lm <-  tslm(train.ts ~ trend + I(trend^2))
ridership.lm.pred <- forecast(ridership.lm, h = stepsAhead, level = 0)

# Figure 3-3
plot(ridership.lm.pred$residuals, ylim = c(-400, 500),  ylab = "Residuals", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(valid.ts - ridership.lm.pred$mean, lwd = 1)
lines(c(2004.25 - 3, 2004.25 - 3), c(-500, 3500))
lines(c(2004.25, 2004.25), c(-500, 3500))
text(1996.25, 500, "Training")
text(2002.75, 500, "Validation")
text(2005.25, 500, "Future")
arrows(2004 - 3, 450, 1991.25, 450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5 - 3, 450, 2004, 450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5, 450, 2006, 450, code = 3, length = 0.1, lwd = 1, angle = 30)