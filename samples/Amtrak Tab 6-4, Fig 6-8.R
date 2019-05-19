library(forecast)
library(zoo)

Amtrak.data <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)

nValid <- 36
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))

train.lm.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)
train.lm.trend.season.pred <- forecast(train.lm.trend.season, h = nValid, level = 0)

# Table 6-4
summary(train.lm.trend.season)

# Figure 6-8
par(mfrow = c(2,1))
plot(train.lm.trend.season.pred, ylim = c(1300, 2625),  ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train.lm.trend.season.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)
lines(c(2004.25 - 3, 2004.25 - 3), c(0, 3500)) 
lines(c(2004.25, 2004.25), c(0, 3500))
text(1996.25, 2600, "Training")
text(2002.75, 2600, "Validation")
text(2005.25, 2600, "Future")
arrows(2004 - 3, 2450, 1991.25, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5 - 3, 2450, 2004, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5, 2450, 2006, 2450, code = 3, length = 0.1, lwd = 1, angle = 30)

plot(train.lm.trend.season.pred$residuals, ylim = c(-400, 550),  ylab = "Residuals", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train.ts - train.lm.trend.season.pred$fitted)
lines(valid.ts - train.lm.trend.season.pred$mean)
lines(c(2004.25 - 3, 2004.25 - 3), c(-500, 3500))
lines(c(2004.25, 2004.25), c(-500, 3500))
text(1996.25, 525, "Training")
text(2002.75, 525, "Validation")
text(2005.25, 525, "Future")
arrows(2004 - 3, 425, 1991.25, 425, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5 - 3, 425, 2004, 425, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5, 425, 2006, 425, code = 3, length = 0.1, lwd = 1, angle = 30)
dev.off()


