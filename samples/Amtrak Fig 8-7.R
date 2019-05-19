library(forecast)  

Amtrak.data <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)
length(ridership.ts)
nValid <- 36
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))

set.seed(201)
ridership.nnetar <- nnetar(train.ts, repeats = 20, p = 11, P = 1, size = 7)
summary(ridership.nnetar$model[[1]])
ridership.nnetar.pred <- forecast(ridership.nnetar, h = nValid)
accuracy(ridership.nnetar.pred, valid.ts)

# Figure 7-7
plot(train.ts, ylim = c(1300, 2900),  ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), lty = 1)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ridership.nnetar.pred$fitted, lwd = 2, col = "blue")
lines(ridership.nnetar.pred$mean, lwd = 2, col = "blue", lty = 2)
lines(valid.ts)
lines(c(2004.25 - 3, 2004.25 - 3), c(0, 3500)) 
lines(c(2004.25, 2004.25), c(0, 3500))
text(1996.25, 2850, "Training")
text(2002.75, 2850, "Validation")
text(2005.25, 2850, "Future")
arrows(2004 - 3, 2700, 1991.25, 2700, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5 - 3, 2700, 2004, 2700, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5, 2700, 2006, 2700, code = 3, length = 0.1, lwd = 1, angle = 30)

ridership.nnetar.opt <- nnetar(train.ts)
ridership.nnetar.opt
ridership.nnetar.opt.pred <- forecast(ridership.nnetar.opt, h = nValid)
accuracy(ridership.nnetar.opt.pred, valid.ts)
