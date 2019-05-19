library(forecast)

Amtrak.data <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)

fixed.nValid <- 36
fixed.nTrain <- length(ridership.ts) - fixed.nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, fixed.nTrain))
valid.ts <- window(ridership.ts, start = c(1991, fixed.nTrain + 1), end = c(1991, fixed.nTrain + fixed.nValid))
naive.fixed <- naive(train.ts, h = fixed.nValid)
naive.roll <- ts(Amtrak.data$Ridership[fixed.nTrain:(fixed.nTrain + fixed.nValid - 1)], start = c(1991, fixed.nTrain + 1), end = c(1991, fixed.nTrain + fixed.nValid), freq = 12)

# Figure 3-6
plot(train.ts, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(naive.fixed$mean, lwd = 2, col = "blue", lty = 2)
lines(naive.roll, lwd = 2, col = "blue", lty = 2)
lines(valid.ts)
lines(c(2004.25 - 3, 2004.25 - 3), c(0, 3500)) 
lines(c(2004.25, 2004.25), c(0, 3500))
text(1996.25, 2500, "Training")
text(2002.75, 2500, "Validation")
text(2005.25, 2500, "Future")
arrows(2004 - 3, 2450, 1991.25, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5 - 3, 2450, 2004, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5, 2450, 2006, 2450, code = 3, length = 0.1, lwd = 1, angle = 30)

