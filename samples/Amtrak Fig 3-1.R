library(forecast)

Amtrak.data <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)

# Figure 3-1
plot(ridership.ts, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25))
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1), digits = 2))
lines(c(2004.25 - 3 , 2004.25 - 3), c(0, 3500))
lines(c(2004.25, 2004.25), c(0, 3500))
text(1996.25, 2500, "Training")
text(2002.75, 2500, "Validation")
text(2005.25, 2500, "Future")
arrows(2004 - 3,2450,1991.25,2450,code=3,length=0.1,lwd=1,angle=30)
arrows(2004.5 - 3,2450,2004,2450,code=3,length=0.1,lwd=1,angle=30)
arrows(2004.5,2450,2006,2450,code=3,length=0.1,lwd=1,angle=30)
