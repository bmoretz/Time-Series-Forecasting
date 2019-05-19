library("caret")

rain.df <- read.csv("MelbourneRainfall.csv")
rain.df$Date <- as.Date(rain.df$Date, format="%m/%d/%Y")
rain.df$Rainy <- ifelse(rain.df$Rainfall > 0, 1, 0)
nPeriods <- length(rain.df$Rainy)
rain.df$Lag1 <- c(NA,rain.df$Rainfall[1:(nPeriods-1)])
rain.df$t <- seq(1, nPeriods, 1)
rain.df$Seasonal_sine = sin(2 * pi * rain.df$t / 365.25)
rain.df$Seasonal_cosine = cos(2 * pi * rain.df$t / 365.25)
train.df <- rain.df[rain.df$Date <= as.Date("12/31/2009", format="%m/%d/%Y"), ]
train.df <- train.df[-1,]
valid.df <- rain.df[rain.df$Date > as.Date("12/31/2009", format="%m/%d/%Y"), ]
xvalid <- valid.df[, c(4,6,7)]
  
rainy.lr <- glm(Rainy ~ Lag1 + Seasonal_sine + Seasonal_cosine, data = train.df, family = "binomial")
summary(rainy.lr)
rainy.lr.pred <- predict(rainy.lr, xvalid, type = "response") 
confusionMatrix(ifelse(rainy.lr$fitted > 0.5, 1, 0), train.df$Rainy, positive="1")
confusionMatrix(ifelse(rainy.lr.pred > 0.5, 1, 0), valid.df$Rainy, positive="1")


