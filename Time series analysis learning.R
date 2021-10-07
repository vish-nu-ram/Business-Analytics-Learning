amtrak.data <- read.csv("Amtrak.csv")
amtrak.data
install.packages("stats")
library(stats)
?ts
ridership <- ts(amtrak.data$Ridership, start = c(1991,1), end = c(2003,3), freq = 12)
ridership

plot(ridership)

library(forecast)

ridership.3yrs <- window(ridership, start = c(1997,1), end = c(1999,12))

ridership.lm <- tslm(ridership ~ trend)
plot(ridership, xlab = "Time", ylab = "Ridership", ylim = c(1300,2300), bty = "l")
lines(ridership.lm$fitted.values)

# Exponential 

ridership.lm1 <- tslm(ridership ~ trend, lambda = 0)
plot(ridership, xlab = "Time", ylab = "Ridership", ylim = c(1300,2300), bty = "l")
lines(ridership.lm1$fitted.values, col = "blue")

# Polynomial
ridership.lm2 <- tslm(ridership ~ trend+ I(trend^2))
plot(ridership, xlab = "Time", ylab = "Ridership", ylim = c(1300,2300), bty = "l")
lines(ridership.lm2$fitted.values, col = "red")

plot(ridership.lm2$residuals)
#  You can see seasonality on the error, so wee need to consider that

ridership.lm3 <- tslm(ridership ~ trend+ I(trend^3))
plot(ridership, xlab = "Time", ylab = "Ridership", ylim = c(1300,2300), bty = "l")
lines(ridership.lm3$fitted.values, col = "red")
plot(ridership.lm3$residuals)
