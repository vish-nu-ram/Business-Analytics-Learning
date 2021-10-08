setwd("D:/Rennes/CSE7302c/Day03")

# Linear Regression through Origin
cars
par(mfrow=c(1,1))
plot(cars)

par(mfrow=c(2,2))

carslm <- lm(dist ~ speed, data=cars)
summary(carslm)
plot(carslm)

# Intercept-free model (in general, not a good practice)
carslm0intercept <- lm(dist~speed+0,data = cars)
summary(carslm0intercept)
plot(carslm0intercept)

# Alternative usage for removing intercept: lm(dist ~ speed -1, data = cars)
carslm0interceptAlt <- lm(dist~speed-1,cars)
summary(carslm0interceptAlt)


