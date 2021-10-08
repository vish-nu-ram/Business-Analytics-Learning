setwd("D:/Rennes/CSE7302c/Day03")

library(car)
mtcars
data(mtcars)
View(mtcars)
mtcarslm <- lm(mpg ~ ., data=mtcars)
summary(mtcarslm)
plot(mtcarslm)
vif(mtcarslm)
mtcarsStepAIC <- stepAIC(mtcarslm)
mtcarsStepAIC

mtcarslm2 <- lm(mpg ~ am+qsec+wt, data = mtcars)
summary(mtcarslm2)
plot(mtcarslm2)
vif(mtcarslm2)

mtcarslm3 <- lm(mpg ~ qsec+wt, data=mtcars)
summary(mtcarslm3)
plot(mtcarslm3)
vif(mtcarslm3)
