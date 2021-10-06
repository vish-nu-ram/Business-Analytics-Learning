install.packages("forecast")
library(forecast)
population <- c(2,6,8,8,12,16,20,20,22,26)
sales <- c(58,105,88,118,117,137,157,169,149,202)
pizza <- as.data.frame(cbind(population,sales))

var(pizza)
cor(pizza)
pizza_regr <- lm(sales ~ population, data=pizza)
summary(pizza_regr)

plot(pizza)

abline(pizza_regr)

pizza_std <- data.frame(scale(pizza))
var(pizza_std)
cor(pizza_std)
pizza_regr <- lm(sales ~ population, data=pizza_std)
summary(pizza_regr)

plot(pizza_std)

abline(pizza_regr)

colMeans(pizza)

# Simulate Data
Experience <- c(4,7,1,5,8,10,0,1,6,6,9,2,10,5,6,8,4,6,3,3)
Score <- c(78,100,86,82,86,84,75,80,83,91,88,73,75,81,74,87,79,94,70,89)
Salary <- c(24,43,23.7,34.3,35.8,38,22.2,23.1,30,33,38,26.6, 36.2,31.6,29,34,30.1,33.9,28.2,30)
prog <- as.data.frame(cbind(Salary,Experience, Score))

prog_regr <- lm(Salary ~ Experience + Score, data=prog)
summary(prog_regr)

prog_std <- data.frame(scale(prog))
prog_regr_std <- lm(Salary ~ Experience + Score, data=prog_std)
summary(prog_regr_std)

# non-linear regression ----
x <- rnorm(200)
e <- rnorm(200)
y <- x^2 + e
plot(x,y)
cor(x,y)
cor(x*x,y)

x_quad <- x*x

reg <- lm(y ~ x_quad)

summary(reg)
points(x,reg$fitted.values, pch = 20)


y <- x^3 + e
x_cubic <- x*x*x
plot(x,y)
reg <- lm(y ~ x_cubic)
summary(reg)
points(x,reg$fitted.values, pch = 20)



car.df <- read.csv("ToyotaCorolla.csv")
# use first 1000 rows of data
car.df <- car.df[1:1000, ]
# select variables for regression
selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)

# partition data
set.seed(1)  # set seed for reproducing the partition
train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]

# use lm() to run a linear regression of Price on all 11 predictors in the
# training set.
# use . after ~ to include all the remaining columns in train.df as predictors.
car.lm <- lm(Price ~ ., data = train.df)

#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(car.lm)
car.lm.pred <- predict(car.lm)
accuracy(car.lm.pred, train.df$Price)


# use predict() to make predictions on a new set.
car.lm.pred2 <- predict(car.lm, valid.df)
options(scipen=999, digits = 0)
some.residuals <- valid.df$Price[1:20] - car.lm.pred2[1:20]
data.frame("Predicted" = car.lm.pred2[1:20], "Actual" = valid.df$Price[1:20],
           "Residual" = some.residuals)

options(scipen=999, digits = 3)
# use accuracy() to compute common accuracy measures.
accuracy(car.lm.pred2, valid.df$Price)

#### Figure 6.1

car.lm.pred <- predict(car.lm, valid.df)
all.residuals <- valid.df$Price - car.lm.pred
length(all.residuals[which(all.residuals > -1406 & all.residuals < 1406)])/400
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")
RMSE <- accuracy(car.lm.pred, valid.df$Price)[1,2]
ME <-  accuracy(car.lm.pred, valid.df$Price)[1,1]
MAE <-  accuracy(car.lm.pred, valid.df$Price)[1,3]

abline(v = c(-3*RMSE, -2*RMSE, -1*RMSE,1*RMSE,2*RMSE,3*RMSE), col = "blue", lty = 2)
abline(v = ME, col = "red")
hist(abs(all.residuals), breaks = 25, xlab = "Residuals", main = "")
abline(v = MAE, col = "red")


#### Table 6.5

# use regsubsets() in package leaps to run an exhaustive search.
# unlike with lm, categorical predictors must be turned into dummies manually.
library(leaps)
search <- regsubsets(Price ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
                     method = "exhaustive")
sum <- summary(search)

# show models
sum$which

# show metrics
sum$rsq
sum$adjr2
sum$Cp

#### Table 6.6
# use step() to run stepwise regression.
car.lm.step <- step(car.lm, direction = "backward")
summary(car.lm.step)  # Which variables were dropped?
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)

#### Table 6.7
# create model with no predictors
car.lm.null <- lm(Price~1, data = train.df)
# use step() to run forward regression.
car.lm.step <- step(car.lm.null, scope=list(lower=car.lm.null, upper=car.lm), direction = "forward")
summary(car.lm.step)  # Which variables were added?
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)

#### Table 6.8
# use step() to run stepwise regression.
car.lm.step <- step(car.lm, direction = "both")
summary(car.lm.step)  # Which variables were dropped/added?
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)


## Cool plotting regression function ----
interactive_regression <- function() {
  cat("Click on the plot to create data points; hit [esc] to stop")
  plot(NA, xlim=c(-5,50), ylim=c(-5,50))
  points = data.frame()
  repeat {
    click_loc <- locator(1)
    if (is.null(click_loc)) break
    if(nrow(points) == 0 ) {
      points <- data.frame(x=click_loc$x, y=click_loc$y)
    } else {
      points <- rbind(points, c(click_loc$x, click_loc$y))
    }
    plot(points, xlim=c(-5,50), ylim=c(-5,50), pch=19, col="gray")
    if (nrow(points) < 2) next

    model <- lm(points$y ~ points$x)
    abline(model, lwd=2, col="cornflowerblue")
    text(1, 50, paste(c("Raw intercept: ", round(model$coefficients[1], 2)), collapse=" "))
    text(1, 45, paste(c("Raw slope    : ", round(model$coefficients[2], 2)), collapse=" "))
    text(1, 40, paste(c("Correlation  : ", round(cor(points$x, points$y), 2)), collapse=" "))
  }

  return(points)
}
