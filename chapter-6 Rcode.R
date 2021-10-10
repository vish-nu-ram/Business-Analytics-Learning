#required libraries
library(leaps) #for subset selection
library(gains) #for gains and lift chart
library(forecast) #for accuracy measures
library(corrgram) #for producing a graphical display of a correlation matrix

#load the data
tayko.df <- read.csv("Tayko.csv")
head(tayko.df)
t(t(names(tayko.df)))
summary(tayko.df)

#keep only the requested variables
tayko.df <- tayko.df[, c(2, 18, 19, 21:23, 25)]
t(t(names(tayko.df)))


aggregate(tayko.df$Spending, list(tayko.df$Gender.male), mean)
aggregate(tayko.df$Spending, list(tayko.df$Gender.male), sd)

aggregate(tayko.df$Spending, list(tayko.df$Web.order), mean)
aggregate(tayko.df$Spending, list(tayko.df$Web.order), sd)


aggregate(tayko.df$Spending, list(tayko.df$Address_is_res), mean)
aggregate(tayko.df$Spending, list(tayko.df$Address_is_res), sd)

#scatter plots
plot(tayko.df$Freq ~ tayko.df$Spending)
plot(tayko.df$last_update_days_ago ~ tayko.df$Spending)

#splitting the data
set.seed(1)  
train.index <- sample(c(1:dim(tayko.df)[1]), 
                      0.6*dim(tayko.df)[1])  
valid.index <- setdiff(c(1:dim(tayko.df)[1]), train.index)  
train.df <- tayko.df[train.index, ]
valid.df <- tayko.df[valid.index, ]

reg <- lm(Spending ~ Gender.male + 
            Web.order +
            Address_is_res +
            US +
            Freq +
            last_update_days_ago, data = train.df)
summary(reg)

#stepwise backward regression
step(reg, direction = "backward")


#first observation in the validation set
first.obs <- head(valid.df,1)
dim(first.obs)
library(forecast)
#manual calculation of prediction error
#The prediction for this record is computed using the fitted model:
Spending <- -5.87 + (-11.624718) * 1 + (89.089779) * 2 + (-0.009212) * 3662 + 
  (14.140303) * 1 + (-3.189701) * 0 + (-71.654020) * 1 
Spending

Prediction.error <- first.obs[1,7] - Spending
Prediction.error


pred <- predict(reg, valid.df)

#accuracy
#train set
pred.train <- predict(reg, train.df)
accuracy(pred.train, train.df$Spending)


#validation set
accuracy(pred, valid.df$Spending)
par(mar=c(1,1,1,1))
hist(reg$residuals)

















#load the data
car.df <- read.csv("ToyotaCorolla.csv")
dim(car.df)

#partition the data into training (50%), validation (30%), and test (20%) 
#sets
train.index <- sample(c(1:dim(car.df)[1]), 
                      0.5*dim(car.df)[1])  
remaining.index <- setdiff(c(1:dim(car.df)[1]), train.index)  

valid.index <- sample(remaining.index, 
                      0.3*dim(car.df)[1])
test.index <- setdiff(remaining.index, valid.index)



train.df <- car.df[train.index, ]
valid.df <- car.df[valid.index, ]
test.df <- car.df[test.index, ]


#multiple linear regression on training data
reg <- lm(Price ~ Age_08_04 + KM + factor(Fuel_Type) + HP + Automatic + Doors 
          + Quarterly_Tax + Mfr_Guarantee + Guarantee_Period + Airco 
          + Automatic_airco + CD_Player + Powered_Windows + Sport_Model 
          + Tow_Bar, 
          data = train.df)
summary(reg)


#prediction accuracy on validation data
pred <- predict(reg, valid.df)
accuracy(pred, valid.df$Price)
#prediction accuracy on test data
pred <- predict(reg, test.df)
accuracy(pred, test.df$Price)

