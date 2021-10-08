### Premise & Problem Statement
# An online gaming portal wants to understand their customer patterns based on their 
# transactional behavior. For this, they have constructed a customer level data based 
# on the details they are tracking. The customer database consists of demographic and 
# transactional information for each customer. Building a regression model to predict 
# the customer revenue based on other factors.

#------------------------ READING DATA ------------------------------#
### Clear environment
rm(list=ls(all=TRUE))

### Set WD
setwd("Director path here..") #or Shft+Ctrl+h and choose

### Read data
data<-read.csv("CustomerData.csv",header=T)
names(data)
str(data)

#---------------------- DATA PRE PROCESSING ------------------------------#

###Splitting the data
rows=seq(1,nrow(data),1)
set.seed(123)
trainRows=sample(rows,(70*nrow(data))/100)
train = data[trainRows,] 
validation = data[-trainRows,]

#Note: Apply the same data pre processing steps on validation & test data, that u have done on train data

### Removing ID columns
train$CustomerID <- NULL #Remove ID columns as they won't be used in model building
validation$CustomerID <- NULL
#No CustomerID col in test data   

### Type conversions: From the summar of data, we see that cit is a potential factor attribute . So convert it into a factor
train$City <- as.factor(train$City)
validation$City <- as.factor(validation$City)

###Imputation
sum(is.na(train))
sum(is.na(validation))


#--------------------------- MODEL BUILDING ------------------------------#

###--- Model1 :  With all attributes ---###
# model1<- lm(TotalRevenueGenerated ~ City + NoOfChildren + MinAgeOfChild + 
#               MaxAgeOfChild + Tenure + FrquncyOfPurchase + NoOfUnitsPurchased + 
#               FrequencyOFPlay + NoOfGamesPlayed + NoOfGamesBought + FavoriteChannelOfTransaction + 
#               FavoriteGame, data=train)
#OR say target~. in formula to include all predictor variables
model1 <- lm(formula = TotalRevenueGenerated ~ ., data = train)
summary(model1)
# First Check: F-Stat's p-val << 0.05 -> Model is significant

#Residual analysis
par(mfrow=c(2,2))
plot(model1)
#res are almost linear;no patterns in res; a few lev/outliers are observed

#Let's see the performance of the model1
#Predictions on train, vaildation, and test data
model1_train_preds <- model1$fitted.values #OR
model1_train_preds <- predict(object = model1, newdata = train)
model1_validation_preds <- predict(object = model1, newdata = validation)

library(DMwR)
regr.eval(trues = train$TotalRevenueGenerated, preds = model1_train_preds)
regr.eval(trues = validation$TotalRevenueGenerated, preds = model1_validation_preds)
#Decent fit - Not overfitting or underfitting; error is about 20%
#Note: We report final metrics we obain on test data


###--- Model2 : On all attributes, after removing outliers, high leverage points, etc ---###

#Let's deal with the leverages and res outliers
lev= hat(model.matrix(model1)) #gives allleverages
plot(lev)
#Method1 : Manual - From the plot we made a decision to obtain leverage values greater than 0.2
train[lev>0.2,]
nrow(train[lev>0.2,])
#Let's remove these three points
train<-train[-which(lev>0.2),]

# #Method2 : Convention - If there are n data points and p parameters, then threshold can be taken a 3*p/n (some take 28p/n as well)
# lev_threshold <- 3*length(model1$coefficients)/length(lev)
# train[lev>lev_threshold,]
# length(train[lev>lev_threshold,])

#cooks distance
cook = cooks.distance(model1)
plot(cook,ylab="Cooks distances")
max=as.numeric(which.max(cook))
points(max,cook[max],col='red', pch=19)
train[max,]
train <- train[-max,]

#Residual outliers
residuals = model1$residuals
outliers <- boxplot(residuals,plot=T)$out
sort(outliers)
length(outliers)
#let's keep them.

#Now build a model on the new clean data, with all attributes
model2<- lm(TotalRevenueGenerated~., data = train)
summary(model2)
#Predictions on train, vaildation, and test data
model2_train_preds <- model2$fitted.values #OR
model2_train_preds <- predict(object = model2, newdata = train)
model2_validation_preds <- predict(object = model2, newdata = validation)

regr.eval(trues = train$TotalRevenueGenerated, preds = model2_train_preds)
regr.eval(trues = validation$TotalRevenueGenerated, preds = model2_validation_preds)

###--- Model 3 : Multicollinearity check using Variance inflation factor ---###
library(car)
vif(model2)
#We see FrquncyOfPurchase and NoOfGamesBought have VIF >10; So ignoring them for model building
model3 <- lm(TotalRevenueGenerated ~ City + NoOfChildren + MinAgeOfChild + 
              MaxAgeOfChild + Tenure + NoOfUnitsPurchased + 
              FrequencyOFPlay + NoOfGamesPlayed  + FavoriteChannelOfTransaction + 
              FavoriteGame, data=train)
summary(model3)
#R2 dropped from 73 t0 69! :(
#Let's see the performance of the model1
#Predictions on train, vaildation, and test data
model3_train_preds <- model3$fitted.values #OR
model3_train_preds <- predict(object = model3, newdata = train)
model3_validation_preds <- predict(object = model3, newdata = validation)

regr.eval(trues = train$TotalRevenueGenerated, preds = model3_train_preds)
regr.eval(trues = validation$TotalRevenueGenerated, preds = model3_validation_preds)
#Error got increased.


###--- Model 4 : Using StepAIC   ---###
model1 <- lm(formula = TotalRevenueGenerated ~ ., data = train)
summary(model1)
# Stepwise Regression
library(MASS)
step <- stepAIC(model1, direction="both")
step #the model ignored NoOfGamesPlayed attribute

model4 <- lm(formula = TotalRevenueGenerated ~ City + NoOfChildren + MinAgeOfChild + 
               Tenure + FrquncyOfPurchase + NoOfUnitsPurchased + FrequencyOFPlay + 
               NoOfGamesBought + FavoriteChannelOfTransaction + FavoriteGame, 
             data = train)#formula for best model taken from stepAIC model
summary(model4)
#Predictions on train, vaildation, and test data
model4_train_preds <- model4$fitted.values #OR
model4_train_preds <- predict(object = model4, newdata = train)
model4_validation_preds <- predict(object = model4, newdata = validation)

regr.eval(trues = train$TotalRevenueGenerated, preds = model4_train_preds)
regr.eval(trues = validation$TotalRevenueGenerated, preds = model4_validation_preds)


###General Note: One can try different models in a variety of combinations. As long as the model's overall significance is good,
# one can choose the model for prediction. And, one can finalize a model based on its performance on the test data(and explicability, if needed)

####################################################
## Standardizing the Data
# We will use the Caret pakcage to standardize the data after the split using the __"preProcess()"__ function
#It saves the metrics such as mean and standard deviation used for calculating the standardized value by creating a model object
#We can then use the model object in the "predict()"  function to standardize any other unseen dataset with the same distribuiton and variables

library(caret)
# The "preProcess()" function creates a model object required for standardizing unseen data
# Do not standardize the target variable

train_nonstd = train
test_nonstd = validation

independentattr<-setdiff(names(train),c("TotalRevenueGenerated"))
std_model <- preProcess(train[, independentattr], method = c("range"))
std_model
# The predict() function is used to standardize any other unseen data

train[, independentattr] <- predict(object = std_model, newdata = train[, independentattr])
validation[, independentattr] <- predict(object = std_model, newdata = validation[, independentattr])


# Model3- Build linear regression with all standardized attributes 
LinReg_std1<-lm(TotalRevenueGenerated~., data=train)
summary(LinReg_std1)

#Error verification on train data
regr.eval(train$TotalRevenueGenerated, LinReg_std1$fitted.values) 

#Error verification on test data
Pred<-predict(LinReg_std1,validation)
regr.eval(validation$TotalRevenueGenerated, Pred)
plot(LinReg_std1)

