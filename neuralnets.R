#loading libraries 
library(caret)
install.packages("neuralnet")
library(neuralnet)
library(forecast)
library(gains)


# Problem 11.4 Direct Mailing to Airline Customers. 
## East-West Airlines has entered into a partnership with the wireless phone 
## company Telcon to sell the latter's service via direct mail. The file 
## EastWestAirlinesNN.csv contains a subset of a data sample of who has already
## received a test offer. About 13% accepted.
## You are asked to develop a model to classify East-West customers as to 
## whether they purchase a wireless phone service contract (outcome variable
## Phone_Sale). This model will be used to classify additional customers.

df <- read.csv("EastWestAirlinesNN.csv")
t(t(names(df)))
# remove unnecessary variables
df <- df[,-c(1)]
t(t(names(df)))
# check and re,ove missing values
summary(df)
View(df)
df <- na.omit(df)

set.seed(12345)
train.index <- sample(row.names(df), 0.6*dim(df)[1])  
valid.index <- setdiff(row.names(df), train.index)  
train.df <- df[train.index, ]
valid.df <- df[valid.index, ]
t(t(names(valid.df)))


#normalise
norm.values <- preProcess(train.df[,-15], method="range")
train.norm.df <- predict(norm.values, train.df[,-15])
valid.norm.df <- predict(norm.values, valid.df[,-15])

## 11.4.a Run a neural net model on these data, using a single hidden layer with
## 5 nodes.

nn <- neuralnet(factor(train.df$Phone_sale) ~ Topflight +
                  Balance +
                  Qual_miles +
                  cc1_miles. +
                  cc2_miles. +
                  cc3_miles. +
                  Bonus_miles +
                  Bonus_trans +
                  Flight_miles_12mo +
                  Flight_trans_12 +
                  Online_12+Email +
                  Club_member +
                  Any_cc_miles_12mo,
                data = train.norm.df, linear.output = F,
                hidden = 5)

plot(nn)
nn$weights
# predictions on training and validation data
options(scipen = 0)
# training prediction probabilities
train.pred <- compute(nn, train.norm.df)
train.pred <- train.pred$net.result[,2]
# convert probabilities to classes
train.class <- (1* (train.pred>0.5))
confusionMatrix(factor(train.class), factor(train.df$Phone_sale), positive = "1")



# validation prediction probabilities
valid.pred <- compute(nn, valid.norm.df)
valid.pred <- valid.pred$net.result[,2]
# convert probabilities to classes
valid.class <- (1* (valid.pred>0.5))
# confusion matrix 
confusionMatrix(factor(valid.class), factor(valid.df$Phone_sale), positive = "1")


##11.4.b Run a second neural net model on the data, this time setting the 
##number of hidden nodes to 1. Comment now on the difference between this model
##and the model you ran earlier, and how overfitting might have affected 
##results.

# run nn with 1 hidden node

nn <- neuralnet(factor(train.df$Phone_sale) ~ Topflight + 
                  Balance +
                  Qual_miles +
                  cc1_miles. + 
                  cc2_miles. + 
                  cc3_miles. + 
                  Bonus_miles + 
                  Bonus_trans + 
                  Flight_miles_12mo + 
                  Flight_trans_12 + 
                  Online_12+Email + 
                  Club_member + 
                  Any_cc_miles_12mo, 
                data = train.norm.df, linear.output = F, 
                hidden = 1)

plot(nn)
#predictions on training and validation data
options(scipen = 0)
#training predictions
train.pred <- compute(nn, train.norm.df)
train.pred <- train.pred$net.result[,2]
# convert probabilities to classes
train.class <- (1* (train.pred>0.5)) 
confusionMatrix(factor(train.class), factor(train.df$Phone_sale), positive = "1")


valid.pred <- compute(nn, valid.norm.df)
valid.pred <- valid.pred$net.result[,2]
# convert probabilities to classes
valid.class <- (1* (valid.pred>0.5))
# confusion matrix 
confusionMatrix(factor(valid.class), factor(valid.df$Phone_sale), positive = "1")
