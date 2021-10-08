###Linear Regression
#Setting the working directory
setwd("")

#Reading the data
data<-read.csv("CustomerData.csv",header=T,sep=",")
str(data)
test<-read.csv("Eval.csv",header=T)
data$City<-as.factor(data$City)
test$City<-as.factor(test$City)
custid<-data[,1]
data<-data[,-1]

mod_lm<-lm(TotalRevenueGenerated~.,data=data)
summary(mod_lm)
library(DMwR)
pred_1<-predict(mod_lm,newdata=data)
regr.eval(data$TotalRevenueGenerated,pred_1)
par(mfrow=c(2,2))
plot(mod_lm)
pred_2<-predict(mod_lm,newdata=test)
regr.eval(test$TotalRevenueGenerated,pred_2)

