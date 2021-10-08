######Simple linear Regression- 
#Plan- Consider a data set-- simple linear regression, identify outliers, influentiential observations leverages and 
# remove those observations again build the lm model- Interpretation

#For simplicity
getwd()
setwd("")
library(DMwR) # For obtaining evaluation metrics

data<-read.csv("Data_Regression.csv",header=T,sep=",")
summary(data)

#Is there any visible relationship between X and Y
plot(data$X,data$Y)

#The relation in Quantitative
cor(data$X,data$Y)

#Linear Regression model
mod_lm<-lm(Y~X,data=data)

data$pred<-predict(mod_lm,newdata = data) # These are the predicted values
regr.eval(data$Y,data$pred)

##Is the model significant
summary(mod_lm) #p value for F statistic is less than 0.05 implies that the model is better than naive one(predicting mean for any X)
                # X is significant in predicting Y as the p value for estimated slope is less than 0.005

#Looking at the R square value (0.23).  23% variance in Y is explained by X 

#From the residual plots, What is point 10. Does it have a high leverage, is it influential.
#Is it an outlier

#Plotting the residuals and checking the assumptions
par(mfrow=c(2,2))
plot(mod_lm)






