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

----------------------
#Lets compute Leverage values and the residual
data$lev<-(((data$X - mean(data$X))/sd(data$X))^2 +1)/36
data$resid<-mod_lm$residuals

#There is an underlying assumption in linear regression that the errors are normally distributed.
#To check this, lets perform Shapiro wilk test on the residuals
shapiro.test(data$resid)
# Shapiro-Wilk normality test
# 
# data:  data$resit
# W = 0.83897, p-value = 0.0001072 #We reject the null hypothesis that this is normally distributed

#Which datapoint has the highest leverage
data[which.max(data$lev),] 
# Observe that the point with highest leverage is point 33 which has a residual of 171.18

#From the plot, we observe that point 10 has a highest residual
data[which.max(data$resit),]
#Observe that though, this point has high residual its leverage is not very high.

#Which of these points are outliers. Lets consider cook's distance
data$cook<-round(cooks.distance(mod_lm),2)

#Influential points are those, which change the regression line too much. What if 
#we remove these points and build the lm model again

data1<-data[-c(10,33),c(1,2)]
mod_lm_1<-lm(Y~X,data=data1)
summary(mod_lm_1)
plot(data1$X,data1$Y)
par(mfrow=c(2,2))
plot(mod_lm_1)

#Observe that the model significance, variable significance and R2 values improved over the previous
#model. Now X is able to explain 31.4% variance in Y. 
data1$pred<-predict(mod_lm_1,data1)
data1$resid<-mod_lm_1$residuals

#Does this follow a nortmality assumption
shapiro.test(data1$resid)
# Shapiro-Wilk normality test
# 
# data:  data1$resid
# W = 0.97314, p-value = 0.5531 #We donot have enough evidence to reject the null hypothesis-
#Now the errors are normally distributed, which is also evident from q-q plot

data1$lev<-(((data1$X - mean(data1$X))/sd(data1$X))^2 +1)/34
data1$cook<-cooks.distance(mod_lm_1)

data1[which.max(data1$lev),] 
data1[which.max(data1$cook),]
data1[which.max(data1$resid),]
regr.eval(data1$Y,data1$pred)

##Removing another data point that has high leverage and cooks distance
data2<-data1[-30,c(1,2)] #Removing the 30th data point
mod_lm_2<-lm(Y~X,data=data2)
plot(mod_lm_2)
summary(mod_lm_2)
#Observe that the model significance, variable significance and R2 values improved over the previous
#model. Now X is able to explain 35.3% variance in Y. 
data2$pred<-predict(mod_lm_2,data2)
data2$resid<-mod_lm_2$residuals
shapiro.test(data2$resid)
# Shapiro-Wilk normality test
# 
# data:  data2$resid
# W = 0.97818, p-value = 0.7452 #Normalcy assumption is maintained 

data2$lev<-(((data2$X - mean(data2$X))/sd(data2$X))^2 +1)/33
3*mean(data2$lev)
data2[which.max(data2$lev),]

regr.eval(data2$Y,data2$pred)

# Observe the error metrics for all the three data 
# regr.eval(data$Y,data$pred) #Initial data
# mae          mse         rmse         mape 
# 7.054641e+01 1.155829e+04 1.075095e+02 5.379166e-02 
# 
# regr.eval(data1$Y,data1$pred) #data with two points with high leverage and cooks distance removed
# mae          mse         rmse         mape 
# 5.450148e+01 4.827898e+03 6.948307e+01 4.379498e-02
# 
# regr.eval(data2$Y,data2$pred) #Another point with cooks distance>0.5 removed
# mae          mse         rmse         mape 
# 5.335098e+01 4.484977e+03 6.696997e+01 4.286107e-02


#We removed 3 points which had high leverage and high cooks distance. 
#now observing the Q-Q plots and result from Shapiro Wilk test, the errors appear to be normal.
#In some instance, the model is predicting high values. Also, observe that the region in which 
#we have high error is the region where we have too few values.

#We can check the following:
#1. Are these values/data points belong to same sample. For example, we are experimenting for car
# and collected the data for buses !! 
#2. If these are genuine points, then can we get more number of data points in this region
#3. We analysed the data with points included and points removed. we present both these analyses 
# and let the client decide if these points are important.

#Since this is a univariate analysis, we are able to visualize, look at the data points and
#perform the analysis. In multilinear regression, when we have many independent variables, it is
#difficult to visualize the influences so we rely on computations to check influences.







