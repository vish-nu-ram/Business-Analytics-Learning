rm(list=ls(all=TRUE))
setwd("D:/Rennes/CSE7302c/Day07")

#TIME SERIES FORECASTING

library("forecast")
library("stats")
library("data.table")
library("TTR")

# Microsoft stock price over 5 years from 19-Dec-2011 to 16-Dec-2016
MSStock <- fread("Microsoft-5year.csv",header = T, sep = ",", select = c("AdjCloseUSD") )
MSStock

MStimeseries <- ts(MSStock)
MStimeseries

plot(MStimeseries)

#For monthly time series data, 
#you set frequency=12, 
#while for quarterly time series data, 
#you set frequency=4

#You can also specify the first 
#year that the data was collected, 
#and the first interval in that year 
#by using the 'start'
#parameter in the ts() function. For example, if the first data 
#point corresponds to the second quarter of 1986, you would set 
#start=c(1986,2).

# Read in the US Air Carrier Traffic - Revenue Passenger Miles dataset
#   Unit: Thousand Miles
#   Data Source: http://www.bts.gov/xml/air_traffic/src/index.xml
#   and https://datamarket.com/data/set/281x/us-air-carrier-traffic-statistics-revenue-passenger-miles 

miles = read.csv("us-air-carrier-traffic-statistic.csv")
miles

milestimeseries <- ts(miles, frequency = 12, start = c(1996,1))
milestimeseries
par(mfrow=c(1,1))
plot(milestimeseries)

#Regression on time

sales <- read.csv("Sales_RegressionOnTime.csv")
sales
salestimeseries <- ts(sales, frequency = 4)
salestimeseries
plot(salestimeseries)
sales <- data.frame(sales)
sales$time <- seq(1:20)
edit(sales)
plot(sales$Sales_.1000, type="l")
saleslm1 <- lm(sales$Sales_.1000 ~ sales$time)
saleslm2 <- lm(sales$Sales_.1000 ~ 
                 poly(sales$time, 2, raw=TRUE))
points(sales$time, predict(saleslm1), 
       type="l", col="red", lwd=2)
points(sales$time, predict(saleslm2), 
       type="l", col="blue", lwd=2)

# Incorporating seasonality
sales$seasonal <- as.factor(rep(c(1:4),5))
edit(sales)
sales
saleslm2s <- lm(sales$Sales_.1000 ~ poly(time, 2, raw=TRUE)+
                  seasonal, data=sales)

plot(sales$Sales_.1000, type="l")
points(sales$time, predict(saleslm2s), 
       type="l", col="blue", lwd=2)

miles <- data.frame(miles)
miles$time <- seq(1:204)
edit(miles)
plot(miles$miles, type="l")
lm1 <- lm(miles$miles ~ miles$time)
lm2 <- lm(miles$miles ~ 
            poly(miles$time, 2, raw=TRUE))
lm3 <- lm(miles$miles ~ 
            poly(miles$time, 3, raw=TRUE))

points(miles$time, predict(lm1), 
       type="l", col="red", lwd=2)
points(miles$time, predict(lm2), 
       type="l", col="green", lwd=2)
points(miles$time, predict(lm3), 
       type="l", col="blue", lwd=2)

miles$seasonal <- as.factor(rep(c(1:12),17))
edit(miles)

lm1s <- lm(miles ~ ., data=miles)
lm2s <- lm(miles ~ poly(time, 2, raw=TRUE)+
             seasonal, data=miles)
lm3s <- lm(miles ~ poly(time, 3, raw=TRUE)+
             seasonal, data=miles)

plot(miles$miles, type="l")
points(miles$time, predict(lm1s), 
       type="l", col="red", lwd=2)
points(miles$time, predict(lm2s), 
       type="l", col="blue", lwd=2)

# Incorporating Seasonality - Another approach

miles$mae <- miles$miles/predict(lm1)
edit(miles)

seasonal <- tapply(miles$mae, 
                   miles$seasonal, mean)
seasonal

milespr <- predict(lm1)*rep(seasonal,17)

plot(miles$miles, type="l")
points(miles$time, milespr, 
       type="l", col="red", lwd=2)

miles$mae <- miles$miles-predict(lm1)
edit(miles)

seasonalAdd <- tapply(miles$mae, 
                      miles$seasonal, mean)
seasonalAdd

milespr <- predict(lm1)+rep(seasonalAdd,17)

plot(miles$miles, type="l")
points(miles$time, milespr, 
       type="l", col="blue", lwd=2)

# Advanced models
# Holt-Winters

#Important Concepts - Decomposition and Moving averages
#Decomposition

milestimeseriescomponents <- 
  decompose(milestimeseries, type = "additive")

plot(milestimeseriescomponents)
milestimeseriescomponents$seasonal
milestimeseriescomponents$trend
milestimeseriescomponents$figure

par(mfrow=c(1,1))

#We can specify the initial 
#value and slope

#Trend and Additive Seasonality model
miles = read.csv("us-air-carrier-traffic-statistic.csv")
miles
milestimeseries <- ts(miles, frequency = 12, start = c(1996,1))
milestimeseries
plot(milestimeseries)
milesforecast <- 
  HoltWinters(milestimeseries)
milesforecast
plot(milesforecast)
milesforecast$SSE
milesresiduals <- residuals(milesforecast)
milesresiduals
par(mfrow = c(1,1))
plot(milesresiduals)
par(mfrow = c(1,2))
acf(milesresiduals)
pacf(milesresiduals)
Box.test(milesresiduals, lag=24, type="Ljung-Box")

#Trend and Multiplicative Seasonality model
milesforecastMult <- 
  HoltWinters(milestimeseries,seasonal = "multiplicative")
milesforecastMult
milesforecast
milesforecast$fitted


#it predicts seasonal peaks well
milesforecast2 <- 
  forecast.HoltWinters(milesforecast, 
                       h=36)

milesforecast2
par(mfrow = c(1, 1))
plot.forecast(milesforecast2,
              shadecols="oldstyle")

#ARIMA
#Important Concepts - ACF, PACF and Stationarity

#Simulated data to understand ACF & PACF

par(mfrow=c(1,1))
time <- c(1:100)
growthT <- time
plot(growthT~time)

par(mfrow=c(1,2))

growthT <- ts(growthT)
acf(growthT)
pacf(growthT)

par(mfrow=c(1,1))
time <- c(1:100)
growthS <- time
growthS <- sin(growthS*pi*12/180)
plot(growthS~time, type="l")

par(mfrow=c(1,2))

growthS <- ts(sin(growthS))
acf(growthS)
pacf(growthS)

par(mfrow=c(1,1))
time <- c(1:100)
growthR <- time
growthR <- runif(100, min=0, max=1)
plot(growthR~time, type="l")

par(mfrow=c(1,2))

growthR <- ts(runif(growthR))
acf(growthR)
pacf(growthR)
#ACF and PACF of real world data

par(mfrow=c(1,3))
plot.ts(milestimeseries)
acf(milestimeseries, lag.max=20)
pacf(milestimeseries, lag.max=20)
#acf(milestimeseries, lag.max=20, ci.type="ma")

# Differencing and ACF, PACF on
# Stationary and Non-Stationary Data

par(mfrow=c(1,2))
MSacf <- acf(MStimeseries, lag.max=20)
MSpacf <- pacf(MStimeseries, lag.max=20)
MSacf
MSpacf
MSAcf <- Acf(MStimeseries, lag.max = 20)
MSPacf <- Pacf(MStimeseries, lag.max = 20)
MSAcf
MSPacf

MStimeseriesdiff1 <- diff(MStimeseries, differences=1)
MStimeseriesdiff1

MStimeseriesdiff2 <- diff(MStimeseries, differences=2)
MStimeseriesdiff2

acf(MStimeseriesdiff1, lag.max=20)
pacf(MStimeseriesdiff1, lag.max=20)

plot(MStimeseriesdiff1)
plot(MStimeseriesdiff2)

ndiffs(MStimeseries)
ndiffs(milestimeseries)
nsdiffs(milestimeseries)

par(mfrow=c(1,2))
acf(milestimeseries, lag.max=20)
pacf(milestimeseries, lag.max=20)

milestimeseriesdiff1 <- diff(milestimeseries, differences=1)
milestimeseriesdiff1
acf(milestimeseriesdiff1, lag.max=20)
pacf(milestimeseriesdiff1, lag.max=20)

milestimeseriesdiff2 <- diff(milestimeseries, differences=2)
milestimeseriesdiff2
acf(milestimeseriesdiff2, lag.max=20)
pacf(milestimeseriesdiff2, lag.max=20)

#Step-by-step ARIMA model building
# Model 1
# Step 1: Plot timeseries (in terms of ARIMA, it is an ARIMA(0,0,0))
miles = read.csv("us-air-carrier-traffic-statistic.csv")
miles
milestimeseries <- ts(miles, frequency = 12, start = c(1996,1))
milestimeseries
par(mfrow = c(1, 1))
plot(milestimeseries)

# Step 2: Plot ACF and PACF to get preliminary understanding of the process
par(mfrow = c(1, 2))
acf(milestimeseries)
pacf(milestimeseries)

# Step 3: The suspension bridge pattern in ACF suggests both nonstationarity
# and strong seasonality.  Perform a seasonal difference (ARIMA(0,0,0)(0,1,0)12)
par(mfrow = c(1, 1))
milestimeseriesseasonaldiff1 <- 
  diff(milestimeseries, lag = 12, differences=1)
milestimeseriesseasonaldiff1
plot(milestimeseriesseasonaldiff1)

# Step 4: Check ACF and PACF for seasonally differenced data
#to explore remaining dependencies
par(mfrow = c(1, 2))
acf(milestimeseriesseasonaldiff1)
pacf(milestimeseriesseasonaldiff1)

# Step 5: Strong positive autocorrelation indicates need for either an AR component
# or a non-seasonal differencing.  Perform a non-seasonal differencing.
# ARIMA(0,1,0)(0,1,0)12
par(mfrow = c(1, 1))
milestimeseriesSeasNoSeasdiff1 <- 
  diff(milestimeseriesseasonaldiff1, differences=1)
milestimeseriesSeasNoSeasdiff1
plot(milestimeseriesSeasNoSeasdiff1)

# Step 6: Check ACF and PACF to explore remaining dependencies
par(mfrow = c(1, 2))
acf(milestimeseriesSeasNoSeasdiff1)
pacf(milestimeseriesSeasNoSeasdiff1)

# Step 7: ACF and PACF show significant NEGATIVE lag-1, which then cutoff, requiring
# an MA(1) term.  Also, the significant lag at the seasonal
# period is NEGATIVE, requiring a SeasonalMA(1) term
milesArima1 <- Arima(milestimeseries, order = c(0,1,1),
                     seasonal = c(0,1,1), include.drift = TRUE)
milesArima1

# Step 8: Check residuals to ensure they are white noise
par(mfrow = c(1, 2))
acf(milesArima1$residuals, lag.max = 24)
pacf(milesArima1$residuals, lag.max = 24)
Box.test(milesArima1$residuals, lag=24, type="Ljung-Box")

# Step 9: Start forecasting
par(mfrow = c(1, 1))
milestimeseriesforecastsArima1 <- forecast.Arima(milesArima1, 
                                                 h=36)
plot.forecast(milestimeseriesforecastsArima1)
milestimeseriesforecastsArima1

#ARIMA - Model 2
#Step-by-step ARIMA model building
# Model 2
# Step 1: Plot timeseries (in terms of ARIMA, it is an ARIMA(0,0,0))
miles = read.csv("us-air-carrier-traffic-statistic.csv")
miles
milestimeseries <- ts(miles, frequency = 12, start = c(1996,1))
milestimeseries
par(mfrow = c(1, 1))
plot(milestimeseries)

# Step 2: Perform a seasonal differencing on the original time series (ARIMA(0,0,0)(0,1,0)12)
par(mfrow = c(1, 1))
milestimeseriesseasonaldiff1 <- 
  diff(milestimeseries, lag = 12, differences=1)
milestimeseriesseasonaldiff1
plot(milestimeseriesseasonaldiff1)

# Step 3: Check ACF and PACF for seasonally differenced data
#to explore remaining dependencies
par(mfrow = c(1, 2))
acf(milestimeseriesseasonaldiff1)
pacf(milestimeseriesseasonaldiff1)

# Step 4: Strong positive autocorrelation indicates need for either an AR component
# or a non-seasonal differencing.  Add an AR term.
# ARIMA(1,0,0)(0,1,0)12
milesArima2 <- Arima(milestimeseries, order = c(1,0,0),
                     seasonal = c(0,1,0), include.drift = TRUE)
milesArima2

# Step 5: Check ACF and PACF to explore remaining dependencies
par(mfrow = c(1, 2))
acf(milesArima2$residuals)
pacf(milesArima2$residuals)
Box.test(milesArima2$residuals, lag=24, type="Ljung-Box")

# Step 6: Strong negative autocorrelation at the seasonal period
#indicates need for a seasonal MA term. ARIMA(1,0,0)(0,1,1)12
milesArima2 <- Arima(milestimeseries, order = c(1,0,0),
                     seasonal = c(0,1,1), include.drift = TRUE)
milesArima2

# Step 7: Check ACF and PACF to explore remaining dependencies
par(mfrow = c(1, 2))
acf(milesArima2$residuals)
pacf(milesArima2$residuals)
Box.test(milesArima2$residuals, lag=24, type="Ljung-Box")

# Step 8: Start forecasting
par(mfrow = c(1, 1))
milestimeseriesforecastsArima2 <- forecast.Arima(milesArima2, 
                                                 h=36)
plot.forecast(milestimeseriesforecastsArima2)
milestimeseriesforecastsArima2

# Automated functions are available
milesAutoArima <- auto.arima(milestimeseries,ic='aic')
milesAutoArima
milestimeseriesforecastsAutoArima <- forecast.Arima(milesAutoArima, 
                                                 h=36)
plot.forecast(milestimeseriesforecastsAutoArima)
milestimeseriesforecastsAutoArima

#MSAutoArima <- auto.arima(MStimeseries, ic='aic')
#MSAutoArima
#MSforecastsAutoArima <- forecast.Arima(MSAutoArima,                                                     h=36)
#plot.forecast(MSforecastsAutoArima)
#MSforecastsAutoArima

# Manufacturing Case Study (Courtesy: http://ucanalytics.com/blogs/step-by-step-graphic-guide-to-forecasting-through-arima-modeling-in-r-manufacturing-case-study-example/)
# Step 1: Convert data into Time Series
par(mfrow = c(1, 1))
TractorSales<-read.csv("Tractor-Sales.csv")
TractorSalesTS<-ts(TractorSales[,2],start = c(2003,1),frequency = 12)
TractorSalesTS
plot(TractorSalesTS, xlab="Years", ylab = "Tractor Sales")
TractorSalesComponents <- decompose(TractorSalesTS, type = "multiplicative")
TractorSalesComponents
plot(TractorSalesComponents)

# Step 2: Difference data to make it stationary
ndiffs(TractorSalesTS)
plot(diff(TractorSalesTS),ylab="Differenced Tractor Sales")

# Step 3: As the time series is not stationary on variance, log transform the data
plot(log10(TractorSalesTS),ylab="Log (Tractor Sales)")

# Step 4: Difference log transformed data to check for stationarity
plot(diff(log10(TractorSalesTS)),ylab="Differenced Log (Tractor Sales)")

# Step 5: Build ARIMA model
TractorSalesARIMA <- auto.arima(TractorSalesTS)
TractorSalesARIMA
LogTractorSalesARIMA <- auto.arima(log10(TractorSalesTS))
LogTractorSalesARIMA

# Step 6: Check residuals to ensure they are white noise
par(mfrow=c(1,2))
acf(ts(LogTractorSalesARIMA$residuals),main="ACF Residual")
pacf(ts(LogTractorSalesARIMA$residuals),main="PACF Residual")
Box.test(LogTractorSalesARIMA$residuals, lag=24, type="Ljung-Box")

# Step 7: Forecast Tractor Sales
par(mfrow = c(1, 1))
TractorSalesForecasts <- forecast.Arima(LogTractorSalesARIMA, 
                                        h=36)
plot.forecast(TractorSalesForecasts, shadecols = "oldstyle")
TractorSalesForecasts

# Alternative method
pred <- predict(LogTractorSalesARIMA, n.ahead=36)
pred

plot(TractorSalesTS,type="l",xlim=c(2004,2018),ylim=c(1,1600),
     xlab = "Year",ylab = "Tractor Sales")
lines(10^(pred$pred),col="blue")
lines(10^(pred$pred+1.96*pred$se),col="red")
lines(10^(pred$pred-1.96*pred$se),col="green")

