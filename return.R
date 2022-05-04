## MSc Business Analitics
##Financial Modelling and Analysis


require (xts) # attache the package to convert our dataframes into an xts (time-series) object for simplicity in calculations

#setwd("~/R/Trinity/Session1") # setting a working directory to access cvs data file
prices = read.csv("BitcoinPrices.csv", header = TRUE, sep = ",") # import prices
prices$Time = as.POSIXct(prices$Time,format="%Y-%m-%d", tz = "") # converting a date column into date format 
prices.xts <- xts(prices[,-1], order.by=prices[,1]) # converting a data frame into xts object (time-series object)
plot (prices.xts$coinbase)

#return calculation

#log return
return = diff (log(prices.xts)) # Log return calculation
return = return [-1] # removing the first empty observation, received after return calculation
summary (return)
plot(return$coinbase)

# it will be the same as :
library(PerformanceAnalytics)
return2 = Return.calculate(prices.xts, method="log")

return3 = diff (prices.xts) # differences
# the sane as 
return4 = Return.calculate(prices.xts, method="difference")


#daily gain/loss (%)
return5 = diff(prices.xts)/prices.xts[-length(prices.xts)]



