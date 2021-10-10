library(forecast)



sales.df <- read.csv("ShampooSales.csv")
#create the time series object
sales.ts <- ts(sales.df$Shampoo.Sales, 
               start = c(1995, 1), end = c(1997, 12), freq = 12)



#plot the original series
plot(sales.ts, xlab = "years", ylab = "Sales",
     main = "Time plot of monthly sales of a certain shampoo\n over a 3 year period")




#change the scale of the series
plot(sales.ts, xlab = "Quarter", ylab = "Sales", log = 'xy',
     main = "Time plot of log of monthly sales of a certain shampoo \nover a 3 year period")





#load the data
sep.df <- read.csv("Sept11Travel.csv")

#create time series object for Air
air.ts <- ts(sep.df$Air.RPM..000s., start = c(1990, 1), end = c(2004, 4), 
             freq = 12)

tts<-ts(sep.df$Air.RPM..000s., start =c(1990,1), end=c(2001,8), freq= 12)

#partition the data
nValid <- 32
nTrain <- length(air.ts) - nValid
train.ts <- window(air.ts, start = c(1990, 1), end = c(1990,nTrain))
valid.ts <- window(air.ts, start = c(1990, nTrain + 1), end= c(1990, nTrain + nValid))





# pre event data
options(scipen=10)
air.ts <- ts(sep.df$Air.RPM..000s., 
             start = c(1990, 1), end = c(2001, 9), freq = 12)
plot(air.ts, xlab = "Time", ylab = "Air RPM (000s)", 
     main="Time plot of Air RMP (000s) for pre-event series")




library(devtools)
library(ggpubr)
