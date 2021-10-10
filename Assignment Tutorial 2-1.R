library(leaps)
library(gains)
library(forecast)
install.packages("corrgram")
library(corrgram)

tayko.df <- read.csv("Tayko.csv")

tayko.df <- tayko.df [ , c(2,18,19,21:23,25)]

aggregate(tayko.df$Spending, list(tayko.df$Gender.male), mean)
aggregate(tayko.df$Spending, list(tayko.df$Gender.male), sd)

aggregate(tayko.df$Spending, list(tayko.df$Web.order), mean)
aggregate(tayko.df$Spending, list(tayko.df$Web.order), sd)

aggregate(tayko.df$Spending, list(tayko.df$Address_is_res), mean)
aggregate(tayko.df$Spending, list(tayko.df$Address_is_res), sd)

attach(mtcars)
par(mfrow=c(1,2))
plot(tayko.df$Freq ~ tayko.df$Spending)
plot(tayko.df$last_update_days_ago ~ tayko.df$Spending)

## Looking at the plot, we see two clusters. so we do see a relationship.But there isn't a clear LINEAR relationship from visual representation. 


## C

set.seed(1)
train.index <- sample(c(1:dim(tayko.df)[1]), 0.6*dim(tayko.df)[1])

valid.index <- setdiff(c(1:dim(tayko.df)[1]), train.index)

train.df <- tayko.df[train.index,]
valid.df <- tayko.df[valid.index,]

reg <- lm(Spending ~ Gender.male + Web.order + Address_is_res + US + Freq + last_update_days_ago, data = train.df)

summary(reg)

step(reg, direction = "backward")

#Gender is the first one dropped, significance level is 0.8, so it is removed first ( Not so significant)

first.obs <- head(valid.df,1)
dim(first.obs)



library(forecast)
#
#
#
#
#

#6.4



toyota.corolla <- read.csv("ToyotaCorolla.csv")

train.index <- sample(c(1:dim(toyota.corolla)[1]), 
                      0.5*dim(toyota.corolla)[1])  
remaining.index <- setdiff(c(1:dim(toyota.corolla)[1]), train.index)  

valid.index <- sample(remaining.index, 
                      0.3*dim(toyota.corolla)[1])
test.index <- setdiff(remaining.index, valid.index)

train.df <- toyota.corolla[train.index, ]
valid.df <- toyota.corolla[valid.index, ]
test.df <- toyota.corolla[test.index, ]

toyota.reg <- lm(train.df$Price ~ ., data = train.df)

summary(toyota.reg)


#################

library(forecast)



sales.df <- read.csv("ShampooSales.csv")
#create the time series object
sales.ts <- ts(sales.df$Shampoo.Sales, 
               start = c(1995, 1), end = c(1997, 12), freq = 12)



#plot the original series
dev.new(width=5, height=4)

plot(sales.ts, xlab = "years", ylab = "Sales",
     main = "Time plot of monthly sales of a certain shampoo\n over a 3 year period")


#change the scale of the series
plot(sales.ts, xlab = "Quarter", ylab = "Sales", log = 'xy',
     main = "Time plot of log of monthly sales of a certain shampoo \nover a 3 year period")


### C




