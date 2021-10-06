rm(list = ls(all=TRUE))

setwd("/Users/vishnu/Documents/GitHub/Business-Analytics-Learning")

toyota.corola.df = read.csv("ToyotaCorolla.csv")

toyota.corola.df

View(toyota.corola.df)

fuel_type_cat <- model.matrix(~0 + toyota.corola.df$Fuel_Type, data =  toyota.corola.df)
color.cat <- model.matrix(~0 + toyota.corola.df$Color , data = toyota.corola.df)

head(fuel_type_cat)
head(color.cat)


unique(toyota.corola.df$Color)

summary(toyota.corola.df)
unique(toyota.corola.df$Gears)

cor(na.omit(toyota.corola.df[,-c(1,2,5,6,8,10:12,14:16,19:39)]))

Propensity <- c(0.03, 0.52, 0.38, 0.82, 0.33, 0.42, 0.55, 0.59, 0.09, 0.21, 
                0.43, 0.04, 0.08, 0.13, 0.01, 0.79, 0.42, 0.29, 0.08, 0.02)
Actual <- c(0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
df <- cbind(Propensity, Actual)
df <- as.data.frame(df)
summary(df)

train <- sample(toyota.corolla.df$Id, 500)
test <- sample(setdiff(toyota.corolla.df$Id, train), 200) 
validation <- sample(setdiff(toyota.corolla.df$Id, union(test,train)), 300)

head(train)
head(test)
head(validation)

head(toyota.corola.df[train,])
head(toyota.corola.df[test,])
head(toyota.corola.df[validation,])

install.packages("caret")

library(caret)

confusionMatrix(factor(ifelse(df$Propensity>0.25, 1, 0)),
                factor(df$Actual), positive = "1")

confusionMatrix(factor(ifelse(df$Propensity>0.5, 1, 0)), 
                factor(df$Actual), positive = "1")

confusionMatrix(factor(ifelse(df$Propensity>0.75, 1, 0)), 
                factor(df$Actual), positive = "1")

install.packages("gains")
library(gains)

gain <- gains(df$Actual, df$Propensity, groups = 10)
#gain
barplot(gain$mean.resp/mean(df$Actual), 
        names.arg = gain$depth,
        xlab = "Depth of File", 
        ylab = "Mean Response", 
        main = "Decile-wise lift chart")

