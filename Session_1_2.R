# Simple strategies for dimension reduction
library(reshape)
#### Table 4.3
boston.housing.df <- read.csv("BostonHousing.csv", header = TRUE)
head(boston.housing.df, 9)
summary(boston.housing.df)

# compute mean, standard dev., min, max, median, length, and missing values of CRIM
mean(boston.housing.df$CRIM)
sd(boston.housing.df$CRIM)
min(boston.housing.df$CRIM)
max(boston.housing.df$CRIM)
median(boston.housing.df$CRIM)
length(boston.housing.df$CRIM)

# find the number of missing values of variable CRIM
sum(is.na(boston.housing.df$CRIM))

# compute mean, standard dev., min, max, median, length, and missing values for all
# variables
data.frame(mean=sapply(boston.housing.df, mean),
           sd=sapply(boston.housing.df, sd),
           min=sapply(boston.housing.df, min),
           max=sapply(boston.housing.df, max),
           median=sapply(boston.housing.df, median),
           length=sapply(boston.housing.df, length),
           miss.val=sapply(boston.housing.df, function(x)
             sum(length(which(is.na(x))))))



#### Table 4.4
round(cor(boston.housing.df),2)

#### Table 4.5
table(boston.housing.df$CHAS)

#### Table 4.6
# create bins of size 1
boston.housing.df$RM.bin <- .bincode(boston.housing.df$RM, c(1:9))

# compute the average of MEDV by (binned) RM and CHAS
# in aggregate() use the argument by= to define the list of aggregating variables,
# and FUN= as an aggregating function.
aggregate(boston.housing.df$MEDV, by=list(RM=boston.housing.df$RM.bin,
                                          CHAS=boston.housing.df$CHAS), FUN=mean)

#### Table 4.7
boston.housing.df <- read.csv("BostonHousing.csv")
# create bins of size 1
boston.housing.df$RM.bin <- .bincode(boston.housing.df$RM, c(1:9))

# use melt() to stack a set of columns into a single column of data.
# stack MEDV values for each combination of (binned) RM and CHAS
mlt <- melt(boston.housing.df, id=c("RM.bin", "CHAS"), measure=c("MEDV"))
head(mlt, 5)

# use cast() to reshape data and generate pivot table
cast(mlt, RM.bin ~ CHAS, subset=variable=="MEDV",
     margins=c("grand_row", "grand_col"), mean)


# Principle Components Analysis ----
decathlon <- read.csv(file = "decathlon_data.txt", sep="\t", header = TRUE)
round(cor(decathlon),2)
heatmap(cor(decathlon))

correlates <- decathlon[,c("X100m", "X110h", "X400m","X1500m")]
decathlon_eigen <- eigen(cor(correlates))
decathlon_eigen$values
sum(decathlon_eigen$values)
decathlon_eigen$values / sum(decathlon_eigen$values)
decathlon_eigen$vectors


dec_pca <- prcomp(decathlon, scale. = TRUE)
screeplot(dec_pca, type="lines")
round(dec_pca$rotation, 2)
summary(dec_pca)

dec_scores <- dec_pca$x
plot(dec_scores, pch=19)

decathlon$PC1 <- dec_scores[, "PC1"]
decathlon$PC2 <- dec_scores[, "PC2"]
decathlon$PC3 <- dec_scores[, "PC3"]
decathlon$PC4 <- dec_scores[, "PC4"]

# Evaluating Predictive Performance ----

#### Table 5.1

# package forecast is required to evaluate performance
library(forecast)

# load file
toyota.corolla.df <- read.csv("ToyotaCorolla.csv")

# remove missing Price data
toyota.corolla.df <-
  toyota.corolla.df[!is.na(toyota.corolla.df[validation,]$Price),]

# randomly generate training and validation sets
training <- sample(toyota.corolla.df$Id, 600)
validation <- sample(setdiff(toyota.corolla.df$Id, training), 400)

# run linear regression model
reg <- lm(Price~., data=toyota.corolla.df[,-c(1,2,6,8,11,15,37,38)], subset=training,
          na.action=na.exclude)
pred_t <- predict(reg, na.action=na.pass)
pred_v <- predict(reg, newdata=toyota.corolla.df[validation,-c(1,2,6,8,11,15,37,38)], na.action=na.pass)

## evaluate performance
# training
accuracy(pred_t, toyota.corolla.df[training,]$Price)
# validation
accuracy(pred_v, toyota.corolla.df[validation,]$Price)

#### Figure 5.2

# generate random Training and Validation sets
training <- sample(toyota.corolla.df$Id, 600)
validation <- sample(toyota.corolla.df$Id, 400)

# regression model based on all numerical predictors
reg <- lm(Price~., data = toyota.corolla.df[,-c(1,2,6,8,11,15,37,38)], subset = training)

# predictions
pred_v <- predict(reg, newdata = toyota.corolla.df[validation,-c(1,2,6,8,11,15,37,38)])

# load package gains, compute gains (we will use package caret for categorical y later)
library(gains)
gain <- gains(toyota.corolla.df[validation,]$Price[!is.na(pred_v)], pred_v[!is.na(pred_v)])

# cumulative lift chart
options(scipen=999) # avoid scientific notation
# we will compute the gain relative to price
price <- toyota.corolla.df[validation,]$Price[!is.na(toyota.corolla.df[validation,]$Price)]
plot(c(0,gain$cume.pct.of.total*sum(price))~c(0,gain$cume.obs),
     xlab="# cases", ylab="Cumulative Price", main="Lift Chart", type="l")

# baseline
lines(c(0,sum(price))~c(0,dim(toyota.corolla.df[validation,])[1]), col="gray", lty=2)

# Decile-wise lift chart
barplot(gain$mean.resp/mean(price), names.arg = gain$depth,
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")




#### Table 5.5
library(caret)
library(e1071)

owner.df <- read.csv("ownerExample.csv")
confusionMatrix(as.factor(ifelse(owner.df$Probability>0.5, 'owner', 'nonowner')),
                as.factor(owner.df$Class))
confusionMatrix(as.factor(ifelse(owner.df$Probability>0.25, 'owner', 'nonowner')),
                as.factor(owner.df$Class))
confusionMatrix(as.factor(ifelse(owner.df$Probability>0.75, 'owner', 'nonowner')),
                as.factor(owner.df$Class))



#### Figure 5.4

# replace data.frame with your own
df <- read.csv("liftExample.csv")


# create empty accuracy table
accT = c()

# compute accuracy per cutoff
for (cut in seq(0,1,0.1)){
  cm <- confusionMatrix(as.factor(1 * (df$prob > cut)), as.factor(df$actual))
  accT = c(accT, cm$overall[1])
}

# plot accuracy
plot(accT ~ seq(0,1,0.1), xlab = "Cutoff Value", ylab = "", type = "l", ylim = c(0, 1))
lines(1-accT ~ seq(0,1,0.1), type = "l", lty = 2)
legend("topright",  c("accuracy", "overall error"), lty = c(1, 2), merge = TRUE)



#### Figure 5.5

library(pROC)
r <- roc(df$actual, df$prob)
plot.roc(r)

# compute auc
auc(r)



#### Figure 5.6

# first option with 'caret' library:
library(caret)
lift.example <- lift(relevel(as.factor(actual), ref="1") ~ prob, data = df)
xyplot(lift.example, plot = "gain")

# Second option with 'gains' library:
library(gains)
df <- read.csv("liftExample.csv")
gain <- gains(df$actual, df$prob, groups=dim(df)[1])
plot(c(0, gain$cume.pct.of.total*sum(df$actual)) ~ c(0, gain$cume.obs),
     xlab = "# cases", ylab = "Cumulative", type="l")
lines(c(0,sum(df$actual))~c(0,dim(df)[1]), col="gray", lty=2)




#### Figure 5.7

# use gains() to compute deciles.
# when using the caret package, deciles must be computed manually.

gain <- gains(df$actual, df$prob)
barplot(gain$mean.resp / mean(df$actual), names.arg = gain$depth, xlab = "Percentile",
        ylab = "Mean Response", main = "Decile-wise lift chart")



