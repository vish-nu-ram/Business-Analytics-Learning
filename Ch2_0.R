#### Table 2.3

housing.df <- read.csv("WestRoxbury.csv", header = TRUE)  # load data
dim(housing.df)  # find the dimension of data frame
head(housing.df)  # show the first six rows
View(housing.df)  # show all the data in a new tab

# Practice showing different subsets of the data
housing.df[1:10, 1]  # show the first 10 rows of the first column only
housing.df[1:10, ]  # show the first 10 rows of each of the columns
housing.df[5, 1:10]  # show the fifth row of the first 10 columns
housing.df[5, c(1:2, 4, 8:10)]  # show the fifth row of some columns
housing.df[, 1]  # show the whole first column
housing.df$TOTAL.VALUE  # a different way to show the whole first column
housing.df$TOTAL.VALUE[1:10]  # show the first 10 rows of the first column
length(housing.df$TOTAL.VALUE)  # find the length of the first column
mean(housing.df$TOTAL.VALUE)  # find the mean of the first column
summary(housing.df)  # find summary statistics for each column
round(cor(housing.df[,-14]), 2)
round(colMeans(housing.df[,-14]),2)

#### Table 2.4

# random sample of 500 observations
s <- sample(row.names(housing.df), 500)
round(cor(housing.df[1:500,-14]),2)
round(colMeans(housing.df[1:500,-14]),2)

# oversample houses with over 10 rooms
s1 <- sample(row.names(housing.df), 10, prob = ifelse(housing.df$ROOMS>10, 0.9, 0.01))
s2 <- sample(row.names(housing.df), 10)

# housing.df[s,]
round(cor(housing.df[s,-14]),2)
round(colMeans(housing.df[s,-14]),2)

#### Table 2.5

names(housing.df)  # print a list of variables to the screen.
t(t(names(housing.df)))  # print the list in a useful column format
colnames(housing.df)[1] <- c("TOTAL_VALUE")  # change the first column's name
class(housing.df$REMODEL) # REMODEL is a factor variable
class(housing.df[ ,14]) # Same.
levels(housing.df[, 14])  # It can take one of three levels
class(housing.df$BEDROOMS)  # BEDROOMS is an integer variable
class(housing.df[, 1])  # Total_Value is a numeric variable



#### Table 2.6
# Option 1: use dummies package
# install.packages(dummies)
library(dummies)
housing.df <- dummy.data.frame(housing.df, sep = ".")
names(housing.df)

# Option 2: use model.matrix() to convert all categorical variables in the data frame into a set of dummy variables. We must then turn the resulting data matrix back into
# a data frame for further work.
housing.df <- read.csv("WestRoxbury.csv", header = TRUE)  # load data
xtotal <- model.matrix(~ 0 + REMODEL, data = housing.df)
xtotal <- as.data.frame(xtotal)
t(t(names(xtotal)))  # check the names of the dummy variables
head(xtotal)

#### Outliers
summary(housing.df)
boxplot(housing.df$TOTAL.VALUE)
plot(density(housing.df$TOTAL.VALUE))
abline(v = quantile(housing.df$TOTAL.VALUE, probs = c(0.001, 0.999)))
plot(density(scale(housing.df$TOTAL.VALUE)))

#### Table 2.7

# To illustrate missing data procedures, we first convert a few entries for
# bedrooms to NA's. Then we impute these missing values using the median of the
# remaining values.
rows.to.missing <- sample(row.names(housing.df), 10)
housing.df[rows.to.missing,]$BEDROOMS <- NA
summary(housing.df$BEDROOMS)  # Now we have 10 NA's and the median of the
# remaining values is 3.

# replace the missing values using the median of the remaining values.
# use median() with na.rm = TRUE to ignore missing values when computing the median.
housing.df[rows.to.missing,]$BEDROOMS <- median(housing.df$BEDROOMS, na.rm = TRUE)

summary(housing.df$BEDROOMS)




#### Table 2.9

# use set.seed() to get the same partitions when re-running the R code.
set.seed(1)

## partitioning into training (60%) and validation (40%)
# randomly sample 60% of the row IDs for training; the remaining 40% serve as
# validation
train.rows <- sample(rownames(housing.df), dim(housing.df)[1]*0.6)
# collect all the columns with training row ID into training set:
train.data <- housing.df[train.rows, ]
# assign row IDs that are not already in the training set, into validation
valid.rows <- setdiff(rownames(housing.df), train.rows)
valid.data <- housing.df[valid.rows, ]

# alternative code for validation (works only when row names are numeric):
# collect all the columns without training row ID into validation set
valid.data <- housing.df[-train.rows, ] # does not work in this case

## partitioning into training (50%), validation (30%), test (20%)
# randomly sample 50% of the row IDs for training
train.rows <- sample(rownames(housing.df), dim(housing.df)[1]*0.5)

# sample 30% of the row IDs into the validation set, drawing only from records
# not already in the training set
# use setdiff() to find records not already in the training set
valid.rows <- sample(setdiff(rownames(housing.df), train.rows),
                     dim(housing.df)[1]*0.3)

# assign the remaining 20% row IDs serve as test
test.rows <- setdiff(rownames(housing.df), union(train.rows, valid.rows))

# create the 3 data frames by collecting all columns from the appropriate rows
train.data <- housing.df[train.rows, ]
valid.data <- housing.df[valid.rows, ]
test.data <- housing.df[test.rows, ]


#### Table 2.11

reg <- lm(TOTAL.VALUE ~ .-TAX, data = housing.df, subset = train.rows) # remove variable "TAX"
tr.res <- data.frame(train.data$TOTAL.VALUE, reg$fitted.values, reg$residuals)
head(tr.res)



#### Table 2.12

pred <- predict(reg, newdata = valid.data)
vl.res <- data.frame(valid.data$TOTAL.VALUE, pred, residuals =
                       valid.data$TOTAL.VALUE - pred)
head(vl.res)



#### Table 2.13

library(forecast)
# compute accuracy on training set
accuracy(reg$fitted.values, train.data$TOTAL.VALUE)

# compute accuracy on prediction set
pred <- predict(reg, newdata = valid.data)
accuracy(pred, valid.data$TOTAL.VALUE)
