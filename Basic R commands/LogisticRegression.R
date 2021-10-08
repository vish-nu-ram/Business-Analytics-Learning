setwd("~/Sridhar_CDrive/Desktop/Desktop_20150323/Batch 47/CSE 7302c/Day03")

# LOGISTIC REGRESSION

# Read in Flier Response Data
flierresponse <- read.csv("FlierResponse.csv", header = T, sep = ",")
flierresponse
str(flierresponse)
flierresponse$Response <- as.factor(flierresponse$Response)
str(flierresponse)
flierresponseglm <- glm(Response~Age, data = flierresponse, family = "binomial")
nd <- data.frame(Age=50)  # To predict the probability for Age=50, put that info in a data-frame
predict(flierresponseglm,newdata=nd)
predict(flierresponseglm,newdata=nd,type="response")
flierresponseglm
summary(flierresponseglm)
logLik(flierresponseglm)
deviance(flierresponseglm)
AIC(flierresponseglm)

# CASE STUDY - The Framingham Heart Study

# Read in the Framingham dataset
framingham = read.csv("framingham.csv")
View(framingham)

# Look at structure
str(framingham)
framingham$education = factor(framingham$education)
str(framingham)

# Load the library caTools
library(caTools)
#library(DAAG)
#library(rms)
library(car)

# Randomly split the data into training and testing sets
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.70)

# Split up the data using subset
train = subset(framingham, split==TRUE)
test = subset(framingham, split==FALSE)

# Check the proportions of CHD in both sets
cat(sum(train$TenYearCHD)/nrow(train),sum(test$TenYearCHD)/nrow(test))

# Logistic Regression Model
framinghamLog = glm(TenYearCHD ~ ., data = train, family=binomial)
summary(framinghamLog)
car::vif(framinghamLog)
#rms::vif(framinghamLog)
#DAAG::vif(framinghamLog)
#framinghamLogDispersion = glm(TenYearCHD ~ ., data = train, family=quasibinomial)
#summary(framinghamLogDispersion)
#car::vif(framinghamLogDispersion)

#Accuracy on the training set
predictTrain = predict(framinghamLog, type="response", newdata=train)

# Confusion matrix with threshold of 0.5
table(train$TenYearCHD, predictTrain > 0.5)

# Accuracy on Train Set
(2170+30)/(2170+30+357+9)

# Predictions on the test set
predictTest = predict(framinghamLog, type="response", newdata=test)

# Confusion matrix with threshold of 0.5
table(test$TenYearCHD, predictTest > 0.5)

# Accuracy on Test Set
(915+12)/(915+12+158+7)

# Confusion matrix with threshold of 0.9
table(test$TenYearCHD, predictTest > 0.9)
# Confusion matrix with threshold of 0.7
table(test$TenYearCHD, predictTest > 0.7)
# Confusion matrix with threshold of 0.5
table(test$TenYearCHD, predictTest > 0.5)
# Confusion matrix with threshold of 0.3
table(test$TenYearCHD, predictTest > 0.3)
# Confusion matrix with threshold of 0.1
table(test$TenYearCHD, predictTest > 0.1)

# Test set AUC 
library(ROCR)
ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
par(mfrow=c(1,1))
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

