
# Lets start by creating two large datasets
data1  <- rnorm(50000,sd=1)  #Random data with Standard deviation 1
data2 <- rnorm(50000,sd=2)   #Random data with Standard deviation 2

#Lets compute variance
var(data1)    # This value should be approximately 1^2  = 1
var(data2)    # This value should be approximately 2^2  = 4

hist(data1,200)

data3 <- data1+data2  # Create a new data set which is the sum of previous 2 datasets

#Lets compute standard deviation of the new dataset
var3 <- var(data3)  # This should be approx 1 + 4 = 5 (sum of two individual variances)
hist(data3,200)

x11()
data4 <- data1 - data2  #Now lets create a dataset thats the difference of the original 2 sets
var4 <- var(data4)   # This should be approx 1 + 4 = 5 (sum of two individual variances)
hist(data4,200)

