setwd("~/Sridhar_CDrive/Desktop/Desktop_20150323/Batch 17/CSE 7315c/Day03")

# Calculating probabilities and corresponding values

1 - pnorm(64, mean=71, sd=sqrt(20.25))
1 - pnorm(65,71,4.5)
1 - pnorm(69, 71, sqrt(20.25))
qnorm(0.0594,71,4.5)
qnorm(0.05990691,71,4.5)
qnorm(0.20,71,4.5)
pnorm(80.5, 90, sqrt(9))
qnorm(0.1,0,1)
qnorm(0.3,0,1)

# Percentile or Percent
qnorm(0.8, 0, 1)
qnorm(0.975, 0, 1)
qnorm(0.025, 0, 1)
qnorm(0.05, 0, 1)
qnorm(0.95, 0, 1)

# Hadlum vs Hadlum case

pregnancy <- read.csv("HadlumPregnancy.csv", header=T, sep=",")
pregnancy = pregnancy[,-2]
summary(pregnancy)
var(pregnancy)
mean(pregnancy)
pnorm(310, mean(pregnancy), sd(pregnancy))
plot(pregnancy)

pnorm(5.9,6,sqrt(3))
