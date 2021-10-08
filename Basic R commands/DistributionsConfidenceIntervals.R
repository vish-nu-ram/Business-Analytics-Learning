setwd("~/Sridhar_CDrive/Desktop/Desktop_20150323/Corporate Training/Deloitte/Hyderabad/201605-06/Day02")

# Confidence Intervals in t-distribution

potency <- c(98.6,102.1,100.7,102.0,97.0,103.4,98.9,101.6,102.9,105.2)
potency
t.test(potency, mu=100, conf.level = 0.90)
ttest <- t.test(potency, mu=100)
ttest$statistic
sd(potency)
ttest$parameter

# Critical t value at a specified confidence level and degrees of freedom
conf.level = 0.90
df = 17
qt((1-conf.level)/2, df)
qt(0.025,24)
qt(0.025, 19)
qt(0.975, 19)

z = qnorm((1 - conf.level)/2, lower.tail = FALSE)
xbar = 80
sdx = 4
c(xbar - z * sdx, xbar + z * sdx)

#EMLP <- read.csv("EMLP.csv",  header=T, sep=",")
#EMLP
#t.test(EMLP$Returns, alternative = "greater")
library(reshape2)
library(ggplot2)
library(stockPortfolio) #Install the package and dependencies first (one-time only)

# Color Scheme
ealred <- "#7D110C"
ealtan <- "#CDC4B6"
eallighttan <- "#F7F6F0"
ealdark <- "#423C30"

create.histogram <- function (data) {
  ticker <- data$Var2[1]
  binsize <- diff(range(data$value))/15
  g <- ggplot(data, aes(x=value)) + geom_histogram(binwidth=binsize, fill=ealred, color=ealtan) + labs(x="Daily Return (%)", y=NULL) + ggtitle(paste("Histogram of Daily Returns for\n", ticker))
  g <- add.mytheme(g)
  return(g)
}

add.mytheme <- function(g) {
  g <- g + theme_grey() + theme(text=element_text(color=ealdark), panel.background=element_rect(fill=eallighttan))
  return(g)
}

returns <- getReturns(c("EMLP", "FAS"), freq="day", get="all")
returns.melted <- melt(returns$R, na.rm=TRUE)
emlp.returns <- subset(returns.melted, Var2=="EMLP")
fas.returns <- subset(returns.melted, Var2=="FAS")

g.emlp <- create.histogram(emlp.returns)
g.fas <- create.histogram(fas.returns)
g.emlp
g.fas
ggsave("t-test - EMLP Daily Returns Histogram.png", plot=g.emlp)
ggsave("t-test - FAS Daily Returns Histogram.png", plot=g.fas)

t.test(emlp.returns$value, alternative="greater")
t.test(fas.returns$value, alternative="greater")

t.test(emlp.returns$value)
