
PopulationMean =100
PopulationSD =2
SampleSize = 15

NumberOfSamples = 10000
t_Score <- array(0,dim=NumberOfSamples)
for (i in 1:NumberOfSamples) {
	RandomSample <- rnorm(n=SampleSize,mean=PopulationMean,sd=PopulationSD)
	SampleMean <- mean(RandomSample)
	SampleSD <- sd(RandomSample)
	StdErrOfMean <- SampleSD/sqrt(SampleSize)
	t_Score[i] <- (SampleMean - PopulationMean)/StdErrOfMean
}

( var(t_Score) )  #the variance of t-scores for all 10000 samples


DegreesOfFreedom <- SampleSize - 1

Variance_of_t_distribution <- DegreesOfFreedom/(DegreesOfFreedom-2)
print(Variance_of_t_distribution)

