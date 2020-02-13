# question 1
normalroll <- c(1,2,3,4,5,6)
loadedroll <-c(1,2,3,4,5,6,6,6,6,6)
m1 <- mean(loadedroll)
m2 <- var(loadedroll)
m3 <- var(normalroll)

#question 2
rollLoadedDie <- function(times) 
{
  loadedProbability <- c(.1,.1,.1,.1,.1,.5)
  x <- sample(1:6, size = times, replace=TRUE, prob = loadedProbability)
  barplot(table(x)) 
  return(x)
}


#question3
myRolls <-rollLoadedDie(10000)
hist(myRolls)


#question4
#Modify the code on Slide #58 of lecture #2 so that the means vs. trial size 
#plots are from the loaded die.  Generate these plots a few times.  How many rolls appear to be necessary to get convergence on 
#the expected values for the mean and variance?

trialSizes <- c(5,10,15,20,25,30,40,50,100,200,300,400,500,1000,2000,3000,4000,5000,10000,20000,30000,100000)
means <- vector(mode = "double", length = length(trialSizes))
variances <- vector(mode = "double", length = length(trialSizes))

for (i in 1:length(trialSizes))
{
  rolls <- vector(length = trialSizes[i], mode = "double")
  
  rolls <- rollLoadedDie(trialSizes[i])
  
  means[i] <- mean(rolls)
  variances[i] <- var(rolls)

}

plot(log10(trialSizes),means)
lines(log10(trialSizes), rep(4.5, length(trialSizes)))

plot(log10(trialSizes), variances)
lines(log10(trialSizes), rep(3.25,length(trialSizes)))

  
