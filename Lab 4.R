
#######################################################Questions 1##################################################

#likelihood 
#infected <- c(.91,.09)
#notInfected <- c(.16,.84)
#Function to create a data list given a 
getData <-  function(likelihood, numPoints)
{
  d <- vector(mode="integer",length=numPoints)
  for(i in 1:numPoints)
  {
    if(runif(1) <= likelihood[1])
    {
      d[i] <- 1
    }
    else
    {
      d[i] <- 2
    }
    
  }
  return(d)
}  

#getData(infected,50)
#data <- getData(infected,50)

############################*Variables to change*##################################################
#prior
#(Fair|Loaded) 
prior <- c(.99,.01)
#likelihood  
A_likelihood <- c(1/6,1/6,1/6,1/6,1/6,1/6)
B_likelihood <- c(1/10,1/10,1/10,1/10,1/10,5/10)
#data rolls given 
data<-c(2,3,2,6,3,5,6,2,6,6,2,6,6,2,3,6,6,6,5,6,6,5,6,6,6,6,6,4,6,3,3,3,6,6,5,6,6)
##################################################################################################
A <-  vector()
B <-  vector()
x <- 0
y <- 0
titleStr <- ""

for(i in 1:length(data))
{
  
  A[i] <- prior[1]
  B[i] <- prior[2]
  marginal <- prior[1] * A_likelihood[data[i]] + prior[2]*B_likelihood[data[i]]
  #print(marginal)
  prior[1] <- prior[1] * A_likelihood[data[i]] / marginal
  prior[2] <- prior[2] * B_likelihood[data[i]] / marginal
  
  
  #titleStr <-  paste(titleStr, data[i], sep="")
  #plot(1:i, A, main=titleStr, ylim=c(0,1), xlim=c(1,length(data)+1))
  #plot(1:i, B, main=titleStr, ylim=c(0,1), xlim=c(1,length(data)+1))
}

titleStr="Fair Die"
plot(1:i, A, main=titleStr, ylim=c(0,1), xlim=c(1,length(data)+1))

titleStr="Loaded Die"
plot(1:i, B, main=titleStr, ylim=c(0,1), xlim=c(1,length(data)+1))

################################################Question 2##########################################################

#Prior
#priorA <- .99
#priorB <- .01


# p(positive) , p(negative)
likelihoodA <- c(1/6,1/6,1/6,1/6,1/6,1/6)
likelihoodB<- c( 1/10, 1/10, 1/10, 1/10, 1/10, 5/10)

#generate data  
getDataFromLikelihood <- function( likelihood, numPoints) 
{
  d <- vector(mode="integer", length=numPoints);
  myprobs <- c( 1/10, 1/10, 1/10, 1/10, 1/10, 5/10)
    
  #set.seed(1)
  d <- sample(1:6, size = numPoints, replace = TRUE, prob = likelihood)
  #plotting distribution
  #barplot(table(d))
  
  
  return(d)
}

print(data)
numTests <- 1:100
numSimulationsPerCycle <- 10000

averagePosteriorValue <- c(length=length(numTests))
estimatedPower<- c(length=length(numTests))

for( i in numTests ) 
{
  posteriorValues <- c(length=numSimulationsPerCycle)
  
  for( j in 1:numSimulationsPerCycle  ) 
  {
    # reset our prior each time  
    probA <-  c(.99,.01)
    
    data <- getDataFromLikelihood( likelihoodB, numTests[i])
    
    for( k in 1:length(data))
    {
      denom <- probA[1] * likelihoodA[data[i]] + probA[2] * likelihoodB[data[k]];
     
      probA[1] = probA[1] * likelihoodA[data[k]] / denom;
      probA[2] = probA[2] * likelihoodB[data[k]] / denom;
     
    }
    #*change this*
    posteriorValues[j] = probA[2]	
  }
  
  averagePosteriorValue[i] = mean(posteriorValues)
  #is this correct?
  estimatedPower[i] <- sum(posteriorValues >= 1-0.001  ) / numSimulationsPerCycle 
}


plot(numTests, log10(1-averagePosteriorValue))
lines( numTests, rep(log10(.05),length(numTests)),col="red")


plot( numTests, estimatedPower)





















