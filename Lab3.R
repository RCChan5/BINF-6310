#probability of having the disease
#(+|-)
prior <- c(.001,.999)

#(+|-)
infected <- c(.91,.09)
notInfected <- c(.16,.84)


###how many times it needs to be ran?
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

data <- getData(infected,50)
probInfectedVals <-  vector()
x <- 0
y <- 0
titleStr <- ""
for(i in 1:length(data))
{
  
  probInfectedVals[i] <- prior[1]
  marginal <- prior[1] * infected[data[i]] + prior[2]*notInfected[data[i]]
  
  prior[1] <- prior[1] * infected[data[i]] / marginal
  prior[2] <- prior[2] * notInfected[data[i]] / marginal
  
  titleStr <-  paste(titleStr, data[i], sep="")
  
  
  
  plot(1:i, probInfectedVals, main=titleStr, ylim=c(0,1), xlim=c(1,length(data)+1))
 
}
######################################################part 2
probTruePositive <- .96
probFalsePositive <- .95


# p(positive) , p(negative)
likelihoodGivenCylon <- c( 0.96, 0.04)
likelihoodGivenNotCylon<- c(0.05,.95)

getDataFromLikelihood <- function( likelihood, numPoints) 
{
  d <- vector(mode="integer", length=numPoints);
  
  for( i in 1:numPoints ) 
  {
    if( runif(1) <= likelihood[2] )
    {
      d[i] <- 1;
    }
    else
    {
      d[i] <- 2;
    }
    
  }
  
  return(d)
}

numTests <- 1:20
numSimulationsPerCycle <- 10000

averagePosteriorValue <- c(length=length(numTests))
estimatedPower<- c(length=length(numTests))

for( i in numTests ) 
{
  posteriorValues <- c(length=numSimulationsPerCycle)
  
  for( j in 1:numSimulationsPerCycle  ) 
  {
    # reset our prior each time 
    probCylon <-  c(.001,.999)
    
    data <- getDataFromLikelihood( likelihoodGivenNotCylon, numTests[i])
    
    for( k in 1:length(data))
    {
      denom <- probCylon[1] * likelihoodGivenCylon[data[i]] + probCylon[2] * likelihoodGivenNotCylon[data[k]];
      
      probCylon[1] = probCylon[1] * likelihoodGivenCylon[data[k]] / denom;
      probCylon[2] = probCylon[2] * likelihoodGivenNotCylon[data[k]] / denom;
    }
    
    posteriorValues[j] = probCylon[1]	
  }
  
  averagePosteriorValue[i] = mean(posteriorValues)
  estimatedPower[i] <- sum(posteriorValues >= 1-0.0001  ) / numSimulationsPerCycle 
}


plot(numTests, log10(1-averagePosteriorValue))
lines( numTests, rep(log10(.05),length(numTests)),col="red")


plot( numTests, estimatedPower)

