#question 1
people <- c(0:30)
plot(people ,dbinom(people,30,p=1/3))

dbinom(12,30,1/3)

#question 2
people <- c(0:100)
plot(people ,dbinom(people,100,p=.4))

binom.test(53,100,.4,alternative = "two.sided")


binom.test(53,100,.4,alternative = "greater")

sum(dbinom(53:100,100,.4))

#question 3z

myVals <- rbinom(1000,10000,.5)

mean(myVals)

var(myVals)
test <- c()
for(i in 1:length(myVals))
{
 test[i] <-  binom.test(myVals[i],10000,p=.50)$p.value
 print(test[i])
}

hist(test)

