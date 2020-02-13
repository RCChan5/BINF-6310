#Binmoial 
#n = 25 p = .05 times won 16
#
binom.test(16,25,.5,alternative="greater")
binom.test(0,8,.15)
1-pbinom(15,25,.5)
sum(dbinom(16:25,25,.5))

seqs <- c(0:25)
plot(seqs,dbinom(seqs,25,p=0.5))