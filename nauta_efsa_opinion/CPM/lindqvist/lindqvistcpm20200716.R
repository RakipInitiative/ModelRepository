# packages
require(distr)

# input parameters
transferRateLower <- -3.7
transferRateUpper <- -1
chickenPortionSizeMean <- 189
chickenPortionSizeStd <- 126.9



# model script
#set.seed(1234)

X <- runif(1,transferRateLower,transferRateUpper)

NpDistribution <- DiscreteDistribution(supp=c(1,2,3,4,5),prob=c(0.28,0.48,0.11,0.09,0.04))
Np <- r(NpDistribution)(1)

chickenPortionLogNorm <- dlnorm(1,log(chickenPortionSizeMean),log(chickenPortionSizeMean))
#chickenPortionLogNorm <- 335.0318

pTR <- Np*chickenPortionLogNorm/1097*10^X

dose <- 1
dosemean <-1

# visualization script
xx <- rlnorm(10000,log10(chickenPortionSizeMean),log10(chickenPortionSizeStd))
plot(density((xx)),xlim=c(1, 1000))
axis(1, 1:1000, 1:1000)
