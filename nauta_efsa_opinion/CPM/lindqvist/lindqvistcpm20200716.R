#set.seed(1234)

# packages
require(distr)

# input parameters
Iterations <- 10000 # 10000
transferRateLower <- -3.7
transferRateUpper <- -1
chickenPortionSizeMean <- 189
chickenPortionSizeStd <- 126.9

# these are the 91 Sims from @Risk
logNPerGramLowerBound <- -2
logNPerGramStepsize <- 0.1
logNPerGramUpperBound <- 7


# model script
# R needs mu & sigma instead of mean &std-dev as input 
# for lognorm distribution
chickenPortionSizeSigmaSquare = log(chickenPortionSizeStd^2/chickenPortionSizeMean^2 + 1)
chickenPortionSizeMu = log(chickenPortionSizeMean) - chickenPortionSizeSigmaSquare/2

# transfer rate BA4
X <- runif(Iterations,transferRateLower,transferRateUpper)

# AZ4
NpDistribution <- DiscreteDistribution(supp=c(1,2,3,4,5),prob=c(0.28,0.48,0.11,0.09,0.04))
Np <- r(NpDistribution)(Iterations)

rndNrChickenPortionLogNorm <- rlnorm(Iterations,chickenPortionSizeMu,chickenPortionSizeSigmaSquare)

#AY6
ones <- c(rep(1,Iterations))
dummyRes <- Np*rndNrChickenPortionLogNorm/1097*10^X
df <- data.frame(ones,dummyRes)
pTR <- apply(df,1,FUN=min)

#AZ4 - Nr of @Risk Simulations
logNPerGram <- seq(logNPerGramLowerBound,logNPerGramUpperBound,by=logNPerGramStepsize)


#AX4 - size 91
NcarcassVec <- rpois(1,1097*10^logNPerGram)

count <- 0
dose <- c(rep(1,length(logNPerGram)))
# go through all simulations
for (Ncarcass in NcarcassVec) 
{ 
  count = count +1
  # AY9
  if (Ncarcass == 0) {
    dose[count] <- 0
  } else {
    if (Ncarcass<25000) {
      dose[count] <- mean(rbinom(1,Ncarcass,pTR))
    } else {
      dummy <-round(rnorm(1,Ncarcass*pTR,Ncarcass*pTR*(1-pTr)))
      dose[count] <- mean(max(0,dummy))
    }
  }
}
  

# DR model
## DR params for classical model
alpha <- 0.145 #C5
beta <- 7.59 #C6
eta <- 0 #C8
r <- 0 #C9
const <- 0.33 #C10

# classical DRM script
PillInf <- const #AY11
Pill <- PillInf*(1-exp(log(gamma(alpha+beta))+log(gamma(dose+beta))-log(gamma(dose+alpha+beta))-log(gamma(beta))))






# visualization script
plot(logNPerGram,dose)
#axis(1, 1:1000, 1:1000)

