
#################################
#model
#################################
# set.seed(1234)
modConsumerPhaseLindqvist <- function(niter, Pprev, CretLogMin, CretLogMax, CretLogStep, meanPortion, stdPortion, upperPortion, transferRateLower,transferRateUpper){
  
  #transform lognormal parameters for the portion size
  fvarWc <- function(x,a) (exp(x) - 1)*exp(2*log(meanPortion))-stdPortion^2
  sigmaWc <- round(sqrt(uniroot(fvarWc, lower = 0, upper = 1e3, tol = 1e-7)$root),3)
  muWc <- round(log(meanPortion) - ((sigmaWc)^2)/2,3)
  
  Cretlog <- seq(CretLogMin,CretLogMax,by=CretLogStep)
  Cret <- 10^Cretlog
  Wc <- array(NA, niter)
  Nportion <- matrix(NA, length(Cretlog),niter)
  Ptr <- array(NA, length(Cretlog))
  dose <- matrix(NA, length(Cretlog),niter)
  
  X <- runif(niter,transferRateLower,transferRateUpper)
  NpDistribution <- DiscreteDistribution(supp=c(1,2,3,4,5),prob=c(0.28,0.48,0.11,0.09,0.04))
  Np <- r(NpDistribution)(niter)
  
  Wc <- apply(cbind(rlnorm(niter, muWc, sigmaWc), upperPortion*rep(1,niter)),1,min)
  
  Ptr <- apply(cbind(rep(1,niter),Np*Wc/1097*10^X),1,min)

  for (j in 1:length(Cretlog)){
    Nportion[j,1:niter] <- ifelse(Cret[j]*Wc<1e3, rpois(niter, Cret[j]*1097), round(rnorm(niter, Cret[j]*1097, sqrt(Cret[j]*1097)),0))
    dose[j,1:niter] <- ifelse(Nportion[j,1:niter]==0, 0, rbinom(niter, size=Nportion[j,1:niter], prob=Ptr))
  }
  
  dosemean <- rowMeans(dose)*Pprev
  PrevExp <- Pprev*(sum(dose>0)/niter)
  return(list(Cretlog=Cretlog, dosemean=dosemean, dose=dose, PrevExp = PrevExp)) 
}

runConsumerPhaseLindqvist <- modConsumerPhaseLindqvist(niter, Pprev, CretLogMin, CretLogMax, CretLogStep, meanPortion, stdPortion, upperPortion, transferRateLower,transferRateUpper)

Cretlog <- runConsumerPhaseLindqvist$Cretlog
dose <- runConsumerPhaseLindqvist$dose
dosemean <- runConsumerPhaseLindqvist$dosemean
PrevExp <- runConsumerPhaseLindqvist$PrevExp
#############################



