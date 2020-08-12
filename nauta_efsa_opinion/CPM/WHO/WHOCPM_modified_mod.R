

#################################
#model
#################################
# set.seed(1234)

myCPMname <- c("CPM FAO/WHO")

modConsumerPhaseFAOWHO <- function(niter, 
                                   Pprev, 
                                   muCret, sigmaCret, piCret,
                                   CretLogMin, CretLogMax, CretLogStep, 
                                   meanPortion, stdPortion, upperPortion, 
                                   aFloose, bFloose, 
                                   aVdilute, bVdilute, 
                                   aVdrip, bVdrip){
  
  #transform lognormal parameters for the portion size
  fvarWc <- function(x,a) (exp(x) - 1)*exp(2*log(meanPortion))-stdPortion^2
  sigmaWc <- round(sqrt(uniroot(fvarWc, lower = 0, upper = 1e3, tol = 1e-7)$root),3)
  muWc <- round(log(meanPortion) - ((sigmaWc)^2)/2,3)
  
  #random numbers from the mixture distribution
  rnorm.mixture <- function(n, K, mu.mix=rep(0,K), sigma.mix=rep(1,K), pi.mix=rep(0.5,K)) {
    
    N = n #number of samples
    U = runif(N) #N random samples from U(0,1)
    
    pi.mix.sum <- rep(0, length(pi.mix)+1)
    for (i in 2:length(pi.mix.sum)) pi.mix.sum[i] <- pi.mix.sum[i-1] + pi.mix[i-1]
    
    sample = rep(NA,N) #N samples from mixture model
    for (i in 1:N){
      
      k <- min(which(U[i]<pi.mix.sum))-1
      sample[i] = rnorm(1, mu.mix[k], sigma.mix[k])
    }
    
    return(list(sample=sample))
  }
  
  Cretlog <- seq(CretLogMin,CretLogMax,by=CretLogStep)
  Cret <- 10^Cretlog
  Wc <- array(NA, niter)
  Nportion <- matrix(NA, length(Cretlog),niter)
  Ptr <- array(NA, length(Cretlog))
  dose <- matrix(NA, length(Cretlog),niter)
  
  
  Wc <- apply(cbind(rlnorm(niter, muWc, sigmaWc), upperPortion*rep(1,niter)),1,min)
  
  Floose <- runif(niter, aFloose, bFloose) #N5
  Vdrip <- runif(niter, aVdrip, bVdrip) #N6
  Vdilute <- runif(niter, aVdilute, bVdilute) #N7

  
  for (j in 1:length(Cretlog)){
    Nportion[j,1:niter] <- ifelse(Cret[j]*Wc<1e3, rpois(niter, Cret[j]*Wc), round(rnorm(niter, Cret[j]*Wc, sqrt(Cret[j]*Wc)),0))
    lambda <- rbinom(niter, size=Nportion[j,1:niter], prob=Floose*Vdrip/Vdilute)
    dose[j,1:niter] <- rpois(niter, lambda)
  }
  
  #calculating mean(dose) based on normal distribution of Cretlog
  require(distr)
  K <- length(muCret)
  CretLogDist <- rnorm.mixture(n=niter, K, mu.mix=muCret, sigma.mix=sigmaCret, pi.mix=piCret)$sample
  CretlogDens <- hist(CretLogDist[CretLogDist>CretLogMin & CretLogDist<CretLogMax],breaks=Cretlog,plot=FALSE)$density
  CretlogDens[length(Cretlog)]<-0
  newDistr <- DiscreteDistribution(supp=Cretlog,prob=CretlogDens/sum(CretlogDens))
  myNormCretlog <- r(newDistr)(1000)
  
  # weighted mean and probability of prevalence, based on normal distributed Cretlog
  dosemean <- mean(dose[match(myNormCretlog,Cretlog),1:niter])*Pprev
  PrevExp <- Pprev*(sum(dose[match(myNormCretlog,Cretlog),1:niter]>0)/niter/length(match(myNormCretlog,Cretlog)))
  
  return(list(Cretlog=Cretlog, dosemean=dosemean, dose=dose, PrevExp = PrevExp)) 
}

runConsumerPhaseFAOWHO <- modConsumerPhaseFAOWHO(niter, 
                                                 Pprev, 
                                                 muCret, sigmaCret, piCret,
                                                 CretLogMin, CretLogMax, CretLogStep, 
                                                 meanPortion, stdPortion, upperPortion, 
                                                 aFloose, bFloose, 
                                                 aVdilute, bVdilute, 
                                                 aVdrip, bVdrip)
Cretlog <- runConsumerPhaseFAOWHO$Cretlog
dose <- runConsumerPhaseFAOWHO$dose
dosemean <- runConsumerPhaseFAOWHO$dosemean
PrevExp <- runConsumerPhaseFAOWHO$PrevExp
#############################


