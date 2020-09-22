# CPM
#################################
#parameters
#################################
niter <- 10000
Pprev <- 0.25
CretLogMin <- -2
CretLogMax <- 7
CretLogStep <- 0.1
muCret <- 1.5
sigmaCret <- 1.2
piCret <- 1.0
meanPortion <- 189.0
stdPortion <- 126.9
upperPortion <- 1000.0

transferRateLower <- -3.7
transferRateUpper <- -1
#DRMstuff
alphaGamma <- 0.44
betaGamma <- 0.51
rPillinf <- 0.06
etaPillinf <- 0.88

#############################


#################################
#model
#################################
set.seed(1234)
myCPMname <- c("CPM Lindqvist")
modConsumerPhaseLindqvist <- function(niter, 
                                      Pprev, 
                                      muCret, sigmaCret, piCret,
                                      CretLogMin, CretLogMax, CretLogStep, 
                                      meanPortion, stdPortion, upperPortion, 
                                      transferRateLower,transferRateUpper){
  
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
  
  X <- runif(niter,transferRateLower,transferRateUpper)
  NpDistribution <- DiscreteDistribution(supp=c(1,2,3,4,5),prob=c(0.28,0.48,0.11,0.09,0.04))
  Np <- r(NpDistribution)(niter)
  
  Wc <- apply(cbind(rlnorm(niter, muWc, sigmaWc), upperPortion*rep(1,niter)),1,min)
  
  Ptr <- apply(cbind(rep(1,niter),Np*Wc/1097*10^X),1,min)

  for (j in 1:length(Cretlog)){
    Nportion[j,1:niter] <- ifelse(Cret[j]*Wc<1e3, rpois(niter, Cret[j]*1097), round(rnorm(niter, Cret[j]*1097, sqrt(Cret[j]*1097)),0))
    dose[j,1:niter] <- ifelse(Nportion[j,1:niter]==0, 
                              0, 
                              ifelse(Nportion[j,1:niter]<25e3, 
                                     rbinom(niter, size=Nportion[j,1:niter], prob=Ptr), 
                                     round(rnorm(niter,Nportion[j,1:niter]*Ptr,sqrt(Nportion[j,1:niter]*Ptr*(1-Ptr))))
                              )
    )
  }
  
  # exluding rare cases if normal distribution creates negativ doses
  dose<-ifelse(dose<0,0,dose)
  
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

runConsumerPhaseLindqvist <- modConsumerPhaseLindqvist(niter, 
                                                       Pprev, 
                                                       muCret, sigmaCret, piCret,
                                                       CretLogMin, CretLogMax, CretLogStep, 
                                                       meanPortion, stdPortion, upperPortion, 
                                                       transferRateLower,transferRateUpper)

Cretlog <- runConsumerPhaseLindqvist$Cretlog
dose <- runConsumerPhaseLindqvist$dose
dosemean <- runConsumerPhaseLindqvist$dosemean
PrevExp <- runConsumerPhaseLindqvist$PrevExp
#############################









#################################
#DRM
#################################
condPillinf <- 1 - (1+dose/etaPillinf)^(-rPillinf)
modDoseResponseMedianChallenge <- function(dose, Pprev, condPillinf, alphaGamma, betaGamma){
  
  nrOfCrets <- dim(dose)[1]
  niter <- dim(dose)[2]
  
  
  Pill <- matrix(NA, nrOfCrets, niter)
  
  for (i in 1:nrOfCrets){
    Pill[i,] <- condPillinf[i,]*round((1 - exp(lgamma(alphaGamma+betaGamma)+lgamma(betaGamma+dose[i,])-lgamma(betaGamma)-lgamma(alphaGamma+betaGamma+dose[i,]))),4)
  }
  
  Pillmean <- rowMeans(Pill)
  Qill <- Pillmean*Pprev
  PrevExp <- Pprev*(sum(dose>0)/niter)
  
  return(list(Pill=Pill, Pillmean=Pillmean, Qill=Qill, PrevExp=PrevExp))
  #return(list(Qill=Qill, PrevExp=PrevExp))
}

runDoseResponseMedianChallenge <- modDoseResponseMedianChallenge(dose, Pprev, condPillinf, alphaGamma, betaGamma)
   
  
  Pillmean <-runDoseResponseMedianChallenge$Pillmean
  Pill <-runDoseResponseMedianChallenge$Pill
  PrevExp <- runDoseResponseMedianChallenge$PrevExp
  #################################
  
  
  #################################
  #visualisation
  #################################
plot(Cretlog, Pillmean, xlab = "log(C_ret)", ylab = "P_ill", main = "probability of getting ill based on\ncontamination of chicken meat bought at retail")
text(5,0.05,family="A",font=2, "CPM: Lindqvist", pos=3)
text(4.5,0.02,family="D",font=2, "DRM: Median Challenge", pos=3)
  #################################