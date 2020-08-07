# CPM
#################################
#parameters
#################################
niter <- 10000
Pprev <- 0.25
CretLogMin <- -2
CretLogMax <- 7
CretLogStep <- 0.1
meanPortion <- 189.0
stdPortion <- 126.9
upperPortion <- 1000.0
logPtr <- c(2.24, 2.36, 2.37, 2.58, 2.82, 2.86, 3.16, 3.17, 3.47, 3.52, 3.57, 3.83, 3.83, 3.84, 3.87, 3.89, 3.89, 3.90, 3.94, 4.03, 4.09, 4.42, 4.53, 4.54, 4.54, 4.62, 4.62, 4.68, 4.73, 4.76,4.84, 4.92, 4.93, 4.95, 4.97, 5.20, 5.25, 5.27, 5.39, 5.47, 5.60, 5.83, 5.89, 5.95, 5.96, 6.02, 6.23, 6.38, 6.96, 7.37, 7.90, 8.20, 9.00, 9.00, 9.00)
#############################


#################################
#model
#################################
# set.seed(1234)
modConsumerPhaseNauta <- function(niter, Pprev, CretLogMin, CretLogMax, CretLogStep, meanPortion, stdPortion, upperPortion, logPtr){
  
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
  
  
  Wc <- apply(cbind(rlnorm(niter, muWc, sigmaWc), upperPortion*rep(1,niter)),1,min)
  
    Ptr <- 10^(-sample(logPtr, niter,replace=TRUE))

  for (j in 1:length(Cretlog)){
    Nportion[j,1:niter] <- ifelse(Cret[j]*Wc<1e3, rpois(niter, Cret[j]*Wc), round(rnorm(niter, Cret[j]*Wc, sqrt(Cret[j]*Wc)),0))
    dose[j,1:niter] <- ifelse(Nportion[j,1:niter]==0, 0, rbinom(niter, size=Nportion[j,1:niter], prob=Ptr))
  }
  
  dosemean <- rowMeans(dose)*Pprev
  PrevExp <- Pprev*(sum(dose>0)/niter)
  return(list(Cretlog=Cretlog, dosemean=dosemean, dose=dose, PrevExp = PrevExp)) 
}

runConsumerPhaseNauta <- modConsumerPhaseNauta(niter, Pprev, CretLogMin, CretLogMax, CretLogStep, meanPortion, stdPortion, upperPortion, logPtr)

Cretlog <- runConsumerPhaseNauta$Cretlog
dose <- runConsumerPhaseNauta$dose
dosemean <- runConsumerPhaseNauta$dosemean
PrevExp <- runConsumerPhaseNauta$PrevExp
#############################




#################################
#visualisation
#################################
xmin = min(Cretlog)
xmax = max(Cretlog)*1.1
ymin = min(log10(dosemean))
ymax = max(log10(dosemean))*1.1
plot(Cretlog, log10(dosemean), xlab = "log(C_ret) in cfu/g", ylab = "log(dose) in cfu/g after consumer handling of chicken meat in ready-to-eat chicken salad", main = "result of van Asselt Consumer phase model",xlim=c(xmin,xmax),ylim=c(ymin,ymax))




#################################
#################################
#################################




#################################
#DRM
#################################
#parameters
#################################
# dose <- matrix(1,3,4)
 Pprev <- 0.25
  condPillinf <- 0.33
  betaGamma <- 7.59
  alphaGamma <- 0.145
  
  #################################  
  #model
  #################################
  modDoseResponseClassical <- function(dose, Pprev, condPillinf, alphaGamma, betaGamma){
    
    nrOfCrets <- dim(dose)[1]
    nrOfDoses <- dim(dose)[2]
    
    Pill <- matrix(NA, nrOfCrets, nrOfDoses)
    
    for (i in 1:nrOfDoses) Pill[1:nrOfCrets,i] <- condPillinf*round((1 - exp(lgamma(alphaGamma+betaGamma)+lgamma(betaGamma+dose[1:nrOfCrets,i])-lgamma(betaGamma)-lgamma(alphaGamma+betaGamma+dose[1:nrOfCrets,i]))),4)
    
    Pillmean <- rowMeans(Pill)
    Qill <- Pillmean*Pprev
    PrevExp <- Pprev*(sum(dose>0)/niter)
    
    return(list(Pill=Pill, Pillmean=Pillmean, Qill=Qill, PrevExp=PrevExp))
    #return(list(Qill=Qill, PrevExp=PrevExp))
  }
  
  runDoseResponseClassical <- modDoseResponseClassical(dose, Pprev, condPillinf, alphaGamma, betaGamma)
  
  
  Pillmean <-runDoseResponseClassical$Pillmean
  Pill <-runDoseResponseClassical$Pill
  PrevExp <- runDoseResponseClassical$PrevExp
  #################################
  
  
  #################################
  #visualisation
  #################################
plot(Cretlog, Pillmean, xlab = "log(C_ret)", ylab = "P_ill", main = "probability of getting ill based on contamination of chicken meat bought at retail")
  #################################