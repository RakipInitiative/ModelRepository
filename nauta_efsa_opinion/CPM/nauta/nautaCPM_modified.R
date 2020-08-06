#parameters

niter <- 10000
Pprev <- 0.25
#muCret <- 1.5
#sigmaCret <- 1.2
#piCret <- 1.0
CretLogMin <- 5
CretLogMax <- 5
CretLogStep <- 0.1
meanPortion <- 189.0
stdPortion <- 126.9
upperPortion <- 1000.0
logPtr <- c(2.24, 2.36, 2.37, 2.58, 2.82, 2.86, 3.16, 3.17, 3.47, 3.52, 3.57, 3.83, 3.83, 3.84, 3.87, 3.89, 3.89, 3.90, 3.94, 4.03, 4.09, 4.42, 4.53, 4.54, 4.54, 4.62, 4.62, 4.68, 4.73, 4.76,4.84, 4.92, 4.93, 4.95, 4.97, 5.20, 5.25, 5.27, 5.39, 5.47, 5.60, 5.83, 5.89, 5.95, 5.96, 6.02, 6.23, 6.38, 6.96, 7.37, 7.90, 8.20, 9.00, 9.00, 9.00)
#############################



#model
# set.seed(1234)
#modConsumerPhaseNauta <- function(niter, Pprev, CretLogMin, CretLogMax, CretLogStep, meanPortion, stdPortion, upperPortion, logPtr){
  
  #transform lognormal parameters for the portion size
  fvarWc <- function(x,a) (exp(x) - 1)*exp(2*log(meanPortion))-stdPortion^2
  sigmaWc <- round(sqrt(uniroot(fvarWc, lower = 0, upper = 1e3, tol = 1e-7)$root),3)
  muWc <- round(log(meanPortion) - ((sigmaWc)^2)/2,3)
  
  Cretlog <- seq(CretLogMin,CretLogMax,by=CretLogStep)
  Cret <- 10^Cretlog
  Wc <- array(NA, niter)
  Nportion <- matrix(NA, length(Cretlog),niter)
  Ptr <- array(NA, niter)
  dose <- matrix(NA, length(Cretlog),niter)
  
  
  
  
  for (i in 1:niter){
    Ptr[i] <- 10^(-sample(logPtr, 1))
    Wc[i] <- min(rlnorm(1, muWc, sigmaWc), upperPortion)
    Nportion[1:length(Cretlog),i] <- ifelse(Cret*Wc[i]<1e3, rpois(1, Cret*Wc[i]), round(rnorm(1, Cret*Wc[i], sqrt(Cret*Wc[i])),0))
    dose[1:length(Cretlog),i] <- ifelse(Nportion[1:length(Cretlog),i]==0, 0, rbinom(1, size=Nportion[1:length(Cretlog),i], prob=Ptr[i]))
  }
  
  dosemean <- rowMeans(dose)*Pprev
  PrevExp <- Pprev*(sum(dose>0)/niter)
#  return(list(Cretlog=Cretlog, dosemean=dosemean, PrevExp = PrevExp)) 
#}

#runConsumerPhaseNauta <- modConsumerPhaseNauta(niter, Pprev, CretLogMin, CretLogMax, CretLogStep, meanPortion, stdPortion, upperPortion, logPtr)

#Cretlog <- runConsumerPhaseNauta$Cretlog
#dosemean <- runConsumerPhaseNauta$dosemean
#PrevExp <- runConsumerPhaseNauta$PrevExp
#############################
#visualisation


  condPillinf <- 0.33
  betaGamma <- 7.59
  alphaGamma <- 0.145
  
  #model
  modDoseResponseClassical <- function(dose, Pprev, condPillinf, alphaGamma, betaGamma){
    
    niter <- length(dose)
    
    Pill <- array(NA, niter)
    
    for (i in 1:niter) Pill[i] <- condPillinf*round((1 - exp(lgamma(alphaGamma+betaGamma)+lgamma(betaGamma+dose[i])-lgamma(betaGamma)-lgamma(alphaGamma+betaGamma+dose[i]))),4)
    
    Pillmean <- mean(Pill)
    Qill <- Pillmean*Pprev
    PrevExp <- Pprev*(sum(dose>0)/niter)
    
    return(list(Pill=Pill, Pillmean=Pillmean, Qill=Qill, PrevExp=PrevExp))
    #return(list(Qill=Qill, PrevExp=PrevExp))
  }
  
  runDoseResponseClassical <- modDoseResponseClassical(dose, Pprev, condPillinf, alphaGamma, betaGamma)
  
  
  Qill <-runDoseResponseClassical$Qill
  PrevExp <- runDoseResponseClassical$PrevExp
  
  
  
  
#plot(Cretlog, Qill, xlab = "Cretlog", ylab = "dosemean", main = "doses from meat contaminated at retail")
