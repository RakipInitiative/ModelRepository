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

transferRateWashedHandsMean <- -7.04
transferRateWashedHandsStd <- 0.66
transferRateUnwashedHandsMean <- -2.88
transferRateUnwashedHandsStd <- 0.68
transferRateUnwashedBoardMean <- -3.15
transferRateUnwashedBoardStd <- 0.49
transferRateWashedBoardMean <- 0
transferRateWashedBoardStd <- 0
probUnwashedHands <- 0.2
probWashedHands <- 0.8
probUnwashedBoard <- 0.05
probWashedHandsBoard <- 0.95

#############################


#################################
#model
#################################
# set.seed(1234)  CretLogMin, CretLogMax, CretLogStep, 

require(distr)
modConsumerPhaseVanAsselt <- function(niter, Pprev, CretLogMin, CretLogMax, CretLogStep, meanPortion, stdPortion, upperPortion, transferRateWashedHandsMean, transferRateWashedHandsStd, transferRateUnwashedHandsMean, transferRateUnwashedHandsStd, transferRateWashedBoardMean, transferRateWashedBoardStd, transferRateUnwashedBoardMean, transferRateUnwashedBoardStd, probUnwashedHands, probWashedHands, probUnwashedBoard, probWashedHandsBoard){


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
  transferRateHands <- array(NA, niter)
  transferRateBoard <- array(NA, niter)
  
  transferRateUnwashedHands <- 10^(rnorm(niter,transferRateUnwashedHandsMean,transferRateUnwashedHandsStd))
  transferRateWashedHands <- 10^(rnorm(niter,transferRateWashedHandsMean,transferRateWashedHandsStd))
  for (i in 1:niter) {
    transferRateHandsDistr <- DiscreteDistribution(supp=c(transferRateUnwashedHands[i],transferRateWashedHands[i]),prob=c(probUnwashedHands,probWashedHands))
    transferRateHands[i] <- r(transferRateHandsDistr)(1)
  }
  transferRateUnwashedBoard <- (1-transferRateHands)*10^(rnorm(niter,transferRateUnwashedBoardMean,transferRateUnwashedBoardStd))
  
  
  #from @Risk code
  transferRateWashedBoard <- rnorm(niter,transferRateWashedBoardMean,transferRateWashedBoardStd)
  # Marcel thinks this should be more like this:
  #transferRateWashedBoard <- (1-transferRateHands)*10^(rnorm(niter,transferRateWashedBoardMean,transferRateWashedBoardStd))
  
  for (i in 1:niter) {
    transferRateBoardDistr <- DiscreteDistribution(supp=c(transferRateUnwashedBoard[i],transferRateWashedBoard[i]),prob=c(probUnwashedBoard,probWashedHandsBoard))
    transferRateBoard[i] <- r(transferRateBoardDistr)(1)
  }
  Ptr = transferRateHands + transferRateBoard
  
  Wc <- apply(cbind(rlnorm(niter, muWc, sigmaWc), upperPortion*rep(1,niter)),1,min)
  
  
  for (j in 1:length(Cretlog)){
    Nportion[j,1:niter] <- ifelse(Cret[j]*Wc<1e3, rpois(niter, Cret[j]*Wc), round(rnorm(niter, Cret[j]*Wc, sqrt(Cret[j]*Wc)),0))
    dose[j,1:niter] <- ifelse(Nportion[j,1:niter]==0, 0, rbinom(niter, size=Nportion[j,1:niter], prob=Ptr))
  }
  
  dosemean <- rowMeans(dose)*Pprev
  PrevExp <- Pprev*(sum(dose>0)/niter)
  return(list(Cretlog=Cretlog, dosemean=dosemean, dose=dose, PrevExp = PrevExp)) 
}

runConsumerPhaseVanAsselt <- modConsumerPhaseVanAsselt(niter, Pprev, CretLogMin, CretLogMax, CretLogStep, meanPortion, stdPortion, upperPortion, transferRateWashedHandsMean, transferRateWashedHandsStd, transferRateUnwashedHandsMean, transferRateUnwashedHandsStd, transferRateWashedBoardMean, transferRateWashedBoardStd, transferRateUnwashedBoardMean, transferRateUnwashedBoardStd, probUnwashedHands, probWashedHands, probUnwashedBoard, probWashedHandsBoard)

Cretlog <- runConsumerPhaseVanAsselt$Cretlog
dose <- runConsumerPhaseVanAsselt$dose
dosemean <- runConsumerPhaseVanAsselt$dosemean
PrevExp <- runConsumerPhaseVanAsselt$PrevExp
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
#plot(Cretlog, Pillmean, xlab = "log(C_ret)", ylab = "P_ill", main = "probability of getting ill based on contamination of chicken meat bought at retail")
  #################################