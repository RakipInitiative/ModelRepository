# set.seed(1234)
meanPortion <- 189.0
sigmaCret <- 0.2
muCret <- 1.5
Pprev <- 0.25
niter <- 100000
upperPortion <- 1000.0
stdPortion <- 126.9
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


# model
require(distr)

modConsumerPhaseVanAsselt <- function(niter, Pprev, muCret, sigmaCret, meanPortion, stdPortion, upperPortion, transferRateWashedHandsMean, transferRateWashedHandsStd, transferRateUnwashedHandsMean, transferRateUnwashedHandsStd, transferRateWashedBoardMean, transferRateWashedBoardStd, transferRateUnwashedBoardMean, transferRateUnwashedBoardStd, probUnwashedHands, probWashedHands, probUnwashedBoard, probWashedHandsBoard){
  
  #transform lognormal parameters for the portion size
  fvarWc <- function(x,a) (exp(x) - 1)*exp(2*log(meanPortion))-stdPortion^2
  sigmaWc <- round(sqrt(uniroot(fvarWc, lower = 0, upper = 1e3, tol = 1e-7)$root),3)
  muWc <- round(log(meanPortion) - ((sigmaWc)^2)/2,3)
  
  
  Cretlog <- array(NA, niter)
  Cret <- array(NA, niter)
  Wc <- array(NA, niter)
  Nportion <- array(NA, niter)
  Ptr <- array(NA, niter)
  dose <- array(NA, niter)
  transferRateHands <- array(NA, niter)
  transferRateBoard <- array(NA, niter)

  # transfer rates for washed/unwashed hands/board
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
  
    
  K <- length(muCret)
  for (i in 1:niter){
    Cretlog[i] <- rnorm(1, muCret, sigmaCret)
    Cret[i] <- 10^Cretlog[i]
    
    Wc[i] <- min(rlnorm(1, muWc, sigmaWc), upperPortion)
    
    Nportion[i] <- ifelse(Cret[i]*Wc[i]<1e3, rpois(1, Cret[i]*Wc[i]), round(rnorm(1, Cret[i]*Wc[i], sqrt(Cret[i]*Wc[i])),0))
    
    
    dose[i] <- ifelse(Nportion[i]==0, 0, rbinom(1, size=Nportion[i], prob=Ptr[i]))
  }
  
  dosemean <- mean(dose)*Pprev
  PrevExp <- Pprev*(sum(dose>0)/niter)
  return(list(dose=dose, dosemean=dosemean, PrevExp = PrevExp)) 
}

runConsumerPhaseVanAsselt <- modConsumerPhaseVanAsselt(niter, Pprev, muCret, sigmaCret, meanPortion, stdPortion, upperPortion, transferRateWashedHandsMean, transferRateWashedHandsStd, transferRateUnwashedHandsMean, transferRateUnwashedHandsStd, transferRateWashedBoardMean, transferRateWashedBoardStd, transferRateUnwashedBoardMean, transferRateUnwashedBoardStd, probUnwashedHands, probWashedHands, probUnwashedBoard, probWashedHandsBoard)

dose <- runConsumerPhaseVanAsselt$dose
dosemean <- runConsumerPhaseVanAsselt$dosemean
PrevExp <- runConsumerPhaseVanAsselt$PrevExp

# visualization
library(gridExtra)

resultsCPM <- cbind(round(runConsumerPhaseVanAsselt$dosemean,0), round(runConsumerPhaseVanAsselt$PrevExp*100,1))
rownames(resultsCPM) <- c("CPM VanAsselt")
colnames(resultsCPM) <- c("Mean dose", 'Prevalence of exposure [%]')

tableCPM <- matrix(round(runConsumerPhaseVanAsselt$dose,0))
rownames(tableCPM) <- rownames(tableCPM, do.NULL=FALSE, prefix="Value.")
colnames(tableCPM) <- c('Dose')
write.csv(tableCPM, file="doses-consumer-phase-VanAsselt.csv")

sortdose <-sort(log10(runConsumerPhaseVanAsselt$dose))
vsize <-length(sortdose)
pdose <- array(NA,vsize)
for (i in 1:vsize)
{pdose[i] <- (i-0.5)/vsize}

plot(sortdose,pdose, 
     xlab = "log Dose", ylab = "",
     main = "doses from meat contaminated at retail")
grid.table(resultsCPM)																																																																																																													