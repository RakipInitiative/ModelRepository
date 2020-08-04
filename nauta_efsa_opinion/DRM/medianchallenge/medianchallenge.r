#parameters
dose <- rep(1,100000)
alphaGamma <- 0.44
Pprev <- 0.25
#condPillinf <- 0.33
betaGamma <- 0.51
rPillinf <- 0.06
etaPillinf <- 0.88

#model

condPillinf <- 1 - (1+dose/etaPillinf)^(-rPillinf)
modDoseResponseMedianChallenge <- function(dose, Pprev, condPillinf, alphaGamma, betaGamma){
  
  niter <- length(dose)
  
  Pill <- array(NA, niter)
  
  for (i in 1:niter) Pill[i] <- condPillinf[i]*round((1 - exp(lgamma(alphaGamma+betaGamma)+lgamma(betaGamma+dose[i])-lgamma(betaGamma)-lgamma(alphaGamma+betaGamma+dose[i]))),4)
  
  Pillmean <- mean(Pill)
  Qill <- Pillmean*Pprev
  PrevExp <- Pprev*(sum(dose>0)/niter)
  
  #return(list(Pill=Pill, Pillmean=Pillmean, Qill=Qill, PrevExp=PrevExp))
  return(list(Qill=Qill, PrevExp=PrevExp))
}

runDoseResponseMedianChallenge <- modDoseResponseMedianChallenge(dose, Pprev, condPillinf, alphaGamma, betaGamma)
#runDoseResponseMedianChallenge <- modDoseResponseMedianChallenge(dose=runConsumerPhaseNauta$dose, Pprev, condPillinf, alphaGamma, betaGamma)

Qill <-runDoseResponseMedianChallenge$Qill
PrevExp <- runDoseResponseMedianChallenge$PrevExp


#visualisation
library(gridExtra)

results <- cbind(round(runDoseResponseMedianChallenge$PrevExp*100,1),
                 round(runDoseResponseMedianChallenge$Qill*100,2))
rownames(results) <- c("MedianChallenge DR")
colnames(results) <- c('Prevalence of Exposure [%]', 'Q_ill [%]')
grid.table(results)