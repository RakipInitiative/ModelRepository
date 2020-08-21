#parameters
dose <- rep(1,100000)
Pprev <- 0.25
alphaGamma <- 0.38
betaGamma <- 0.51
rPillinf <- 0.76
etaPillinf <- 0.0092

#model

condPillinf <- 1 - (1+dose/etaPillinf)^(-rPillinf)
modDoseResponseMedianOutbreak <- function(dose, Pprev, condPillinf, alphaGamma, betaGamma){
  
  niter <- length(dose)
  
  Pill <- array(NA, niter)
  
  for (i in 1:niter) Pill[i] <- condPillinf[i]*round((1 - exp(lgamma(alphaGamma+betaGamma)+lgamma(betaGamma+dose[i])-lgamma(betaGamma)-lgamma(alphaGamma+betaGamma+dose[i]))),4)
  
  Pillmean <- mean(Pill)
  Qill <- Pillmean*Pprev
  PrevExp <- Pprev*(sum(dose>0)/niter)
  
  #return(list(Pill=Pill, Pillmean=Pillmean, Qill=Qill, PrevExp=PrevExp))
  return(list(Qill=Qill, PrevExp=PrevExp))
}

runDoseResponseMedianOutbreak <- modDoseResponseMedianOutbreak(dose, Pprev, condPillinf, alphaGamma, betaGamma)
#runDoseResponseMedianOutbreak <- modDoseResponseMedianOutbreak(dose=runConsumerPhaseNauta$dose, Pprev, condPillinf, alphaGamma, betaGamma)

Qill <-runDoseResponseMedianOutbreak$Qill
PrevExp <- runDoseResponseMedianOutbreak$PrevExp


#visualisation
library(gridExtra)

results <- cbind(round(runDoseResponseMedianOutbreak$PrevExp*100,1),
                 round(runDoseResponseMedianOutbreak$Qill*100,2))
rownames(results) <- c("MedianOutbreak DR")
colnames(results) <- c('Prevalence of Exposure [%]', 'Q_ill [%]')
grid.table(results)