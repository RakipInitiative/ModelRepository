
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
